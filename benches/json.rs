// `impl Trait` is not required for this parser but we use to to show that it can be used to
// significantly simplify things
#![feature(conservative_impl_trait)]

#[macro_use]
extern crate bencher;

#[macro_use]
extern crate combine;

use std::collections::HashMap;
use std::io::Read;
use std::fs::File;
use std::path::Path;
use std::hash::Hash;
use bencher::{black_box, Bencher};

use combine::{Parser, RangeStream, Stream, StreamOnce};
use combine::error::{Consumed, ParseError};

use combine::parser::char::{char, digit, spaces, string};
use combine::parser::item::{any, one_of, satisfy_map};
use combine::parser::sequence::between;
use combine::parser::repeat::{sep_by, skip_many, skip_many1};
use combine::parser::choice::{choice, optional};
use combine::parser::function::parser;
use combine::parser::range;

use combine::stream::state::State;

#[derive(PartialEq, Debug)]
enum Value<S>
where
    S: Eq + Hash,
{
    Number(f64),
    String(S),
    Bool(bool),
    Null,
    Object(HashMap<S, Value<S>>),
    Array(Vec<Value<S>>),
}

fn lex<P>(p: P) -> impl Parser<Input = P::Input, Output = P::Output>
where
    P: Parser,
    P::Input: Stream<Item = char>,
    <P::Input as StreamOnce>::Error: ParseError<
        <P::Input as StreamOnce>::Item,
        <P::Input as StreamOnce>::Range,
        <P::Input as StreamOnce>::Position,
    >,
{
    p.skip(spaces())
}

fn number<'a, I>() -> impl Parser<Input = I, Output = f64>
where
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    lex(range::recognize((
        optional(char('-')),
        char('0').or((
            skip_many1(digit()),
            optional((char('.'), skip_many(digit()))),
        ).map(|_| '0')),
        optional((
            (one_of("eE".chars()), optional(one_of("+-".chars()))),
            skip_many1(digit()),
        )),
    ))).map(|s: &'a str| s.parse().unwrap())
        .expected("number")
}

fn json_char<I>() -> impl Parser<Input = I, Output = char>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    parser(|input: &mut I| {
        let (c, consumed) = try!(any().parse_lazy(input).into());
        let mut back_slash_char = satisfy_map(|c| {
            Some(match c {
                '"' => '"',
                '\\' => '\\',
                '/' => '/',
                'b' => '\u{0008}',
                'f' => '\u{000c}',
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                _ => return None,
            })
        });
        match c {
            '\\' => consumed.combine(|_| back_slash_char.parse_stream(input)),
            '"' => Err(Consumed::Empty(I::Error::empty(input.position()).into())),
            _ => Ok((c, consumed)),
        }
    })
}

fn json_string<'a, I>() -> impl Parser<Input = I, Output = &'a str>
where
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    between(
        char('"'),
        lex(char('"')),
        range::recognize(skip_many(json_char())),
    ).expected("string")
}

fn object<'a, I>() -> impl Parser<Input = I, Output = Value<&'a str>>
where
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let field = (json_string(), lex(char(':')), json_value_()).map(|t| (t.0, t.2));
    let fields = sep_by(field, lex(char(',')));
    between(lex(char('{')), lex(char('}')), fields)
        .map(Value::Object)
        .expected("object")
}

#[inline(always)]
fn json_value<'a, I>() -> impl Parser<Input = I, Output = Value<&'a str>>
where
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    spaces().with(json_value_())
}

// We need to use `parser!` to break the recursive use of `value` to prevent the returned parser
// from containing itself
parser!{
    #[inline(always)]
    fn json_value_['a, I]()(I) -> Value<I::Range>
        where [ I: RangeStream<Item = char, Range = &'a str> ]
    {
        let array = between(
            lex(char('[')),
            lex(char(']')),
            sep_by(json_value_(), lex(char(','))),
        ).map(Value::Array);

        choice((
            json_string().map(Value::String),
            object(),
            array,
            number().map(Value::Number),
            lex(string("false").map(|_| Value::Bool(false))),
            lex(string("true").map(|_| Value::Bool(true))),
            lex(string("null").map(|_| Value::Null)),
        ))
    }
}

#[test]
fn json_test() {
    use self::Value::*;
    let input = r#" {
    "array": [1, ""],
    "object": {},
    "number": 3.14,
    "small_number": 0.59,
    "int": -100,
    "exp": -1e2,
    "exp_neg": 23e-2,
    "true": true,
    "false"  : false,
    "null" : null
}"#;
    let result = json_value().easy_parse(input);
    let expected = Object(
        vec![
            ("array", Array(vec![Number(1.0), String("".to_string())])),
            ("object", Object(HashMap::new())),
            ("number", Number(3.14)),
            ("small_number", Number(0.59)),
            ("int", Number(-100.)),
            ("exp", Number(-1e2)),
            ("exp_neg", Number(23E-2)),
            ("true", Bool(true)),
            ("false", Bool(false)),
            ("null", Null),
        ].into_iter()
            .map(|(k, v)| (k.to_string(), v))
            .collect(),
    );
    match result {
        Ok(result) => assert_eq!(result, (expected, "")),
        Err(e) => {
            println!("{}", e);
            assert!(false);
        }
    }
}

fn test_data() -> String {
    let mut data = String::new();
    File::open(&Path::new(&"benches/data.json"))
        .and_then(|mut file| file.read_to_string(&mut data))
        .unwrap();
    data
}

fn bench_json(bencher: &mut Bencher) {
    let data = test_data();
    let mut parser = json_value();
    match parser.easy_parse(State::new(&data[..])) {
        Ok((Value::Array(_), _)) => (),
        Ok(_) => assert!(false),
        Err(err) => {
            println!("{}", err);
            assert!(false);
        }
    }
    bencher.iter(|| {
        let result = parser.easy_parse(State::new(&data[..]));
        black_box(result)
    });
}

fn bench_json_core_error(bencher: &mut Bencher) {
    let data = test_data();
    let mut parser = json_value();
    match parser.parse(State::new(&data[..])) {
        Ok((Value::Array(_), _)) => (),
        Ok(_) => assert!(false),
        Err(err) => {
            println!("{}", err);
            assert!(false);
        }
    }
    bencher.iter(|| {
        let result = parser.parse(State::new(&data[..]));
        black_box(result)
    });
}

fn bench_json_core_error_no_position(bencher: &mut Bencher) {
    let data = test_data();
    let mut parser = json_value();
    match parser.parse(&data[..]) {
        Ok((Value::Array(_), _)) => (),
        Ok(_) => assert!(false),
        Err(err) => {
            println!("{}", err);
            assert!(false);
        }
    }
    bencher.iter(|| {
        let result = parser.parse(&data[..]);
        black_box(result)
    });
}

/*
fn bench_buffered_json(bencher: &mut Bencher) {
    let data = test_data();
    bencher.iter(|| {
        let buffer = BufferedStream::new(State::new(IteratorStream::new(data.chars())), 1);
        let mut parser = json_value();
        match parser.easy_parse(State::with_positioner(buffer, SourcePosition::default())) {
            Ok((Value::Array(v), _)) => {
                black_box(v);
            }
            Ok(_) => assert!(false),
            Err(err) => {
                println!("{}", err);
                assert!(false);
            }
        }
    });
}

*/

benchmark_group!(
    json,
    bench_json,
    bench_json_core_error,
    bench_json_core_error_no_position // bench_buffered_json
);
benchmark_main!(json);
