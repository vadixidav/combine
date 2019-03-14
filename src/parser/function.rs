//! Parsers constructor from regular functions

use lib::marker::PhantomData;

use error::{ConsumedResult, ParseResult};
use parser::{ParseMode, PartialMode};
use stream::Stream;
use Parser;

impl<'a, I: Stream, O> Parser for FnMut(&mut I) -> ParseResult<O, I> + 'a {
    type Input = I;
    type Output = O;
    type PartialState = ();

    #[inline]
    fn parse_lazy(&mut self, input: &mut Self::Input) -> ConsumedResult<O, I> {
        self(input).into()
    }
}

#[derive(Copy, Clone)]
pub struct FnParser<I, F>(F, PhantomData<fn(I) -> I>);

/// Wraps a function, turning it into a parser.
///
/// Mainly needed to turn closures into parsers as function types can be casted to function pointers
/// to make them usable as a parser.
///
/// ```
/// extern crate combine;
/// # use combine::*;
/// # use combine::parser::char::digit;
/// # use combine::error::{Consumed, StreamError};
/// # use combine::stream::easy;
/// # fn main() {
/// let mut even_digit = parser(|input| {
///     // Help type inference out
///     let _: &mut easy::Stream<&str> = input;
///     let position = input.position();
///     let (char_digit, consumed) = try!(digit().parse_stream(input));
///     let d = (char_digit as i32) - ('0' as i32);
///     if d % 2 == 0 {
///         Ok((d, consumed))
///     }
///     else {
///         //Return an empty error since we only tested the first token of the stream
///         let errors = easy::Errors::new(
///             position,
///             StreamError::expected(From::from("even number"))
///         );
///         Err(Consumed::Empty(errors.into()))
///     }
/// });
/// let result = even_digit
///     .easy_parse("8")
///     .map(|x| x.0);
/// assert_eq!(result, Ok(8));
/// # }
/// ```
#[inline(always)]
pub fn parser<I, O, F>(f: F) -> FnParser<I, F>
where
    I: Stream,
    F: FnMut(&mut I) -> ParseResult<O, I>,
{
    FnParser(f, PhantomData)
}

impl<I, O, F> Parser for FnParser<I, F>
where
    I: Stream,
    F: FnMut(&mut I) -> ParseResult<O, I>,
{
    type Input = I;
    type Output = O;
    type PartialState = ();

    #[inline]
    fn parse_lazy(&mut self, input: &mut Self::Input) -> ConsumedResult<O, I> {
        (self.0)(input).into()
    }
}

impl<I, O> Parser for fn(&mut I) -> ParseResult<O, I>
where
    I: Stream,
{
    type Input = I;
    type Output = O;
    type PartialState = ();

    #[inline]
    fn parse_lazy(&mut self, input: &mut Self::Input) -> ConsumedResult<O, I> {
        self(input).into()
    }
}

#[derive(Copy)]
pub struct EnvParser<E, I, T>
where
    I: Stream,
{
    env: E,
    parser: fn(E, &mut I) -> ParseResult<T, I>,
}

impl<E, I, T> Clone for EnvParser<E, I, T>
where
    I: Stream,
    E: Clone,
{
    fn clone(&self) -> Self {
        EnvParser {
            env: self.env.clone(),
            parser: self.parser,
        }
    }
}

impl<E, I, O> Parser for EnvParser<E, I, O>
where
    E: Clone,
    I: Stream,
{
    type Input = I;
    type Output = O;
    type PartialState = ();

    #[inline]
    fn parse_lazy(&mut self, input: &mut Self::Input) -> ConsumedResult<O, I> {
        (self.parser)(self.env.clone(), input).into()
    }
}

/// Constructs a parser out of an environment and a function which needs the given environment to
/// do the parsing. This is commonly useful to allow multiple parsers to share some environment
/// while still allowing the parsers to be written in separate functions.
///
/// ```
/// # extern crate combine;
/// # use std::collections::HashMap;
/// # use combine::*;
/// # use combine::char::letter;
/// # fn main() {
/// struct Interner(HashMap<String, u32>);
/// impl Interner {
///     fn string<I>(&self, input: &mut I) -> ParseResult<u32, I>
///         where I: Stream<Item=char>,
///               I::Error: ParseError<I::Item, I::Range, I::Position>,
///     {
///         many(letter())
///             .map(|s: String| self.0.get(&s).cloned().unwrap_or(0))
///             .parse_stream(input)
///     }
/// }
///
/// let mut map = HashMap::new();
/// map.insert("hello".into(), 1);
/// map.insert("test".into(), 2);
///
/// let env = Interner(map);
/// let mut parser = env_parser(&env, Interner::string);
///
/// let result = parser.parse("hello");
/// assert_eq!(result, Ok((1, "")));
///
/// let result = parser.parse("world");
/// assert_eq!(result, Ok((0, "")));
/// # }
/// ```
#[inline(always)]
pub fn env_parser<E, I, O>(env: E, parser: fn(E, &mut I) -> ParseResult<O, I>) -> EnvParser<E, I, O>
where
    E: Clone,
    I: Stream,
{
    EnvParser {
        env: env,
        parser: parser,
    }
}

#[derive(Copy, Clone)]
pub struct ModeFnParser<I, S, F>(F, PhantomData<fn(&mut I, &mut S)>);

/// Wraps a function, turning it into a parser.
///
/// Mainly needed to turn closures into parsers as function types can be casted to function pointers
/// to make them usable as a parser.
///
/// ```
/// extern crate combine;
/// # use combine::*;
/// # use combine::parser::char::digit;
/// # use combine::error::{Consumed, StreamError};
/// # use combine::stream::easy;
/// # fn main() {
/// let mut even_digit = parser(|input| {
///     // Help type inference out
///     let _: &mut easy::Stream<&str> = input;
///     let position = input.position();
///     let (char_digit, consumed) = try!(digit().parse_stream(input));
///     let d = (char_digit as i32) - ('0' as i32);
///     if d % 2 == 0 {
///         Ok((d, consumed))
///     }
///     else {
///         //Return an empty error since we only tested the first token of the stream
///         let errors = easy::Errors::new(
///             position,
///             StreamError::expected(From::from("even number"))
///         );
///         Err(Consumed::Empty(errors.into()))
///     }
/// });
/// let result = even_digit
///     .easy_parse("8")
///     .map(|x| x.0);
/// assert_eq!(result, Ok(8));
/// # }
/// ```
#[inline(always)]
pub fn mode_parser<I, S, O, F>(f: F) -> ModeFnParser<I, S, F>
where
    I: Stream,
    F: FnMut(PartialMode, &mut I, &mut S) -> ConsumedResult<O, I>,
{
    ModeFnParser(f, PhantomData)
}

impl<I, S, O, F> Parser for ModeFnParser<I, S, F>
where
    I: Stream,
    S: Default,
    F: FnMut(PartialMode, &mut I, &mut S) -> ConsumedResult<O, I>,
{
    type Input = I;
    type Output = O;
    type PartialState = S;

    parse_mode!();
    #[inline]
    fn parse_mode<M>(
        &mut self,
        mode: M,
        input: &mut Self::Input,
        state: &mut Self::PartialState,
    ) -> ConsumedResult<O, I>
    where
        M: ParseMode,
    {
        (self.0)(
            PartialMode {
                first: mode.is_first(),
            },
            input,
            state,
        )
    }
}
