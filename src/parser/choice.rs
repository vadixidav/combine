//! Combinators which take one or more parsers and attempts to parse successfully with at least one
//! of them.
use error::FastResult::*;
use error::{ConsumedResult, ParseError, StreamError, Tracked};
use parser::ParseMode;
use stream::Resetable;
use {ErrorOffset, Parser, Stream, StreamOnce};

/// Takes a number of parsers and tries to apply them each in order.
/// Fails if all the parsers fails or if an applied parser consumes input before failing.
///
/// ```
/// # #[macro_use]
/// # extern crate combine;
/// # use combine::*;
/// # use combine::char::{digit, letter, string};
/// # use combine::stream::easy::Error;
/// # fn main() {
/// let mut parser = choice!(
///     many1(digit()),
///     string("let").map(|s| s.to_string()),
///     many1(letter()));
/// assert_eq!(parser.parse("let"), Ok(("let".to_string(), "")));
/// assert_eq!(parser.parse("123abc"), Ok(("123".to_string(), "abc")));
/// assert!(parser.parse(":123").is_err());
/// # }
/// ```
#[macro_export]
macro_rules! choice {
    ($first : expr) => {
        $first
    };
    ($first : expr, $($rest : expr),+) => {
        $first.or(choice!($($rest),+))
    }
}

#[macro_export]
#[doc(hidden)]
macro_rules! parse_mode_choice {
    () => {
        fn parse_partial(
            &mut self,
            input: &mut Self::Input,
            state: &mut Self::PartialState,
        ) -> ConsumedResult<Self::Output, Self::Input> {
            self.parse_mode_choice($crate::parser::PartialMode::default(), input, state)
        }

        fn parse_first(
            &mut self,
            input: &mut Self::Input,
            state: &mut Self::PartialState,
        ) -> ConsumedResult<Self::Output, Self::Input> {
            self.parse_mode_choice($crate::parser::FirstMode, input, state)
        }
    }
}

/// `ChoiceParser` represents a parser which may parse one of several different choices depending
/// on the input.
///
/// This is an internal trait used to overload the `choice` function.
pub trait ChoiceParser {
    type Input: Stream;
    type Output;
    type PartialState: Default;

    fn parse_first(
        &mut self,
        input: &mut Self::Input,
        state: &mut Self::PartialState,
    ) -> ConsumedResult<Self::Output, Self::Input>;

    fn parse_partial(
        &mut self,
        input: &mut Self::Input,
        state: &mut Self::PartialState,
    ) -> ConsumedResult<Self::Output, Self::Input>;

    fn parse_mode_choice<M>(
        &mut self,
        mode: M,
        input: &mut Self::Input,
        state: &mut Self::PartialState,
    ) -> ConsumedResult<Self::Output, Self::Input>
    where
        M: ParseMode,
        Self: Sized;

    fn add_error_choice(&mut self, error: &mut Tracked<<Self::Input as StreamOnce>::Error>);
}

impl<'a, P> ChoiceParser for &'a mut P
where
    P: ?Sized + ChoiceParser,
{
    type Input = P::Input;
    type Output = P::Output;
    type PartialState = P::PartialState;

    parse_mode_choice!();
    #[inline(always)]
    fn parse_mode_choice<M>(
        &mut self,
        mode: M,
        input: &mut Self::Input,
        state: &mut Self::PartialState,
    ) -> ConsumedResult<Self::Output, Self::Input>
    where
        M: ParseMode,
    {
        if mode.is_first() {
            (**self).parse_first(input, state)
        } else {
            (**self).parse_partial(input, state)
        }
    }

    fn add_error_choice(&mut self, error: &mut Tracked<<Self::Input as StreamOnce>::Error>) {
        (**self).add_error_choice(error)
    }
}

macro_rules! merge {
    ($head: ident) => {
        $head.error
    };
    ($head: ident $($tail: ident)+) => {
        $head.error.merge(merge!($($tail)+))
    };
}

macro_rules! do_choice {
    (
        $input: ident
        $before_position: ident
        $before: ident
        $partial_state: ident
        $state: ident
        ( )
        $($parser: ident $error: ident)+
    ) => { {
        let mut error = Tracked::from(merge!($($error)+));
        // If offset != 1 then the nested parser is a sequence of parsers where 1 or
        // more parsers returned `EmptyOk` before the parser finally failed with
        // `EmptyErr`. Since we lose the offsets of the nested parsers when we merge
        // the errors we must first extract the errors before we do the merge.
        // If the offset == 0 on the other hand (which should be the common case) then
        // we can delay the addition of the error since we know for certain that only
        // the first parser in the sequence were tried
        $(
            if $error.offset != ErrorOffset(1) {
                error.offset = $error.offset;
                $parser.add_error(&mut error);
                error.offset = ErrorOffset(0);
            }
        )+
        EmptyErr(error)
    } };
    (
        $input: ident
        $before_position: ident
        $before: ident
        $partial_state: ident
        $state: ident
        ( $head: ident $($tail: ident)* )
        $($all: ident)*
    ) => { {
        let parser = $head;
        let mut state = $head::PartialState::default();
        match parser.parse_mode(::parser::FirstMode, $input, &mut state) {
            ConsumedOk(x) => ConsumedOk(x),
            EmptyOk(x) => EmptyOk(x),
            ConsumedErr(err) => {
                // If we get `ConsumedErr` but the input is the same this is a partial parse we
                // cannot commit to so leave the state as `Empty` to retry all the parsers
                // on the next call to  `parse_partial`
                if $input.position() != $before_position {
                    *$state = self::$partial_state::$head(state);
                }
                ConsumedErr(err)
            }
            EmptyErr($head) => {
                $input.reset($before.clone());
                do_choice!(
                    $input
                    $before_position
                    $before
                    $partial_state
                    $state
                    ( $($tail)* )
                    $($all)*
                    parser
                    $head
                )
            }
        }
    } }
}

macro_rules! tuple_choice_parser {
    ($head: ident) => {
        tuple_choice_parser_inner!($head; $head);
    };
    ($head: ident $($id: ident)+) => {
        tuple_choice_parser_inner!($head; $head $($id)+);
        tuple_choice_parser!($($id)+);
    };
}

macro_rules! tuple_choice_parser_inner {
    ($partial_state: ident; $($id: ident)+) => {
        #[doc(hidden)]
        pub enum $partial_state<$($id),+> {
            Empty,
            $(
                $id($id),
            )+
        }

        impl<$($id),+> Default for self::$partial_state<$($id),+> {
            fn default() -> Self {
                self::$partial_state::Empty
            }
        }

        #[allow(non_snake_case)]
        impl<Input, Output $(,$id)+> ChoiceParser for ($($id,)+)
        where
            Input: Stream,
            $($id: Parser<Input = Input, Output = Output>),+
        {
            type Input = Input;
            type Output = Output;
            type PartialState = self::$partial_state<$($id::PartialState),+>;

            parse_mode_choice!();
            #[inline]
            fn parse_mode_choice<Mode>(
                &mut self,
                mode: Mode,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ConsumedResult<Self::Output, Self::Input>
            where
                Mode: ParseMode,
            {
                let ($(ref mut $id,)+) = *self;
                let empty = match *state {
                    self::$partial_state::Empty => true,
                    _ => false,
                };
                if mode.is_first() || empty {
                    let before_position = input.position();
                    let before = input.checkpoint();
                    do_choice!(input before_position before $partial_state state ( $($id)+ ) )
                } else {
                    match *state {
                        self::$partial_state::Empty => unreachable!(),
                        $(
                            self::$partial_state::$id(_) => {
                                let result = match *state {
                                    self::$partial_state::$id(ref mut state) => {
                                        $id.parse_mode(mode, input, state)
                                    }
                                    _ => unreachable!()
                                };
                                if result.is_ok() {
                                    *state = self::$partial_state::Empty;
                                }
                                result
                            }
                        )+
                    }
                }
            }

            fn add_error_choice(
                &mut self,
                error: &mut Tracked<<Self::Input as StreamOnce>::Error>
            ) {
                if error.offset != ErrorOffset(0) {
                    let ($(ref mut $id,)+) = *self;
                    // Reset the offset to 1 on every add so that we always (and only) takes the
                    // error of the first parser. If we don't do this the first parser will consume
                    // the offset to the detriment for all the other parsers.
                    $(
                        error.offset = ErrorOffset(1);
                        $id.add_error(error);
                    )+
                }
            }
        }
    }
}

tuple_choice_parser!(A B C D E F G H I J K L M N O P Q R S T U V X Y Z);

macro_rules! array_choice_parser {
    ($($t: tt)+) => {
        $(
        impl<P> ChoiceParser for [P; $t]
        where
            P: Parser,
        {
            type Input = P::Input;
            type Output = P::Output;
            type PartialState = <[P] as ChoiceParser>::PartialState;

            parse_mode_choice!();
            #[inline(always)]
            fn parse_mode_choice<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ConsumedResult<Self::Output, Self::Input>
            where
                M: ParseMode,
            {
                if mode.is_first() {
                    self[..].parse_first(input, state)
                } else {
                    self[..].parse_partial(input, state)
                }
            }
            fn add_error_choice(
                &mut self,
                error: &mut Tracked<<Self::Input as StreamOnce>::Error>
            ) {
                self[..].add_error_choice(error)
            }
        }
        )+
    };
}

array_choice_parser!(
    0 1 2 3 4 5 6 7 8 9
    10 11 12 13 14 15 16 17 18 19
    20 21 22 23 24 25 26 27 28 29
    30 31 32
    );

#[derive(Copy, Clone)]
pub struct Choice<P>(P);

impl<P> Parser for Choice<P>
where
    P: ChoiceParser,
{
    type Input = P::Input;
    type Output = P::Output;
    type PartialState = P::PartialState;

    parse_mode!();
    #[inline(always)]
    fn parse_mode_impl<M>(
        &mut self,
        mode: M,
        input: &mut Self::Input,
        state: &mut Self::PartialState,
    ) -> ConsumedResult<Self::Output, Self::Input>
    where
        M: ParseMode,
    {
        self.0.parse_mode_choice(mode, input, state)
    }

    fn add_error(&mut self, error: &mut Tracked<<Self::Input as StreamOnce>::Error>) {
        let before = error.offset.0;
        self.0.add_error_choice(error);
        error.offset.0 = before.saturating_sub(1);
    }
}

fn slice_parse_mode<I, P, M>(
    self_: &mut [P],
    mode: M,
    input: &mut P::Input,
    state: &mut (usize, P::PartialState),
) -> ConsumedResult<P::Output, P::Input>
where
    P: Parser<Input = I>,
    I: Stream,
    M: ParseMode,
{
    let mut prev_err = None;
    let mut last_parser_having_non_1_offset = 0;
    let before = input.checkpoint();

    let (ref mut index_state, ref mut child_state) = *state;
    if !mode.is_first() && *index_state != 0 {
        return self_[*index_state - 1]
            .parse_partial(input, child_state)
            .map(|x| {
                *index_state = 0;
                x
            });
    }

    for i in 0..self_.len() {
        input.reset(before.clone());

        match self_[i].parse_mode(mode, input, child_state) {
            consumed_err @ ConsumedErr(_) => {
                *index_state = i + 1;
                return consumed_err;
            }
            EmptyErr(err) => {
                prev_err = match prev_err {
                    None => Some(err),
                    Some(mut prev_err) => {
                        if prev_err.offset != ErrorOffset(1) {
                            // First add the errors of all the preceding parsers which did not
                            // have a sequence of parsers returning `EmptyOk` before failing
                            // with `EmptyErr`.
                            let offset = prev_err.offset;
                            for p in &mut self_[last_parser_having_non_1_offset..(i - 1)] {
                                prev_err.offset = ErrorOffset(1);
                                p.add_error(&mut prev_err);
                            }
                            // Then add the errors if the current parser
                            prev_err.offset = offset;
                            self_[i - 1].add_error(&mut prev_err);
                            last_parser_having_non_1_offset = i;
                        }
                        Some(Tracked {
                            error: prev_err.error.merge(err.error),
                            offset: err.offset,
                        })
                    }
                };
            }
            ok @ ConsumedOk(_) | ok @ EmptyOk(_) => {
                *index_state = 0;
                return ok;
            }
        }
    }
    EmptyErr(match prev_err {
        None => I::Error::from_error(
            input.position(),
            StreamError::message_static_message("parser choice is empty"),
        )
        .into(),
        Some(mut prev_err) => {
            if prev_err.offset != ErrorOffset(1) {
                let offset = prev_err.offset;
                let len = self_.len();
                for p in &mut self_[last_parser_having_non_1_offset..(len - 1)] {
                    prev_err.offset = ErrorOffset(1);
                    p.add_error(&mut prev_err);
                }
                prev_err.offset = offset;
                self_.last_mut().unwrap().add_error(&mut prev_err);
                prev_err.offset = ErrorOffset(0);
            }
            prev_err
        }
    })
}

impl<I, O, P> ChoiceParser for [P]
where
    I: Stream,
    P: Parser<Input = I, Output = O>,
{
    type Input = I;
    type Output = O;
    type PartialState = (usize, P::PartialState);

    #[inline(always)]
    fn parse_partial(
        &mut self,
        input: &mut Self::Input,
        state: &mut Self::PartialState,
    ) -> ConsumedResult<Self::Output, Self::Input> {
        slice_parse_mode(self, ::parser::PartialMode::default(), input, state)
    }

    #[inline(always)]
    fn parse_first(
        &mut self,
        input: &mut Self::Input,
        state: &mut Self::PartialState,
    ) -> ConsumedResult<Self::Output, Self::Input> {
        slice_parse_mode(self, ::parser::FirstMode, input, state)
    }

    #[inline(always)]
    fn parse_mode_choice<M>(
        &mut self,
        _mode: M,
        _input: &mut Self::Input,
        _state: &mut Self::PartialState,
    ) -> ConsumedResult<Self::Output, Self::Input>
    where
        M: ParseMode,
    {
        unreachable!()
    }

    fn add_error_choice(&mut self, error: &mut Tracked<<Self::Input as StreamOnce>::Error>) {
        if error.offset != ErrorOffset(0) {
            for p in self {
                error.offset = ErrorOffset(1);
                p.add_error(error);
            }
        }
    }
}

/// Takes a tuple, a slice or an array of parsers and tries to apply them each in order.
/// Fails if all the parsers fails or if an applied parser consumes input before failing.
///
/// ```
/// # extern crate combine;
/// # use combine::*;
/// # use combine::char::{digit, string};
/// # fn main() {
/// // `choice` is overloaded on tuples so that different types of parsers can be used
/// // (each parser must still have the same input and output types)
/// let mut parser = choice((
///     string("Apple").map(|s| s.to_string()),
///     many1(digit()),
///     string("Orange").map(|s| s.to_string()),
/// ));
/// assert_eq!(parser.parse("1234"), Ok(("1234".to_string(), "")));
/// assert_eq!(parser.parse("Orangexx"), Ok(("Orange".to_string(), "xx")));
/// assert!(parser.parse("Appl").is_err());
/// assert!(parser.parse("Pear").is_err());
///
/// // If arrays or slices are used then all parsers must have the same type
/// // (`string` in this case)
/// let mut parser2 = choice([string("one"), string("two"), string("three")]);
/// // Fails as the parser for "two" consumes the first 't' before failing
/// assert!(parser2.parse("three").is_err());
///
/// // Use 'attempt' to make failing parsers always act as if they have not consumed any input
/// let mut parser3 = choice([attempt(string("one")), attempt(string("two")), attempt(string("three"))]);
/// assert_eq!(parser3.parse("three"), Ok(("three", "")));
/// # }
/// ```
#[inline(always)]
pub fn choice<P>(ps: P) -> Choice<P>
where
    P: ChoiceParser,
{
    Choice(ps)
}

#[derive(Copy, Clone)]
pub struct Or<P1, P2>(Choice<(P1, P2)>)
where
    P1: Parser,
    P2: Parser;
impl<I, O, P1, P2> Parser for Or<P1, P2>
where
    I: Stream,
    P1: Parser<Input = I, Output = O>,
    P2: Parser<Input = I, Output = O>,
{
    type Input = I;
    type Output = O;
    type PartialState = <Choice<(P1, P2)> as Parser>::PartialState;

    parse_mode!();
    #[inline(always)]
    fn parse_mode_impl<M>(
        &mut self,
        mode: M,
        input: &mut Self::Input,
        state: &mut Self::PartialState,
    ) -> ConsumedResult<Self::Output, Self::Input>
    where
        M: ParseMode,
    {
        self.0.parse_mode(mode, input, state)
    }

    #[inline]
    fn add_error(&mut self, errors: &mut Tracked<<Self::Input as StreamOnce>::Error>) {
        if errors.offset != ErrorOffset(0) {
            self.0.add_error(errors);
        }
    }
}

/// Equivalent to [`p1.or(p2)`].
///
/// If you are looking to chain 3 or more parsers using `or` you may consider using the
/// [`choice!`] macro instead, which can be clearer and may result in a faster parser.
///
/// ```
/// # extern crate combine;
/// # use combine::*;
/// # use combine::parser::choice::or;
/// # use combine::parser::char::{digit, string};
/// # fn main() {
/// let mut parser = or(
///     string("let"),
///     or(digit().map(|_| "digit"), string("led")),
/// );
/// assert_eq!(parser.parse("let"), Ok(("let", "")));
/// assert_eq!(parser.parse("1"), Ok(("digit", "")));
/// assert!(parser.parse("led").is_err());
///
/// let mut parser2 = or(string("two"), string("three"));
/// // Fails as the parser for "two" consumes the first 't' before failing
/// assert!(parser2.parse("three").is_err());
///
/// // Use 'attempt' to make failing parsers always act as if they have not consumed any input
/// let mut parser3 = or(attempt(string("two")), attempt(string("three")));
/// assert_eq!(parser3.parse("three"), Ok(("three", "")));
/// # }
/// ```
///
/// [`choice!`]: ../macro.choice.html
/// [`p1.or(p2)`]: ../parser/trait.Parser.html#method.or
#[inline(always)]
pub fn or<P1, P2>(p1: P1, p2: P2) -> Or<P1, P2>
where
    P1: Parser,
    P2: Parser<Input = P1::Input, Output = P1::Output>,
{
    Or(choice((p1, p2)))
}

#[derive(Copy, Clone)]
pub struct Optional<P>(P);
impl<P> Parser for Optional<P>
where
    P: Parser,
{
    type Input = P::Input;
    type Output = Option<P::Output>;
    type PartialState = P::PartialState;

    parse_mode!();
    #[inline]
    fn parse_mode_impl<M>(
        &mut self,
        mode: M,
        input: &mut Self::Input,
        state: &mut Self::PartialState,
    ) -> ConsumedResult<Self::Output, Self::Input>
    where
        M: ParseMode,
    {
        let before = input.checkpoint();
        match self.0.parse_mode(mode, input, state) {
            EmptyOk(x) => EmptyOk(Some(x)),
            ConsumedOk(x) => ConsumedOk(Some(x)),
            ConsumedErr(err) => ConsumedErr(err),
            EmptyErr(_) => {
                input.reset(before);
                EmptyOk(None)
            }
        }
    }

    forward_parser!(add_error parser_count, 0);
}

/// Parses `parser` and outputs `Some(value)` if it succeeds, `None` if it fails without
/// consuming any input. Fails if `parser` fails after having consumed some input.
///
/// ```
/// # extern crate combine;
/// # use combine::*;
/// # use combine::parser::char::string;
/// # fn main() {
/// let mut parser = optional(string("hello"));
/// assert_eq!(parser.parse("hello"), Ok((Some("hello"), "")));
/// assert_eq!(parser.parse("world"), Ok((None, "world")));
/// assert!(parser.parse("heya").is_err());
/// # }
/// ```
#[inline(always)]
pub fn optional<P>(parser: P) -> Optional<P>
where
    P: Parser,
{
    Optional(parser)
}

#[macro_export]
macro_rules! dispatch {
    ($( $($pattern: pat)|* => $parser: expr),*) => {
        $crate::parser::function::mode_parser(move |mode, input, (state, ())| {
            if $crate::parser::ParseMode::is_first(mode) {
                *state = match $crate::stream::uncons(input) {
                    $crate::error::FastResult::ConsumedOk(x) |
                    $crate::error::FastResult::EmptyOk(x) => x,
                    $crate::error::FastResult::ConsumedErr(err) => {
                        return $crate::error::FastResult::ConsumedErr(err)
                    }
                    $crate::error::FastResult::EmptyErr(err) => {
                        return $crate::error::FastResult::EmptyErr(err)
                    }
                }
            };
            $crate::error::FastResult::ConsumedOk(match *state { $(
                $($pattern)|* => match $parser.parse_mode(mode, input, &mut Default::default()) { // FIXME Propagate state
                    $crate::error::FastResult::ConsumedOk(x) |
                    $crate::error::FastResult::EmptyOk(x) => x,
                    $crate::error::FastResult::ConsumedErr(err) => {
                        return $crate::error::FastResult::ConsumedErr(err)
                    }
                    $crate::error::FastResult::EmptyErr(err) => {
                        return $crate::error::FastResult::ConsumedErr(err.error)
                    }
                }
            )*})
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::{
        char::char,
        error::unexpected_any,
        item::{any, eof},
    };

    #[test]
    fn choice_single_parser() {
        assert!(choice((any(),),).easy_parse("a").is_ok());
    }

    #[test]
    fn dispatch() {
        let mut parser = dispatch!(
            'a' => eof().map(|_| 'a'),
            'b' => char('b'),
            x => unexpected_any(x)
        );
        assert_eq!(parser.parse("a"), Ok(('a', "")));
        assert_eq!(parser.parse("bb"), Ok(('b', "")));
    }
}
