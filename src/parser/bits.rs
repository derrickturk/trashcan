use super::{ParseError, ParseResult};

#[macro_export]
macro_rules! ok {
    ($i:expr, $e:expr) => { Ok(($i, Ok($e))) }
}

#[macro_export]
macro_rules! err {
    ($i:expr, $err:expr) => { Ok(($i, Err($err))) }
}

#[macro_export]
macro_rules! cut {
    ($i:expr, $err:expr) => { Err(($i, $err)) }
}

#[macro_export]
macro_rules! require {
    ($e:expr) => {
        match $e? {
            (i, Ok(r)) => (i, r),
            (i, Err(e)) => return err!(i, e),
        }
    }

    // TODO: a form that lets you pass in the "fail-back-to" input?
}

// like require, but cuts in case of error
#[macro_export]
macro_rules! require_or_cut {
    ($e:expr) => {
        match $e? {
            (i, Ok(r)) => (i, r),
            (i, Err(e)) => return cut!(i, e),
        }
    };

    ($e:expr => $err:expr) => {
        match $e? {
            (i, Ok(r)) => (i, r),
            (i, Err(_)) => return cut!(i, $err),
        }
    };

    // TODO: a form that lets you pass in the "fail-back-to" input?
}

// promote an error to a cut from a parsing-expression
#[macro_export]
macro_rules! cut_if_err {
    ($e:expr) => {
        match $e {
            Ok((i, Err(e))) => Err((i, e)),
            other => other,
        }
    };

    ($e:expr => $err:expr) => {
        match $e {
            Ok((i, Err(_))) => Err((i, $err)),
            other => other,
        }
    };
}

// works on closures/functions which are Fn(&[u8]) -> ParseResult<T>
#[macro_export]
macro_rules! chain {
    ($input:expr, $parser:expr) => {
        $parser($input)
    };

    ($input:expr, $parser:expr => $($rest:tt)*) => {
        match $parser($input)? {
            (i, Ok(_)) => chain!(i, $($rest)*),
            (i, Err(e)) => err!($input, e),
        }
    };

    // for internal use only
    ($orig:expr; $input:expr, $parser:expr) => {
        $parser($input)
    };

    // for internal use only
    ($orig:expr; $input:expr, $parser:expr => $($rest:tt)*) => {
        match $parser($input)? {
            (i, Ok(_)) => chain!($orig; i, $($rest)*),
            (i, Err(e)) => err!($orig, e),
        }
    };
}

#[macro_export]
macro_rules! alt {
    ($input:expr, $alt:expr) => {
        match $alt? {
            (i, Ok(r)) => Ok((i, Ok(r))),
            (_, Err(_)) =>
                Ok(($input, Err($crate::parser::ParseError::NoAltMatch))),
        }
    };

    ($input:expr, $alt:expr => $cl:expr) => {
        match $alt? {
            (i, Ok(r)) => Ok((i, Ok($cl(r)))),
            (_, Err(_)) =>
                Ok(($input, Err($crate::parser::ParseError::NoAltMatch))),
        }
    };

    ($input:expr, $alt:expr ; $($rest:tt)*) => {
        match $alt? {
            (i, Ok(r)) => Ok((i, Ok(r))),
            (_, Err(_)) => alt!($input, $($rest)*),
        }
    };

    ($input:expr, $alt:expr => $cl:expr ; $($rest:tt)*) => {
        match $alt? {
            (i, Ok(r)) => Ok((i, Ok($cl(r)))),
            (_, Err(_)) => alt!($input, $($rest)*),
        }
    };
}

// for expressions (not functions)
// TODO: I think this can go horribly wrong if an error bumps the input
#[macro_export]
macro_rules! opt {
    ($maybe:expr) => {
        {
            // the compiler can't infer the type here unless we help it
            let r: $crate::parser::ParseResult<Option<_>> = match $maybe? {
                (i, Ok(r)) => Ok((i, Ok(Some(r)))),
                (i, Err(_)) => Ok((i, Ok(None))),
            };
            r
        }
    }

    // TODO: a form that lets you pass in the "fail-back-to" input?
}

#[inline]
pub fn pos(input: &[u8]) -> ParseResult<usize> {
    ok!(input, input.as_ptr() as usize)
}

#[inline]
pub fn byte(input: &[u8], b: u8) -> ParseResult<u8> {
    if !input.is_empty() && input[0] == b {
        ok!(&input[1..], b)
    } else {
        err!(input, ParseError::ExpectedByte(b))
    }
}

#[inline]
pub fn multispace(input: &[u8]) -> ParseResult<&[u8]> {
    for (i, b) in input.iter().enumerate() {
        match *b {
            b'\n' | b'\r' | b'\t' | b' ' => { },
            _ if i == 0 => return err!(input, ParseError::ExpectedWhiteSpace),
            _ => {
                let (parsed, rest) = input.split_at(i);
                return ok!(rest, parsed);
            }
        }
    }
    ok!(&input[input.len()..], input)
}

#[inline]
pub fn digits(input: &[u8]) -> ParseResult<&[u8]> {
    for (i, b) in input.iter().enumerate() {
        match *b {
            b'0' | b'1' | b'2' | b'3' | b'4'
                | b'5' | b'6' | b'7' | b'8' | b'9' => { },
            _ if i == 0 => return err!(input, ParseError::ExpectedDigit),
            _ => {
                let (parsed, rest) = input.split_at(i);
                return ok!(rest, parsed);
            }
        }
    }
    ok!(&input[input.len()..], input)
}

#[inline]
pub fn ascii_letters(input: &[u8]) -> ParseResult<&[u8]> {
    for (i, b) in input.iter().enumerate() {
        if (*b < b'A' || *b > b'Z') && (*b < b'a' || *b > b'z') {
            if i == 0 {
                return err!(input, ParseError::ExpectedAsciiLetter);
            } else {
                let (parsed, rest) = input.split_at(i);
                return ok!(rest, parsed);
            }
        }
    }
    ok!(&input[input.len()..], input)
}

#[inline]
pub fn bytes_in<'a>(input: &'a [u8], set: &[u8]) -> ParseResult<'a, &'a [u8]> {
    for (i, b) in input.iter().enumerate() {
        if !set.contains(b) {
            if i == 0 {
                return err!(input, ParseError::ExpectedInSet);
            } else {
                let (parsed, rest) = input.split_at(i);
                return ok!(rest, parsed);
            }
        }
    }
    ok!(&input[input.len()..], input)
}

#[inline]
pub fn opt<'a, F, R>(input: &'a [u8], parser: F) -> ParseResult<Option<R>>
  where F: Fn(&'a [u8]) -> ParseResult<R> {
    let res = parser(input)?;
    match res {
        (i, Err(_)) => ok!(input, None),
        (i, Ok(r)) => ok!(i, Some(r)),
    }
}

#[inline]
pub fn keyword<'a>(input: &'a [u8], kw: &'static [u8])
  -> ParseResult<'a, &'a [u8]> {
    let (input, _) = opt(input, multispace)?;
    keyword_immediate(input, kw)
}

// no whitespace preceding
#[inline]
pub fn keyword_immediate<'a>(input: &'a [u8], kw: &'static [u8])
  -> ParseResult<'a, &'a [u8]> {
    if input.starts_with(kw) {
        ok!(&input[kw.len()..], &input[..kw.len()])
    } else {
        err!(input, ParseError::ExpectedKeyword(kw))
    }
}

#[inline]
pub fn many<'a, F, R>(input: &'a [u8], parser: F) -> ParseResult<Vec<R>>
  where F: Fn(&'a [u8]) -> ParseResult<R> {
    let mut results = Vec::new();
    let mut i = input;
    let mut res;

    loop {
        // odd, but necessary to not loop forever
        match parser(i)? {
            (new_i, new_res) => {
                i = new_i;
                res = new_res;
            }
        };

        match res {
            Ok(res) => results.push(res),
            Err(_) => return ok!(i, results),
        }

        if i.is_empty() {
            return ok!(i, results)
        }
    }
}

#[inline]
pub fn at_least_one<'a, F, R>(input: &'a [u8], parser: F) -> ParseResult<Vec<R>>
  where F: Fn(&'a [u8]) -> ParseResult<R> {
    let (i, res) = require!(parser(input));
    let mut results = vec![res];

    let (i, mut rest) = require!(many(i, parser));
    results.append(&mut rest);

    ok!(i, results)
}

#[inline]
pub fn delimited<'a, F, R, D, _R>(input: &'a [u8], parser: F, delim_parser: D)
  -> ParseResult<Vec<R>>
  where F: Fn(&'a [u8]) -> ParseResult<R>,
        D: Fn(&'a [u8]) -> ParseResult<_R> {
    let (i, first) = require!(opt!(parser(input)));
    let mut result = match first {
        None => return ok!(input, Vec::new()),
        Some(first) => vec![first],
    };

    let next_parser = |i| {
        let (i, _) = require!(delim_parser(i));
        let (i, res) = require!(parser(i));
        ok!(i, res)
    };

    let (i, mut rest) = require!(many(i, next_parser));
    result.append(&mut rest);

    ok!(i, result)
}

#[inline]
pub fn delimited_at_least_one<'a, F, R, D, _R>(input: &'a [u8], parser: F,
  delim_parser: D) -> ParseResult<Vec<R>>
  where F: Fn(&'a [u8]) -> ParseResult<R>,
        D: Fn(&'a [u8]) -> ParseResult<_R> {
    let (i, first) = require!(parser(input));
    let mut result = vec![first];

    let next_parser = |i| {
        let (i, _) = require!(delim_parser(i));
        let (i, res) = require!(parser(i));
        ok!(i, res)
    };

    let (i, mut rest) = require!(many(i, next_parser));
    result.append(&mut rest);

    ok!(i, result)
}
