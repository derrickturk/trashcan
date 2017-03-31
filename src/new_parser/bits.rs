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

macro_rules! alt_one {
    ($alt:expr) => {
        if let (i, Ok(r)) = $alt? {
            return Ok((i, Ok(r)))
        }
    };

    ($alt:expr => $cl:expr) => {
        if let (i, Ok(r)) = $alt? {
            return Ok((i, Ok($cl(r))))
        }
    };
}

macro_rules! alt_inner {
    ($alt:expr) => {
        alt_one!($alt);
    };

    ($alt:expr => $cl:expr) => {
        alt_one!($alt => $cl);
    };

    ($alt:expr ; $($rest:tt)*) => {
        alt_one!($alt);
        alt_inner!($($rest)*);
    };

    ($alt:expr => $cl:expr ; $($rest:tt)*) => {
        alt_one!($alt => $cl);
        alt_inner!($($rest)*);
    };
}

#[macro_export]
macro_rules! alt {
    ($input:expr, $($alts:tt)*) => {
        {
            alt_inner!($($alts)*);
            // TODO: when this module name changes...
            Ok(($input, Err($crate::new_parser::ParseError::NoAltMatch)))
        }
    }
}

#[inline]
pub fn loc(input: &[u8]) -> ParseResult<usize> {
    ok!(input, input.as_ptr() as usize)
}

#[inline]
pub fn multispace(input: &[u8]) -> ParseResult<&[u8]> {
    for (i, b) in input.iter().enumerate() {
        match *b {
            b'\n' | b'\r' | b'\t' | b' ' => { },
            _ if i == 0 => return err!(input, ParseError::ExpectedWhiteSpace),
            _ => return ok!(&input[i..], &input[..i]),
        }
    }
    ok!(&[], input)
}

pub fn opt<'a, F, R>(input: &'a [u8], parser: F) -> ParseResult<Option<R>>
  where F: Fn(&'a [u8]) -> ParseResult<R> {
    let res = parser(input)?;
    match res {
        (i, Err(_)) => ok!(i, None),
        (i, Ok(r)) => ok!(i, Some(r)),
    }
}

pub fn keyword<'a>(input: &'a [u8], kw: &'static [u8])
  -> ParseResult<'a, &'a [u8]> {
    let (input, _) = opt(input, multispace)?;
    if input.starts_with(kw) {
        ok!(&input[kw.len()..], &input[..kw.len()])
    } else {
        err!(input, ParseError::ExpectedKeyword(kw))
    }
}
