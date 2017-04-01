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
            (i, Err(e)) => return Ok((i, Err(e))),
        }
    }
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
                Ok(($input, Err($crate::new_parser::ParseError::NoAltMatch))),
        }
    };

    ($input:expr, $alt:expr => $cl:expr) => {
        match $alt? {
            (i, Ok(r)) => Ok((i, Ok($cl(r)))),
            (_, Err(_)) =>
                Ok(($input, Err($crate::new_parser::ParseError::NoAltMatch))),
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
#[macro_export]
macro_rules! opt {
    ($maybe:expr) => {
        {
            // the compiler can't infer the type here unless we help it
            let r: $crate::new_parser::ParseResult<Option<_>> = match $maybe? {
                (i, Ok(r)) => Ok((i, Ok(Some(r)))),
                (i, Err(_)) => Ok((i, Ok(None))),
            };
            r
        }
    }
}

#[inline]
pub fn loc(input: &[u8]) -> ParseResult<usize> {
    ok!(input, input.as_ptr() as usize)
}

#[inline]
pub fn byte(input: &[u8], b: u8) -> ParseResult<u8> {
    if !input.is_empty() && input[0] == b {
        ok!(&input[1..], b)
    } else {
        err!(input, ParseError::ExpectedChar(b))
    }
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

#[inline]
pub fn digits(input: &[u8]) -> ParseResult<&[u8]> {
    for (i, b) in input.iter().enumerate() {
        match *b {
            b'0' | b'1' | b'2' | b'3' | b'4'
                | b'5' | b'6' | b'7' | b'8' | b'9' => { },
            _ if i == 0 => return err!(input, ParseError::ExpectedDigit),
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
    keyword_immediate(input, kw)
}

// no whitespace preceding
pub fn keyword_immediate<'a>(input: &'a [u8], kw: &'static [u8])
  -> ParseResult<'a, &'a [u8]> {
    if input.starts_with(kw) {
        ok!(&input[kw.len()..], &input[..kw.len()])
    } else {
        err!(input, ParseError::ExpectedKeyword(kw))
    }
}
