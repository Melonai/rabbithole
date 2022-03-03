#[macro_export]
macro_rules! check {
    ($self:ident, $($kind:pat_param)|+) => {
        if let Some(Token {kind: $( $kind )|+, ..}) = $self.tokens.peek() {
            true
        } else {
            false
        }
    };
}

#[macro_export]
macro_rules! consume {
    ($self:ident, $($kind:pat_param)|+) => {
        if let Some(token) = $self.tokens.next() {
            if let Token {kind: $( $kind )|+, ..} = token {
                Ok(token)
            } else {
                Err(ParserError::new(ParserErrorLocation::Specific(token.location), ParserErrorKind::UnexpectedToken {
                    received: token.kind,
                    expected: merge_token_names!($($kind),+),
                }))
            }
        } else {
            Err(ParserError::new(ParserErrorLocation::Eof, ParserErrorKind::UnexpectedEof {
                expected: merge_token_names!($($kind),+),
            }))
        }
    };
}

#[macro_export]
macro_rules! consume_if {
    ($self:ident, $($kind:pat_param)|+) => {
        if let Some(Token {kind: $( $kind )|+, ..}) = $self.tokens.peek() {
            Some($self.tokens.next().unwrap())
        } else {
            None
        }
    };
}

#[macro_export]
macro_rules! inner {
    ($token:expr, $kind:path ) => {
        match $token.kind {
            $kind(inner) => inner,
            _ => panic!("Tried getting inner content of incorrect kind."),
        }
    };
}

// TODO: Better names for the tokens.
#[macro_export]
macro_rules! merge_token_names {
    ($($kind:pat_param),+) => {
        vec![$( stringify!($kind) ),+].join(", ")
    };
}
