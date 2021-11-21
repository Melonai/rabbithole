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
                Err(anyhow!(
                    // Make a better error message
                    "Received unexpected token: '{:?}'.",
                    token.kind
                ))
            }
        } else {
            // Here too
            Err(anyhow!("Expected a token."))
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
