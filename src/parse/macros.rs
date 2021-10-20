#[macro_export]
macro_rules! check {
    ($self:ident, $($variant:pat_param)|+) => {
        if let Some(token) = $self.tokens.peek() {
            if let Token {variant: $( $variant )|+, ..} = token {
                true
            } else {
                false
            }
        } else {
            false
        }
    };
}

#[macro_export]
macro_rules! consume {
    ($self:ident, $($variant:pat_param)|+) => {
        if let Some(token) = $self.tokens.next() {
            if let Token {variant: $( $variant )|+, ..} = token {
                Ok(token)
            } else {
                Err(anyhow!(
                    // Make a better error message
                    "Received unexpected token: '{:?}'.",
                    token.variant
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
    ($self:ident, $($variant:pat_param)|+) => {
        if let Some(token) = $self.tokens.peek() {
            if let Token {variant: $( $variant )|+, ..} = token {
                Some($self.tokens.next().unwrap())
            } else {
                None
            }
        } else {
            None
        }
    };
}

#[macro_export]
macro_rules! inner {
    ($token:expr, $variant:path ) => {
        match $token.variant {
            $variant(inner) => inner,
            _ => panic!("Tried getting inner content of incorrect variant.")
        }
    };
}