#[derive(Clone, Copy, Debug)]
pub struct Location {
    pub col: usize,
    pub row: usize,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub location: Location,
    pub variant: TokenVariant,
}

#[derive(Clone, Debug)]
pub enum TokenVariant {
    OpPlus,
    OpMinus,
    OpStar,
    OpSlash,
    OpNot,

    GroupOpen,
    GroupClose,

    Int(u32),
    Float(f32),

    Unknown(char),
    Eof,
}
