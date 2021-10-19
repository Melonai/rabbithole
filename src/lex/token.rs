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
    // Basic math operators
    OpPlus,
    OpMinus,
    OpStar,
    OpSlash,
    // Boolean operators
    OpEq,
    OpNot,
    OpNeq,
    OpLt,
    OpGt,
    OpLte,
    OpGte,

    // Statement symbols
    Dot,
    Comma,
    Colon,
    SemiColon,
    Assign,
    ConstAssign,
    Arrow,

    // Groupings
    GroupOpen,
    GroupClose,
    BlockOpen,
    BlockClose,

    // Literals
    Int(u32),
    Float(f32),
    Str(String),
    Identifer(String),

    // Keywords
    KeywordFn,
    KeywordForm,
    KeywordType,
    KeywordSelf,

    Unknown(char),
    Eof,
}
