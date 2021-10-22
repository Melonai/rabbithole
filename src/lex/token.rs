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

#[derive(Clone, Debug, PartialEq)]
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

    Ident(String),

    // Keywords
    KeywordFn,
    KeywordIf,
    KeywordElif,
    KeywordElse,
    KeywordForm,
    KeywordType,
    KeywordTrue,
    KeywordFalse,
    KeywordSelf,
    KeywordReturn,
    KeywordPrint,

    Unknown(char),
    Eof,
}
