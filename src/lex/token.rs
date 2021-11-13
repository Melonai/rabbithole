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
    // Short-circuting boolean operators
    OpAnd,
    OpOr,

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
    ArrayOpen,
    ArrayClose,

    // Literals
    Int(u32),
    Float(f32),
    
    // String Literal Tokens
    Str(String),
    // StrOpen and StrClose are necessary for string embeds.
    // A normal string looks like [StrOpen Str("Hello!") StrClose] in a token stream while a
    // string with an embed would look something like [StrOpen Str("Hello") StrEmbed("world") Str("!") S StrClose].
    StrOpen,
    StrClose,
    // The string embed has to be lexed by a *seperate* lexer.
    StrEmbed(String),
    
    Ident(String),

    // Keywords
    KeywordFn,
    KeywordIf,
    KeywordElif,
    KeywordElse,
    KeywordLoop,
    KeywordForm,
    KeywordType,
    KeywordTrue,
    KeywordFalse,
    KeywordSelf,
    KeywordBreak,
    KeywordContinue,
    KeywordReturn,
    KeywordPrint,

    Unknown(char),
    Eof,
}