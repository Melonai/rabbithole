use std::fmt::{write, Display};

#[derive(Clone, Copy, Debug)]
pub struct Location {
    pub col: usize,
    pub row: usize,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub location: Location,
    pub kind: TokenKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
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

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::OpPlus => write!(f, "+"),
            TokenKind::OpMinus => write!(f, "-"),
            TokenKind::OpStar => write!(f, "*"),
            TokenKind::OpSlash => write!(f, "/"),
            TokenKind::OpEq => write!(f, "=="),
            TokenKind::OpNot => write!(f, "!"),
            TokenKind::OpNeq => write!(f, "!="),
            TokenKind::OpLt => write!(f, "<"),
            TokenKind::OpGt => write!(f, ">"),
            TokenKind::OpLte => write!(f, "<="),
            TokenKind::OpGte => write!(f, ">="),
            TokenKind::OpAnd => write!(f, "&&"),
            TokenKind::OpOr => write!(f, "||"),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::SemiColon => write!(f, ";"),
            TokenKind::Assign => write!(f, "="),
            TokenKind::ConstAssign => write!(f, ":="),
            TokenKind::Arrow => write!(f, "->"),
            TokenKind::GroupOpen => write!(f, "("),
            TokenKind::GroupClose => write!(f, ")"),
            // {{ Escapes a single {
            TokenKind::BlockOpen => write!(f, "{{"),
            TokenKind::BlockClose => write!(f, "}}"),
            TokenKind::ArrayOpen => write!(f, "["),
            TokenKind::ArrayClose => write!(f, "]"),
            TokenKind::Int(v) => write!(f, "{}", v),
            TokenKind::Float(v) => write!(f, "{}", v),
            // These are a bit weird, because they aren't direct mappi
            TokenKind::Str(v) => write!(f, "\"{}\"", v),
            TokenKind::StrOpen => write!(f, "\""),
            TokenKind::StrClose => write!(f, "\""),
            TokenKind::StrEmbed(v) => write!(f, "{{{}}}", v),
            TokenKind::Ident(v) => write!(f, "{}", v),
            TokenKind::KeywordFn => write!(f, "fn"),
            TokenKind::KeywordIf => write!(f, "if"),
            TokenKind::KeywordElif => write!(f, "elif"),
            TokenKind::KeywordElse => write!(f, "else"),
            TokenKind::KeywordLoop => write!(f, "loop"),
            TokenKind::KeywordForm => write!(f, "form"),
            TokenKind::KeywordType => write!(f, "type"),
            TokenKind::KeywordTrue => write!(f, "true"),
            TokenKind::KeywordFalse => write!(f, "false"),
            TokenKind::KeywordSelf => write!(f, "self"),
            TokenKind::KeywordBreak => write!(f, "break"),
            TokenKind::KeywordContinue => write!(f, "continue"),
            TokenKind::KeywordReturn => write!(f, "return"),
            TokenKind::KeywordPrint => write!(f, "print"),
            TokenKind::Unknown(v) => write!(f, "{}", v),
            TokenKind::Eof => write!(f, "end of file"),
        }
    }
}
