use super::scanner::{ScanResult, Scanner, Span};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Token {
    EOF,
    WhiteSpace,
    Comment,

    // atoms
    Int,
    Float,
    String,
    Symbol,

    // keywords
    // Keyword,
    KwSelf, // self (lowercase)
    KwStruct,
    KwFn,
    KwLet,
    KwMut,
    KwIf,
    KwElse,
    KwReturn,
    KwWhile,
    KwFor,

    Semicolon,
    Comma,
    Colon,
    OpenRoundBracket,
    CloseRoundBracket,
    OpenCurlyBracket,
    CloseCurlyBracket,

    Ampersand,
    AmpersandAmpersand,

    Bar,
    BarBar,

    Dot,
    Equal,
    DoubleEqual,
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    Not,
    NotEqual,
    Greater,
    Less,

    LeftShift,
    RightShift,

    GreaterEqual,
    LessEqual,
    Chevron,

    RightArrow,
}

pub struct Lexer {
    scanner: Scanner<char>,
    output: Vec<Span<Token>>,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let scanner = Scanner::new(input.chars().collect());
        let output = Vec::new();
        Self { scanner, output }
    }

    pub fn push(&mut self, span: Span<Token>) {
        self.output.push(span);
    }

    fn transaction(&mut self, mut f: impl FnMut(&mut Self) -> ScanResult) -> ScanResult {
        self.scanner.begin();
        let result = f(self);
        match result {
            ScanResult::Commit => {
                self.scanner.commit();
                ScanResult::Commit
            }
            ScanResult::Rollback => {
                self.scanner.rollback();
                ScanResult::Rollback
            }
        }
    }

    fn string(&mut self) -> ScanResult {
        let start = self.scanner.position();

        if let Some('"') = self.scanner.peek() {
            self.scanner.next();
        } else {
            return ScanResult::Rollback;
        }

        while let Some(c) = self.scanner.peek() {
            match c {
                '"' => {
                    self.scanner.next();
                    break;
                }
                _ => {
                    self.scanner.next();
                }
            }
        }

        let end = self.scanner.position();

        self.push(Span {
            start,
            end,
            value: Token::String,
        });

        ScanResult::Commit
    }

    fn int(&mut self) -> ScanResult {
        let start = self.scanner.position();

        while let Some(c) = self.scanner.peek() {
            match c {
                '0'..='9' => {
                    self.scanner.next();
                }
                _ => {
                    break;
                }
            }
        }

        let end = self.scanner.position();

        if start == end {
            return ScanResult::Rollback;
        }

        self.push(Span {
            start,
            end,
            value: Token::Int,
        });

        ScanResult::Commit
    }

    //TODO fix this shit
    fn float(&mut self) -> ScanResult {
        let start = self.scanner.position();

        if let ScanResult::Rollback = self.int() {
            return ScanResult::Rollback;
        }

        if let Some('.') = self.scanner.peek() {
            self.scanner.next();
        } else {
            return ScanResult::Rollback;
        }

        if let ScanResult::Rollback = self.int() {
            return ScanResult::Rollback;
        }

        let end = self.scanner.position();

        self.push(Span {
            start,
            end,
            value: Token::Float,
        });

        ScanResult::Commit
    }

    fn number(&mut self) -> ScanResult {
        if let ScanResult::Commit = self.transaction(|this| this.int()) {
            return ScanResult::Commit;
        }

        if let ScanResult::Commit = self.transaction(|this| this.float()) {
            return ScanResult::Commit;
        }

        ScanResult::Rollback
    }

    fn whitespace(&mut self) -> ScanResult {
        let start = self.scanner.position();

        while let Some(c) = self.scanner.peek() {
            match c {
                ' ' | '\t' | '\n' => {
                    self.scanner.next();
                }
                _ => {
                    break;
                }
            }
        }

        let end = self.scanner.position();

        if start == end {
            return ScanResult::Rollback;
        }

        self.push(Span {
            start,
            end,
            value: Token::WhiteSpace,
        });

        ScanResult::Commit
    }

    fn operator(&mut self) -> ScanResult {
        let start = self.scanner.position();

        let token = if let Some(c) = self.scanner.peek() {
            match c {
                '.' => Token::Dot,
                '+' => Token::Plus,
                '-' => {
                    if let Some('>') = self.scanner.next() {
                        Token::RightArrow
                    } else {
                        self.scanner.back();
                        Token::Minus
                    }
                }
                '*' => Token::Mul,
                '/' => {
                    if let Some('/') = self.scanner.next() {
                        while let Some(c) = self.scanner.peek() {
                            match c {
                                '\n' => {
                                    break;
                                }
                                _ => {
                                    self.scanner.next();
                                }
                            }
                        }
                        Token::Comment
                    } else {
                        self.scanner.back();
                        Token::Div
                    }
                }
                '^' => Token::Chevron,
                '%' => Token::Mod,
                '(' => Token::OpenRoundBracket,
                ')' => Token::CloseRoundBracket,
                '{' => Token::OpenCurlyBracket,
                '}' => Token::CloseCurlyBracket,
                '&' => {
                    if let Some('&') = self.scanner.next() {
                        Token::AmpersandAmpersand
                    } else {
                        Token::Ampersand
                    }
                }
                '!' => {
                    if let Some('=') = self.scanner.next() {
                        Token::NotEqual
                    } else {
                        self.scanner.back();
                        Token::Not
                    }
                }
                '=' => {
                    if let Some('=') = self.scanner.next() {
                        Token::DoubleEqual
                    } else {
                        self.scanner.back();
                        Token::Equal
                    }
                }
                '<' => match self.scanner.next() {
                    Some('=') => Token::LessEqual,
                    Some('<') => Token::LeftShift,
                    _ => {
                        self.scanner.back();
                        Token::Less
                    }
                },
                '>' => match self.scanner.next() {
                    Some('=') => Token::GreaterEqual,
                    Some('>') => Token::RightShift,
                    _ => {
                        self.scanner.back();
                        Token::Greater
                    }
                },
                '|' => match self.scanner.next() {
                    Some('|') => Token::BarBar,
                    _ => {
                        self.scanner.back();
                        Token::Bar
                    }
                },

                ';' => Token::Semicolon,
                ',' => Token::Comma,
                ':' => Token::Colon,
                _ => return ScanResult::Rollback,
            }
        } else {
            return ScanResult::Rollback;
        };

        self.scanner.next();

        let end = self.scanner.position();

        self.push(Span {
            start,
            end,
            value: token,
        });

        ScanResult::Commit
    }

    fn keyword_or_symbol(&mut self) -> ScanResult {
        let start = self.scanner.position();

        // let keywords = [
        //     "self", "struct", "fn", "let", "mut", "if", "else", "return", "while", "for",
        // ];

        let mut keyword = String::new();

        if let Some(&c @ ('a'..='z' | 'A'..='Z' | '_')) = self.scanner.peek() {
            keyword.push(c);
            self.scanner.next();
            while let Some(&c) = self.scanner.peek() {
                match c {
                    'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                        keyword.push(c);
                        self.scanner.next();
                    }
                    _ => {
                        break;
                    }
                }
            }
        } else {
            return ScanResult::Rollback;
        }

        let token = match keyword.as_str() {
            "self" => Token::KwSelf,
            "struct" => Token::KwStruct,
            "fn" => Token::KwFn,
            "let" => Token::KwLet,
            "mut" => Token::KwMut,
            "if" => Token::KwIf,
            "else" => Token::KwElse,
            "return" => Token::KwReturn,
            "while" => Token::KwWhile,
            "for" => Token::KwFor,
            _ => Token::Symbol,
        };

        let end = self.scanner.position();

        self.push(Span {
            start,
            end,
            value: token,
        });

        ScanResult::Commit
    }

    pub fn lex(mut self) -> Vec<Span<Token>> {
        'while_loop: while let Some(_) = self.scanner.peek() {
            for f in [
                Self::whitespace,
                Self::string,
                Self::keyword_or_symbol,
                Self::number,
                Self::operator,
            ] {
                self.scanner.begin();
                let result = f(&mut self);

                match result {
                    ScanResult::Commit => {
                        self.scanner.commit();
                        continue 'while_loop;
                    }
                    ScanResult::Rollback => {
                        self.scanner.rollback();
                    }
                }
            }
            println!("unexpected character {:?}", self.scanner.peek());
            self.scanner.next();
        }
        self.output
    }
}
