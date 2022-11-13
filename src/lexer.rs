use self::Token::*;
use std::str::Chars;

#[derive(Debug, PartialEq, Copy, Clone)]
pub(crate) enum Token<'a> {
    Identifier(&'a str),
    Int(&'a str),
    Float(&'a str),
    String(&'a str),

    // Keywords
    /// "als"
    If,
    /// "anders"
    Else,
    /// "antwoord"
    Return,
    /// "functie"
    Func,
    /// "zolang"
    While,

    /// "stel"
    Declare,
    /// "waar"
    True,
    /// "onwaar"
    False,
    /// "stop"
    Break,
    /// "volgende"
    Continue,

    // Multi-char tokens:
    /// "<="
    Lte,
    /// ">="
    Gte,
    /// "=="
    Eq,
    /// "!="
    Neq,
    /// "&&"
    And,
    /// "||"
    Or,

    // TODO! Tokenize these separately? Or leave it up to parser?
    // /// "+="
    // PlusAssign,
    // /// "-="
    // MinusAssign,
    // /// "*="
    // MultiplyAssign,
    // /// "/="
    // DivideAssign,

    // One-char tokens:
    /// "="
    Assign,
    /// ";"
    Semi,
    /// ","
    Comma,
    /// "."
    Dot,
    /// "("
    OpenParen,
    /// ")"
    CloseParen,
    /// "{"
    OpenBrace,
    /// "}"
    CloseBrace,
    /// "["
    OpenBracket,
    /// "]"
    CloseBracket,
    /// "!"
    Bang,
    /// "<"
    Lt,
    /// ">"
    Gt,
    /// "-"
    Minus,
    /// "+"
    Plus,
    /// "*"
    Star,
    /// "/"
    Slash,
    /// "^"
    Caret,
    /// "%"
    Percent,

    /// Unknown token, not expected by the lexer, e.g. "№"
    Illegal,
}

/// Peekable iterator over a char sequence.
///
/// Next characters can be peeked via `first` method,
/// and position can be shifted forward via `bump` method.
pub(crate) struct Tokenizer<'a> {
    pos: usize,
    input: &'a str,
    /// Iterator over chars. Slightly faster than a &str.
    chars: Chars<'a>,
}

impl<'a> Tokenizer<'a> {
    pub(crate) fn new(input: &str) -> Tokenizer {
        Tokenizer {
            pos: 0,
            input,
            chars: input.chars(),
        }
    }

    /// Peeks the next char from the input stream without consuming it.
    #[inline]
    fn peek(&self) -> Option<char> {
        // `.next()` optimizes better than `.nth(0)`
        self.chars.clone().next()
    }

    /// Checks if there is nothing more to consume.
    #[inline]
    fn is_eof(&self) -> bool {
        self.offset() >= self.input.len()
    }

    /// Moves to the next character.
    #[inline]
    fn bump(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        self.pos += c.len_utf8();
        Some(c)
    }

    /// Returns current offset into the source file
    #[inline]
    fn offset(&self) -> usize {
        self.pos
    }

    /// Takes a string slice from and to given offsets, respectively
    #[inline]
    fn read_str(&self, from: usize, to: usize) -> &'a str {
        &self.input[from..to]
    }

    /// Eats symbols while predicate returns true or until the end of file is reached.
    #[inline]
    fn skip_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        // It was tried making optimized version of this for eg. line comments, but
        // LLVM can inline all of this and compile it down to fast iteration over bytes.
        while !self.is_eof() && predicate(self.peek().unwrap()) {
            self.bump();
        }
    }
}

impl<'a> From<&'a str> for Token<'a> {
    fn from(value: &'a str) -> Self {
        match value {
            "als" => If,
            "antwoord" => Return,
            "zolang" => While,
            "anders" => Else,
            "functie" => Func,
            "stel" => Declare,
            "ja" => True,
            "nee" => False,
            "volgende" => Continue,
            "stop" => Break,
            _ => Identifier(value),
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let start = self.offset();
        let token = match self.bump()? {
            // Identifiers
            c if c.is_alphabetic() || c == '_' => {
                // first char must be alphabetic, but consecutive chars can have integers
                self.skip_while(|c| c.is_alphanumeric() || c == '_');
                let ident = self.read_str(start, self.offset());
                ident.into()
            }

            // Integers & Floats
            '0'..='9' => {
                let mut decimal = false;
                self.skip_while(|c| {
                    if c.is_ascii_digit() {
                        return true;
                    }

                    if !decimal && c == '.' {
                        decimal = true;
                        return true;
                    }

                    false
                });
                let strval = self.read_str(start, self.offset());
                if decimal {
                    Float(strval)
                } else {
                    Int(strval)
                }
            }

            // String values
            '"' => {
                self.skip_while(|c| c != '"');
                self.bump();
                String(self.read_str(start + 1, self.offset() - 1))
            }

            // Whitespace (skipped, because insignificant)
            c if is_whitespace(c) => return self.next(),

            // Multi-char tokens:
            '=' => {
                if self.peek() == Some('=') {
                    Eq
                } else {
                    Assign
                }
            }
            '!' => {
                if self.peek() == Some('=') {
                    Neq
                } else {
                    Bang
                }
            }
            '<' => {
                if self.peek() == Some('=') {
                    Lte
                } else {
                    Lt
                }
            }
            '>' => {
                if self.peek() == Some('=') {
                    Gte
                } else {
                    Gt
                }
            }
            '/' => {
                // Two consecutive slashes indicate the start of a single-line comment
                // So skip forward until end of line.
                if self.peek() == Some('/') {
                    self.skip_while(|c| c != '\n');
                    return self.next();
                } else {
                    Slash
                }
            }
            '&' if self.peek() == Some('&') => And,
            '|' if self.peek() == Some('|') => Or,
            // One-symbol tokens.
            ';' => Semi,
            ',' => Comma,
            '.' => Dot,
            '(' => OpenParen,
            ')' => CloseParen,
            '{' => OpenBrace,
            '}' => CloseBrace,
            '[' => OpenBracket,
            ']' => CloseBracket,
            '-' => Minus,
            '+' => Plus,
            '*' => Star,
            '^' => Caret,
            '%' => Percent,

            // Unknown / illegal tokens
            _ => Illegal,
        };

        // If we parsed a multi-char token,
        // bump iterator appropriate number of times
        match token {
            Eq | Neq | Gte | Lte | And | Or => self.bump(),
            _ => None,
        };

        Some(token)
    }
}

/// True if `c` is considered a whitespace according to Rust language definition.
/// See [Rust language reference](https://doc.rust-lang.org/reference/whitespace.html)
/// for definitions of these classes.
#[inline]
pub fn is_whitespace(c: char) -> bool {
    // This is Pattern_White_Space.
    //
    // Note that this set is stable (ie, it doesn't change with different
    // Unicode versions), so it's ok to just hard-code the values.

    matches!(
        c,
        // Usual ASCII suspects
        '\u{0009}'   // \t
        | '\u{000A}' // \n
        | '\u{000B}' // vertical tab
        | '\u{000C}' // form feed
        | '\u{000D}' // \r
        | '\u{0020}' // space

        // NEXT LINE from latin1
        | '\u{0085}'

        // Bidi markers
        | '\u{200E}' // LEFT-TO-RIGHT MARK
        | '\u{200F}' // RIGHT-TO-LEFT MARK

        // Dedicated whitespace characters from Unicode
        | '\u{2028}' // LINE SEPARATOR
        | '\u{2029}' // PARAGRAPH SEPARATOR
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ints() {
        for (input, expected) in [("5", vec![Int("5")]), ("-5", vec![Minus, Int("5")])] {
            let tokenizer = Tokenizer::new(input);
            assert_eq!(
                expected,
                tokenizer.collect::<Vec<Token>>(),
                "tokenizer input: {}",
                input
            );
        }
    }

    #[test]
    fn floats() {
        for (input, expected) in [
            ("5.0", vec![Float("5.0")]),
            ("-5.0", vec![Minus, Float("5.0")]),
        ] {
            let tokenizer = Tokenizer::new(input);
            assert_eq!(
                expected,
                tokenizer.collect::<Vec<Token>>(),
                "tokenizer input: {}",
                input
            );
        }
    }

    #[test]
    fn bools() {
        assert_eq!(Some(True), Tokenizer::new("ja").next());
        assert_eq!(Some(False), Tokenizer::new("nee").next());
    }

    #[test]
    fn tokenize_declaration() {
        let mut tokenizer = Tokenizer::new("stel foobar = 5;");
        assert_eq!(tokenizer.next(), Some(Declare));
        assert_eq!(tokenizer.next(), Some(Identifier("foobar")));
        assert_eq!(tokenizer.next(), Some(Assign));
        assert_eq!(tokenizer.next(), Some(Int("5")));
        assert_eq!(tokenizer.next(), Some(Semi));
        assert_eq!(tokenizer.next(), None);
    }

    #[test]
    fn tokenize_infix_expression() {
        for (input, expected) in [
            ("5 - 10", vec![Int("5"), Minus, Int("10")]),
            ("5 + 10", vec![Int("5"), Plus, Int("10")]),
            ("5 * 10", vec![Int("5"), Star, Int("10")]),
            ("5 / 10", vec![Int("5"), Slash, Int("10")]),
            ("5 % 10", vec![Int("5"), Percent, Int("10")]),
            ("5 >= 10", vec![Int("5"), Gte, Int("10")]),
            ("5 <= 10", vec![Int("5"), Lte, Int("10")]),
            ("5 < 10", vec![Int("5"), Lt, Int("10")]),
            ("5 > 10", vec![Int("5"), Gt, Int("10")]),
            ("5 == 10", vec![Int("5"), Eq, Int("10")]),
            ("5 != 10", vec![Int("5"), Neq, Int("10")]),
            ("ja && ja", vec![True, And, True]),
            ("ja || ja", vec![True, Or, True]),
        ] {
            let tokenizer = Tokenizer::new(input);
            assert_eq!(
                expected,
                tokenizer.collect::<Vec<Token>>(),
                "tokenizer input: {}",
                input
            );
        }
    }

    #[test]
    fn tokenize_keywords() {
        let mut tokenizer = Tokenizer::new("als anders antwoord functie stel stop volgende");
        assert_eq!(tokenizer.next(), Some(If));
        assert_eq!(tokenizer.next(), Some(Else));
        assert_eq!(tokenizer.next(), Some(Return));
        assert_eq!(tokenizer.next(), Some(Func));
        assert_eq!(tokenizer.next(), Some(Declare));
        assert_eq!(tokenizer.next(), Some(Break));
        assert_eq!(tokenizer.next(), Some(Continue));
        assert_eq!(tokenizer.next(), None);
    }

    #[test]
    fn prefix_expressions() {
        for (input, expected) in [
            ("!ja", vec![Bang, True]),
            ("-5.0", vec![Minus, Float("5.0")]),
        ] {
            let tokenizer = Tokenizer::new(input);
            assert_eq!(
                expected,
                tokenizer.collect::<Vec<Token>>(),
                "tokenizer input: {}",
                input
            );
        }
    }
}
