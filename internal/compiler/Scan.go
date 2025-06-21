package compiler

import (
	"unicode"
	"unicode/utf8"
)

const EOFChar = -1 // Use -1 as EOF character (common in lexers)

type Scanner struct {
	file    *UnitFile
	content string
	pos     int   // current position in content
	line    int32 // current line number (1-based)
	column  int32 // current column number (1-based)
	ch      rune  // current character

	// keyword lookup table for efficient keyword recognition
	keywords map[string]TokenKind
}

func NewScanner(file *UnitFile) *Scanner {
	scanner := &Scanner{
		file:     file,
		content:  file.content, // Use the original content directly
		pos:      0,
		line:     1,
		column:   1,
		keywords: make(map[string]TokenKind),
	}

	// Initialize keyword lookup table
	scanner.initKeywords()

	// Read the first character
	scanner.readChar()

	return scanner
}

func (s *Scanner) initKeywords() {
	s.keywords = make(map[string]TokenKind)

	// Automatically populate keywords using the token range markers
	for kind := TokenKeywordBegin; kind <= TokenKeywordEnd; kind++ {
		keywordText := kind.String()
		s.keywords[keywordText] = kind
	}
}

func (s *Scanner) readChar() {
	if s.isEOF() {
		return // Don't modify ch when at EOF
	}

	ch, size := utf8.DecodeRuneInString(s.content[s.pos:])
	s.ch = ch
	s.pos += size

	if ch == '\n' {
		s.line++
		s.column = 1
	} else {
		s.column++
	}
}

func (s *Scanner) peekChar() rune {
	if s.isEOF() {
		return EOFChar
	}
	ch, _ := utf8.DecodeRuneInString(s.content[s.pos:])
	return ch
}

func (s *Scanner) isEOF() bool {
	return s.pos >= len(s.content)
}

func (s *Scanner) skipWhitespace() {
	for !s.isEOF() && unicode.IsSpace(s.ch) {
		s.readChar()
	}
}

func (s *Scanner) readIdentifier() string {
	start := s.pos - 1 // -1 because we already read the first char
	for !s.isEOF() && (unicode.IsLetter(s.ch) || unicode.IsDigit(s.ch) || s.ch == '_') {
		s.readChar()
	}
	return s.content[start : s.pos-1]
}

func (s *Scanner) readNumber() (TokenKind, string) {
	start := s.pos - 1
	kind := TokenLiteralInt // Handle different number bases (0x, 0b, 0o)
	if s.ch == '0' && !s.isEOF() {
		next := s.peekChar()
		if next != EOFChar {
			switch next {
			case 'x', 'X':
				s.readChar() // consume '0'
				s.readChar() // consume 'x'
				s.readHexDigits()
				return kind, s.content[start : s.pos-1]
			case 'b', 'B':
				s.readChar() // consume '0'
				s.readChar() // consume 'b'
				s.readBinaryDigits()
				return kind, s.content[start : s.pos-1]
			case 'o', 'O':
				s.readChar() // consume '0'
				s.readChar() // consume 'o'
				s.readOctalDigits()
				return kind, s.content[start : s.pos-1]
			}
		}
	}

	// Read decimal number
	s.readDecimalDigits() // Check for float (decimal point)
	if s.ch == '.' {
		next := s.peekChar()
		if next != EOFChar && unicode.IsDigit(next) {
			kind = TokenLiteralFloat
			s.readChar() // consume '.'
			s.readDecimalDigits()
		}
	}

	// Check for scientific notation
	if s.ch == 'e' || s.ch == 'E' {
		kind = TokenLiteralFloat
		s.readChar() // consume 'e'/'E'
		if s.ch == '+' || s.ch == '-' {
			s.readChar() // consume sign
		}
		s.readDecimalDigits()
	}

	return kind, s.content[start : s.pos-1]
}

func (s *Scanner) readDecimalDigits() {
	for !s.isEOF() && unicode.IsDigit(s.ch) {
		s.readChar()
	}
}

func (s *Scanner) readHexDigits() {
	for !s.isEOF() && (unicode.IsDigit(s.ch) || (s.ch >= 'a' && s.ch <= 'f') || (s.ch >= 'A' && s.ch <= 'F')) {
		s.readChar()
	}
}

func (s *Scanner) readBinaryDigits() {
	for !s.isEOF() && (s.ch == '0' || s.ch == '1') {
		s.readChar()
	}
}

func (s *Scanner) readOctalDigits() {
	for !s.isEOF() && (s.ch >= '0' && s.ch <= '7') {
		s.readChar()
	}
}

func (s *Scanner) readString() string {
	start := s.pos // don't include opening quote

	for {
		s.readChar()
		if s.ch == '"' || s.isEOF() {
			break
		}
		// Handle escape sequences
		if s.ch == '\\' {
			s.readChar() // skip escaped character
		}
	}

	value := s.content[start : s.pos-1]
	if s.ch == '"' {
		s.readChar() // consume closing quote
	}

	return value
}

func (s *Scanner) readComment() string {
	start := s.pos

	for !s.isEOF() && s.ch != '\n' {
		s.readChar()
	}

	return s.content[start:s.pos]
}

func (s *Scanner) createLocation(startLine, startColumn int32, startPos int) Location {
	return Location{
		Position: Position{Line: startLine, Column: startColumn},
		Range:    Range{Begin: int32(startPos), End: int32(s.pos)}, // Byte offsets into content
		File:     s.file,
	}
}

func (s *Scanner) Scan() Token {
	s.skipWhitespace()

	startLine := s.line
	startColumn := s.column
	startPos := s.pos - 1 // -1 to account for the current character position

	// Handle EOF
	if s.isEOF() {
		return Token{
			kind:     TokenEOF,
			value:    "",
			location: s.createLocation(startLine, startColumn, startPos),
		}
	}

	// Handle identifiers and keywords
	if unicode.IsLetter(s.ch) || s.ch == '_' {
		value := s.readIdentifier()
		kind := TokenIdentifier

		// Check if it's a keyword
		if keywordKind, exists := s.keywords[value]; exists {
			kind = keywordKind
		}

		return Token{
			kind:     kind,
			value:    value,
			location: s.createLocation(startLine, startColumn, startPos),
		}
	}

	// Handle numbers
	if unicode.IsDigit(s.ch) {
		kind, value := s.readNumber()
		return Token{
			kind:     kind,
			value:    value,
			location: s.createLocation(startLine, startColumn, startPos),
		}
	}

	// Handle single character tokens and operators
	ch := s.ch
	s.readChar()

	switch ch {
	case '(':
		return Token{kind: TokenLParen, value: "(", location: s.createLocation(startLine, startColumn, startPos)}
	case ')':
		return Token{kind: TokenRParen, value: ")", location: s.createLocation(startLine, startColumn, startPos)}
	case '{':
		return Token{kind: TokenLBrace, value: "{", location: s.createLocation(startLine, startColumn, startPos)}
	case '}':
		return Token{kind: TokenRBrace, value: "}", location: s.createLocation(startLine, startColumn, startPos)}
	case '[':
		return Token{kind: TokenLBracket, value: "[", location: s.createLocation(startLine, startColumn, startPos)}
	case ']':
		return Token{kind: TokenRBracket, value: "]", location: s.createLocation(startLine, startColumn, startPos)}
	case ';':
		return Token{kind: TokenSemicolon, value: ";", location: s.createLocation(startLine, startColumn, startPos)}
	case ',':
		return Token{kind: TokenComma, value: ",", location: s.createLocation(startLine, startColumn, startPos)}
	case '.':
		return Token{kind: TokenDot, value: ".", location: s.createLocation(startLine, startColumn, startPos)}

	// Operators that might have compound forms
	case '=':
		if s.ch == '=' {
			s.readChar()
			return Token{kind: TokenEQ, value: "==", location: s.createLocation(startLine, startColumn, startPos)}
		}
		return Token{kind: TokenAssign, value: "=", location: s.createLocation(startLine, startColumn, startPos)}

	case '!':
		if s.ch == '=' {
			s.readChar()
			return Token{kind: TokenNE, value: "!=", location: s.createLocation(startLine, startColumn, startPos)}
		}
		return Token{kind: TokenNot, value: "!", location: s.createLocation(startLine, startColumn, startPos)}

	case '<':
		if s.ch == '=' {
			s.readChar()
			return Token{kind: TokenLE, value: "<=", location: s.createLocation(startLine, startColumn, startPos)}
		} else if s.ch == '<' {
			s.readChar()
			if s.ch == '=' {
				s.readChar()
				return Token{kind: TokenShlAssign, value: "<<=", location: s.createLocation(startLine, startColumn, startPos)}
			}
			return Token{kind: TokenShl, value: "<<", location: s.createLocation(startLine, startColumn, startPos)}
		}
		return Token{kind: TokenLT, value: "<", location: s.createLocation(startLine, startColumn, startPos)}

	case '>':
		if s.ch == '=' {
			s.readChar()
			return Token{kind: TokenGE, value: ">=", location: s.createLocation(startLine, startColumn, startPos)}
		} else if s.ch == '>' {
			s.readChar()
			if s.ch == '=' {
				s.readChar()
				return Token{kind: TokenShrAssign, value: ">>=", location: s.createLocation(startLine, startColumn, startPos)}
			}
			return Token{kind: TokenShr, value: ">>", location: s.createLocation(startLine, startColumn, startPos)}
		}
		return Token{kind: TokenGT, value: ">", location: s.createLocation(startLine, startColumn, startPos)}

	case '+':
		if s.ch == '=' {
			s.readChar()
			return Token{kind: TokenAddAssign, value: "+=", location: s.createLocation(startLine, startColumn, startPos)}
		} else if s.ch == '+' {
			s.readChar()
			return Token{kind: TokenInc, value: "++", location: s.createLocation(startLine, startColumn, startPos)}
		}
		return Token{kind: TokenAdd, value: "+", location: s.createLocation(startLine, startColumn, startPos)}

	case '-':
		if s.ch == '=' {
			s.readChar()
			return Token{kind: TokenSubAssign, value: "-=", location: s.createLocation(startLine, startColumn, startPos)}
		} else if s.ch == '-' {
			s.readChar()
			return Token{kind: TokenDec, value: "--", location: s.createLocation(startLine, startColumn, startPos)}
		}
		return Token{kind: TokenSub, value: "-", location: s.createLocation(startLine, startColumn, startPos)}

	case '*':
		if s.ch == '=' {
			s.readChar()
			return Token{kind: TokenMulAssign, value: "*=", location: s.createLocation(startLine, startColumn, startPos)}
		}
		return Token{kind: TokenMul, value: "*", location: s.createLocation(startLine, startColumn, startPos)}

	case '/':
		if s.ch == '=' {
			s.readChar()
			return Token{kind: TokenDivAssign, value: "/=", location: s.createLocation(startLine, startColumn, startPos)}
		} else if s.ch == '/' {
			s.readChar() // consume second '/'
			value := s.readComment()
			return Token{kind: TokenComment, value: value, location: s.createLocation(startLine, startColumn, startPos)}
		}
		return Token{kind: TokenDiv, value: "/", location: s.createLocation(startLine, startColumn, startPos)}

	case '%':
		if s.ch == '=' {
			s.readChar()
			return Token{kind: TokenModAssign, value: "%=", location: s.createLocation(startLine, startColumn, startPos)}
		}
		return Token{kind: TokenMod, value: "%", location: s.createLocation(startLine, startColumn, startPos)}

	case '&':
		if s.ch == '&' {
			s.readChar()
			return Token{kind: TokenLAnd, value: "&&", location: s.createLocation(startLine, startColumn, startPos)}
		} else if s.ch == '=' {
			s.readChar()
			return Token{kind: TokenAndAssign, value: "&=", location: s.createLocation(startLine, startColumn, startPos)}
		}
		return Token{kind: TokenAnd, value: "&", location: s.createLocation(startLine, startColumn, startPos)}

	case '|':
		if s.ch == '|' {
			s.readChar()
			return Token{kind: TokenLOr, value: "||", location: s.createLocation(startLine, startColumn, startPos)}
		} else if s.ch == '=' {
			s.readChar()
			return Token{kind: TokenOrAssign, value: "|=", location: s.createLocation(startLine, startColumn, startPos)}
		}
		return Token{kind: TokenOr, value: "|", location: s.createLocation(startLine, startColumn, startPos)}

	case '^':
		if s.ch == '=' {
			s.readChar()
			return Token{kind: TokenXorAssign, value: "^=", location: s.createLocation(startLine, startColumn, startPos)}
		}
		return Token{kind: TokenXor, value: "^", location: s.createLocation(startLine, startColumn, startPos)}

	case '"':
		value := s.readString()
		return Token{kind: TokenLiteralString, value: value, location: s.createLocation(startLine, startColumn, startPos)}

	default:
		// Invalid character
		return Token{
			kind:     TokenInvalid,
			value:    string(ch),
			location: s.createLocation(startLine, startColumn, startPos),
		}
	}
}
