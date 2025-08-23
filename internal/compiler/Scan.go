package compiler

import (
	"unicode"
	"unicode/utf8"
)

type LocationPoint struct {
	pos    int32
	line   int32
	column int32
}

func NewLocationPoint() LocationPoint {
	return LocationPoint{
		pos:    0,
		line:   1,
		column: 1,
	}
}

const eof = utf8.RuneError

type Scanner struct {
	file             *UnitFile
	currentLocation  LocationPoint
	currentCharCache rune
	insertSemi       bool

	keywords map[string]TokenKind
}

func NewScanner(file *UnitFile) *Scanner {
	scanner := &Scanner{
		file:            file,
		currentLocation: NewLocationPoint(),
		insertSemi:      false,
		keywords:        make(map[string]TokenKind),
	}

	scanner.initKeywords()
	scanner.currentCharCache, _ = utf8.DecodeRuneInString(scanner.file.content)

	return scanner
}

func (s *Scanner) createSourceLocation(a LocationPoint) SourceLocation {
	return SourceLocation{
		Position:    SourcePosition{a.line, a.column},
		BeginOffset: a.pos,
		File:        s.file,
	}
}

func (s *Scanner) createSourceRange(a, b LocationPoint) SourceRange {
	return NewSourceRange(s.createSourceLocation(a), s.createSourceLocation(b))
}

func (s *Scanner) createTokenFromSourceRange(kind TokenKind, r SourceRange) Token {
	return Token{
		kind:        kind,
		value:       s.file.content[r.BeginOffset:r.EndOffset],
		sourceRange: r,
	}
}

func (s *Scanner) createTokenFromLocationPoint(kind TokenKind, locP LocationPoint) Token {
	return s.createTokenFromSourceRange(kind, s.createSourceRange(locP, s.currentLocation))
}

func (s *Scanner) initKeywords() {
	s.keywords = make(map[string]TokenKind)

	for kind := TokenKeywordBegin; kind <= TokenKeywordEnd; kind++ {
		keywordText := kind.String()
		s.keywords[keywordText] = kind
	}
}

func (s *Scanner) readChar() rune {
	if s.isEOF() {
		s.currentCharCache = eof
	} else {
		ch, size := utf8.DecodeRuneInString(s.file.content[s.currentLocation.pos:])
		s.currentLocation.pos += int32(size)
		s.currentCharCache, _ = utf8.DecodeRuneInString(s.file.content[s.currentLocation.pos:])

		if ch == '\n' {
			s.currentLocation.line++
			s.currentLocation.column = 1
		} else {
			s.currentLocation.column++
		}
	}
	return s.currentCharCache
}

func (s *Scanner) peekChar(l int) rune {
	if s.isEOF() {
		return eof
	}

	pos, char := s.currentLocation.pos, s.currentCharCache
	for i := 0; i < l; i++ {
		ch, size := utf8.DecodeRuneInString(s.file.content[pos:])
		char = ch
		pos += int32(size)
	}
	return char
}

func (s *Scanner) currentChar() rune {
	return s.currentCharCache
}

func (s *Scanner) isEOF() bool {
	return int(s.currentLocation.pos) >= len(s.file.content)
}

func (s *Scanner) skipWhitespace() {
	for s.currentChar() == ' ' ||
		s.currentChar() == '\t' ||
		s.currentChar() == '\n' && !s.insertSemi ||
		s.currentChar() == '\r' {
		s.readChar()
	}
}

func (s *Scanner) readIdentifier() Token {
	start := s.currentLocation
	for unicode.IsLetter(s.currentChar()) || unicode.IsDigit(s.currentChar()) || s.currentChar() == '_' {
		s.readChar()
	}
	return s.createTokenFromLocationPoint(TokenIdentifier, start)
}

func (s *Scanner) readNumber() Token {
	start := s.currentLocation
	if s.currentChar() == '0' {
		next := s.peekChar(2)
		switch next {
		case 'x', 'X':
			s.readChar() // consume '0'
			s.readChar() // consume 'x'
			s.readHexDigits()
			return s.createTokenFromLocationPoint(TokenLiteralInt, start)
		case 'b', 'B':
			s.readChar() // consume '0'
			s.readChar() // consume 'b'
			s.readBinaryDigits()
			return s.createTokenFromLocationPoint(TokenLiteralInt, start)
		case 'o', 'O':
			s.readChar() // consume '0'
			s.readChar() // consume 'o'
			s.readOctalDigits()
			return s.createTokenFromLocationPoint(TokenLiteralInt, start)
		}
	}

	// Read decimal number
	s.readDecimalDigits() // Check for float (decimal point)
	kind := TokenLiteralInt
	if s.currentChar() == '.' {
		next := s.peekChar(2)
		if unicode.IsDigit(next) {
			kind = TokenLiteralFloat
			s.readChar() // consume '.'
			s.readDecimalDigits()
		}
	}

	// Check for scientific notation
	if s.currentChar() == 'e' || s.currentChar() == 'E' {
		s.readChar() // consume 'e'/'E'
		if s.currentChar() == '+' || s.currentChar() == '-' {
			s.readChar() // consume sign
		}
		kind = TokenLiteralFloat
		s.readDecimalDigits()
	}

	return s.createTokenFromLocationPoint(kind, start)
}

func (s *Scanner) readDecimalDigits() {
	for unicode.IsDigit(s.currentCharCache) {
		s.readChar()
	}
}

func (s *Scanner) readHexDigits() {
	for unicode.IsDigit(s.currentChar()) || (s.currentChar() >= 'a' && s.currentChar() <= 'f') || (s.currentChar() >= 'A' && s.currentChar() <= 'F') {
		s.readChar()
	}
}

func (s *Scanner) readBinaryDigits() {
	for s.currentChar() == '0' || s.currentChar() == '1' {
		s.readChar()
	}
}

func (s *Scanner) readOctalDigits() {
	for s.currentChar() >= '0' && s.currentChar() <= '7' {
		s.readChar()
	}
}

func (s *Scanner) readString() Token {
	start := s.currentLocation
	if s.currentChar() != '"' {
		panic("strings should start with \"")
	}

	s.readChar()

	for !s.isEOF() {
		if s.currentChar() == '"' {
			break
		}
		// Handle escape sequences
		if s.currentChar() == '\\' {
			s.readChar() // skip escaped character
		}
		s.readChar()
	}

	if s.currentChar() == '"' {
		s.readChar() // consume closing quote
	}
	token := s.createTokenFromLocationPoint(TokenLiteralString, start)

	return token
}

func (s *Scanner) readRawString() Token {
	start := s.currentLocation
	if s.currentChar() != '`' {
		panic("raw strings should start with `")
	}

	s.readChar()

	for !s.isEOF() {
		if s.currentChar() == '`' {
			break
		}
		s.readChar()
	}

	if s.currentChar() == '`' {
		s.readChar() // consume closing quote
	}
	token := s.createTokenFromLocationPoint(TokenLiteralString, start)

	return token
}

func (s *Scanner) readComment(start LocationPoint) Token {
	for s.currentChar() != '\n' {
		if s.currentChar() == '\r' && s.peekChar(2) == '\n' {
			s.readChar() // Consume \r
			s.readChar() // Consume \n
			break
		} else {
			s.readChar()
		}
	}
	// probably EOF case
	return s.createTokenFromLocationPoint(TokenComment, start)
}

func (s *Scanner) Scan() Token {
	s.skipWhitespace()

	insertSemi := false
	defer func() {
		s.insertSemi = insertSemi
	}()

	// Handle identifiers and keywords
	if unicode.IsLetter(s.currentChar()) || s.currentChar() == '_' {
		token := s.readIdentifier()

		// Check if it's a keyword
		if keywordKind, exists := s.keywords[token.Value()]; exists {
			token.kind = keywordKind
		}

		switch token.kind {
		// in go spec, true and false are considered identifiers, but we don't, maybe we should fix that later
		case TokenIdentifier, TokenBreak, TokenFallthrough, TokenContinue, TokenReturn, TokenTrue, TokenFalse:
			insertSemi = true
		}

		return token
	}

	// Handle numbers
	if unicode.IsDigit(s.currentChar()) {
		insertSemi = true
		return s.readNumber()
	}

	start := s.currentLocation
	switch s.currentChar() {
	case eof:
		if s.insertSemi {
			s.insertSemi = false
			return Token{
				kind:        TokenSemicolon,
				value:       "\n",
				sourceRange: s.createSourceRange(s.currentLocation, s.currentLocation),
			}
		}
		return Token{
			kind:        TokenEOF,
			value:       "",
			sourceRange: s.createSourceRange(s.currentLocation, s.currentLocation),
		}
	case '\n':
		s.insertSemi = false
		s.readChar()
		return s.createTokenFromLocationPoint(TokenSemicolon, start)
	case '(':
		s.readChar()
		return s.createTokenFromLocationPoint(TokenLParen, start)
	case ')':
		insertSemi = true
		s.readChar()
		return s.createTokenFromLocationPoint(TokenRParen, start)
	case '{':
		s.readChar()
		return s.createTokenFromLocationPoint(TokenLBrace, start)
	case '}':
		insertSemi = true
		s.readChar()
		return s.createTokenFromLocationPoint(TokenRBrace, start)
	case '[':
		s.readChar()
		return s.createTokenFromLocationPoint(TokenLBracket, start)
	case ']':
		insertSemi = true
		s.readChar()
		return s.createTokenFromLocationPoint(TokenRBracket, start)
	case ';':
		s.readChar()
		return s.createTokenFromLocationPoint(TokenSemicolon, start)
	case ',':
		s.readChar()
		return s.createTokenFromLocationPoint(TokenComma, start)
	case '.':
		s.readChar()
		return s.createTokenFromLocationPoint(TokenDot, start)

	// Operators that might have compound forms
	case '=':
		s.readChar()
		if s.currentChar() == '=' {
			s.readChar()
			return s.createTokenFromLocationPoint(TokenEQ, start)
		}
		return s.createTokenFromLocationPoint(TokenAssign, start)

	case '!':
		s.readChar()
		if s.currentChar() == '=' {
			s.readChar()
			return s.createTokenFromLocationPoint(TokenNE, start)
		}
		return s.createTokenFromLocationPoint(TokenNot, start)

	case '<':
		s.readChar()
		if s.currentChar() == '=' {
			s.readChar()
			return s.createTokenFromLocationPoint(TokenLE, start)
		} else if s.currentChar() == '<' {
			s.readChar()
			if s.currentChar() == '=' {
				s.readChar()
				return s.createTokenFromLocationPoint(TokenShlAssign, start)
			}
			return s.createTokenFromLocationPoint(TokenShl, start)
		}
		return s.createTokenFromLocationPoint(TokenLT, start)

	case '>':
		s.readChar()
		if s.currentChar() == '=' {
			s.readChar()
			return s.createTokenFromLocationPoint(TokenGE, start)
		} else if s.currentChar() == '>' {
			s.readChar()
			if s.currentChar() == '=' {
				s.readChar()
				return s.createTokenFromLocationPoint(TokenShrAssign, start)
			}
			return s.createTokenFromLocationPoint(TokenShr, start)
		}
		return s.createTokenFromLocationPoint(TokenGT, start)

	case '+':
		s.readChar()
		if s.currentChar() == '=' {
			s.readChar()
			return s.createTokenFromLocationPoint(TokenAddAssign, start)
		} else if s.currentChar() == '+' {
			insertSemi = true
			s.readChar()
			return s.createTokenFromLocationPoint(TokenInc, start)
		}
		return s.createTokenFromLocationPoint(TokenAdd, start)

	case '-':
		s.readChar()
		if s.currentChar() == '=' {
			s.readChar()
			return s.createTokenFromLocationPoint(TokenSubAssign, start)
		} else if s.currentChar() == '-' {
			insertSemi = true
			s.readChar()
			return s.createTokenFromLocationPoint(TokenDec, start)
		}
		return s.createTokenFromLocationPoint(TokenSub, start)

	case '*':
		s.readChar()
		if s.currentChar() == '=' {
			s.readChar()
			return s.createTokenFromLocationPoint(TokenMulAssign, start)
		}
		return s.createTokenFromLocationPoint(TokenMul, start)

	case '/':
		s.readChar()
		if s.currentChar() == '=' {
			s.readChar()
			return s.createTokenFromLocationPoint(TokenDivAssign, start)
		} else if s.currentChar() == '/' {
			insertSemi = true
			s.readChar() // consume second '/'
			return s.readComment(start)
		}
		return s.createTokenFromLocationPoint(TokenDiv, start)

	case '%':
		s.readChar()
		if s.currentChar() == '=' {
			s.readChar()
			return s.createTokenFromLocationPoint(TokenModAssign, start)
		}
		return s.createTokenFromLocationPoint(TokenMod, start)

	case ':':
		s.readChar()
		if s.currentChar() == '=' {
			s.readChar()
			return s.createTokenFromLocationPoint(TokenColonAssign, start)
		}
		return s.createTokenFromLocationPoint(TokenColon, start)

	case '&':
		s.readChar()
		if s.currentChar() == '&' {
			s.readChar()
			return s.createTokenFromLocationPoint(TokenLAnd, start)
		} else if s.currentChar() == '=' {
			s.readChar()
			return s.createTokenFromLocationPoint(TokenAndAssign, start)
		} else if s.currentChar() == '^' {
			s.readChar()
			if s.currentChar() == '=' {
				s.readChar()
				return s.createTokenFromLocationPoint(TokenAndNotAssign, start)
			}
			return s.createTokenFromLocationPoint(TokenAndNot, start)
		} else {
			return s.createTokenFromLocationPoint(TokenAnd, start)
		}

	case '|':
		s.readChar()
		if s.currentChar() == '|' {
			s.readChar()
			return s.createTokenFromLocationPoint(TokenLOr, start)
		} else if s.currentChar() == '=' {
			s.readChar()
			return s.createTokenFromLocationPoint(TokenOrAssign, start)
		}
		return s.createTokenFromLocationPoint(TokenOr, start)

	case '^':
		s.readChar()
		if s.currentChar() == '=' {
			s.readChar()
			return s.createTokenFromLocationPoint(TokenXorAssign, start)
		}
		return s.createTokenFromLocationPoint(TokenXor, start)

	case '`':
		insertSemi = true
		return s.readRawString()

	case '"':
		insertSemi = true
		return s.readString()

	default:
		// revert insertSemi
		insertSemi = s.insertSemi
		s.readChar()
		s.file.error(NewError(s.createSourceRange(start, s.currentLocation), "unknown token"))
		return s.createTokenFromLocationPoint(TokenInvalid, start)
	}
}
