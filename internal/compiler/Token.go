package compiler

import (
	"fmt"
	"strings"
)

// TokenKind represents the type of a token
type TokenKind int

const (
	// Special tokens
	TokenInvalid TokenKind = iota
	TokenEOF
	TokenComment

	// Identifiers and literals
	TokenIdentifier
	TokenLiteralInt
	TokenLiteralFloat
	TokenLiteralString

	// Delimiters
	TokenLParen    // (
	TokenRParen    // )
	TokenLBrace    // {
	TokenRBrace    // }
	TokenLBracket  // [
	TokenRBracket  // ]
	TokenSemicolon // ;
	TokenDot       // .
	TokenComma     // ,

	// Operators
	TokenLT        // <
	TokenGT        // >
	TokenLE        // <=
	TokenGE        // >=
	TokenAssign    // =
	TokenEQ        // ==
	TokenNE        // !=
	TokenAdd       // +
	TokenSub       // -
	TokenMul       // *
	TokenDiv       // /
	TokenMod       // %
	TokenAddAssign // +=
	TokenSubAssign // -=
	TokenMulAssign // *=
	TokenDivAssign // /=
	TokenModAssign // %=
	TokenLOr       // ||
	TokenLAnd      // &&
	TokenNot       // !
	TokenXor       // ^
	TokenOr        // |
	TokenAnd       // &
	TokenShl       // <<
	TokenShr       // >>
	TokenShlAssign // <<=
	TokenShrAssign // >>=
	TokenXorAssign // ^=
	TokenOrAssign  // |=
	TokenAndAssign // &=
	TokenInc       // ++
	TokenDec       // --

	// Keywords (Go keywords that make sense in shader context)
	TokenConst
	TokenVar
	TokenType
	TokenStruct
	TokenFunc
	TokenReturn
	TokenContinue
	TokenBreak
	TokenImport
	TokenIf
	TokenElse
	TokenFor
	TokenFalse
	TokenTrue
	TokenPackage

	// Shader-specific keywords (not in Go)
	TokenDiscard // shader-specific discard statement

	// Token category markers for easier categorization
	// Literal boundaries
	TokenLiteralBegin = TokenLiteralInt
	TokenLiteralEnd   = TokenLiteralString

	// Delimiter boundaries
	TokenDelimiterBegin = TokenLParen
	TokenDelimiterEnd   = TokenComma

	// Operator boundaries
	TokenOperatorBegin = TokenLT
	TokenOperatorEnd   = TokenDec

	// Keyword boundaries
	TokenKeywordBegin = TokenConst
	TokenKeywordEnd   = TokenDiscard
)

// String returns the string representation of a TokenKind
func (tk TokenKind) String() string {
	switch tk {
	case TokenInvalid:
		return "INVALID"
	case TokenEOF:
		return "EOF"
	case TokenComment:
		return "COMMENT"
	case TokenIdentifier:
		return "IDENTIFIER"
	case TokenLiteralInt:
		return "LITERAL_INT"
	case TokenLiteralFloat:
		return "LITERAL_FLOAT"
	case TokenLiteralString:
		return "LITERAL_STRING"
	case TokenLParen:
		return "("
	case TokenRParen:
		return ")"
	case TokenLBrace:
		return "{"
	case TokenRBrace:
		return "}"
	case TokenLBracket:
		return "["
	case TokenRBracket:
		return "]"
	case TokenSemicolon:
		return ";"
	case TokenDot:
		return "."
	case TokenComma:
		return ","
	case TokenLT:
		return "<"
	case TokenGT:
		return ">"
	case TokenLE:
		return "<="
	case TokenGE:
		return ">="
	case TokenAssign:
		return "="
	case TokenEQ:
		return "=="
	case TokenNE:
		return "!="
	case TokenAdd:
		return "+"
	case TokenSub:
		return "-"
	case TokenMul:
		return "*"
	case TokenDiv:
		return "/"
	case TokenMod:
		return "%"
	case TokenAddAssign:
		return "+="
	case TokenSubAssign:
		return "-="
	case TokenMulAssign:
		return "*="
	case TokenDivAssign:
		return "/="
	case TokenModAssign:
		return "%="
	case TokenLOr:
		return "||"
	case TokenLAnd:
		return "&&"
	case TokenNot:
		return "!"
	case TokenXor:
		return "^"
	case TokenOr:
		return "|"
	case TokenAnd:
		return "&"
	case TokenShl:
		return "<<"
	case TokenShr:
		return ">>"
	case TokenShlAssign:
		return "<<="
	case TokenShrAssign:
		return ">>="
	case TokenXorAssign:
		return "^="
	case TokenOrAssign:
		return "|="
	case TokenAndAssign:
		return "&="
	case TokenInc:
		return "++"
	case TokenDec:
		return "--"
	case TokenConst:
		return "const"
	case TokenVar:
		return "var"
	case TokenType:
		return "type"
	case TokenStruct:
		return "struct"
	case TokenFunc:
		return "func"
	case TokenReturn:
		return "return"
	case TokenContinue:
		return "continue"
	case TokenBreak:
		return "break"
	case TokenImport:
		return "import"
	case TokenIf:
		return "if"
	case TokenElse:
		return "else"
	case TokenFor:
		return "for"
	case TokenFalse:
		return "false"
	case TokenTrue:
		return "true"
	case TokenPackage:
		return "package"
	case TokenDiscard:
		return "discard"
	default:
		return fmt.Sprintf("UNKNOWN(%d)", int(tk))
	}
}

// Token represents a single token in the source code
type Token struct {
	kind     TokenKind
	value    string
	location Location
}

// Kind returns the token's kind/type
func (t Token) Kind() TokenKind {
	return t.kind
}

// Value returns the token's string value
func (t Token) Value() string {
	return t.value
}

// Location returns the token's location in the source
func (t Token) Location() Location {
	return t.location
}

// String returns a string representation of the token
func (t Token) String() string {
	if t.value != "" && t.value != t.kind.String() {
		return fmt.Sprintf("%s(%s)", t.kind.String(), t.value)
	}
	return t.kind.String()
}

// IsKeyword returns true if the token is a keyword
func (t Token) IsKeyword() bool {
	return t.kind >= TokenKeywordBegin && t.kind <= TokenKeywordEnd
}

// IsLiteral returns true if the token is a literal value
func (t Token) IsLiteral() bool {
	return t.kind >= TokenLiteralBegin && t.kind <= TokenLiteralEnd
}

// IsOperator returns true if the token is an operator
func (t Token) IsOperator() bool {
	return t.kind >= TokenOperatorBegin && t.kind <= TokenOperatorEnd
}

// IsDelimiter returns true if the token is a delimiter
func (t Token) IsDelimiter() bool {
	return t.kind >= TokenDelimiterBegin && t.kind <= TokenDelimiterEnd
}

type Position struct {
	Line, Column int32
}

func (p Position) String() string {
	return fmt.Sprintf("%v:%v", p.Line, p.Column)
}

type Range struct {
	Begin, End int32
}

type Location struct {
	Position Position
	Range    Range
	File     *UnitFile
}

func (loc Location) HighlightCodeRange() string {
	if loc.File == nil || int(loc.Position.Line) > len(loc.File.lines) || loc.Position.Line < 1 {
		return ""
	}

	line := loc.File.lines[loc.Position.Line-1] // Convert to 0-based index

	// Build the highlight string
	var result strings.Builder
	result.WriteString(">> \t")
	result.WriteString(line)
	result.WriteString("\n")
	result.WriteString(">> \t")

	// Add spaces up to the beginning of the range
	for i := int32(0); i < loc.Range.Begin; i++ {
		if i < int32(len(line)) && line[i] == '\t' {
			result.WriteString("\t")
		} else {
			result.WriteString(" ")
		}
	}

	// Add carets for the range
	rangeLength := loc.Range.End - loc.Range.Begin
	if rangeLength <= 0 {
		rangeLength = 1 // At least one caret
	}
	result.WriteString(strings.Repeat("^", int(rangeLength)))

	return result.String()
}

func (loc Location) String() string {
	if loc.File != nil {
		return fmt.Sprintf("%v:%v", loc.File.path, loc.Position)
	}
	return fmt.Sprintf("<nil>:%v", loc.Position)
}
