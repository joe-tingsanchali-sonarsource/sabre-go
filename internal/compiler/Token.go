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
	TokenColon     // :

	// Operators
	TokenLT          // <
	TokenGT          // >
	TokenLE          // <=
	TokenGE          // >=
	TokenAssign      // =
	TokenEQ          // ==
	TokenNE          // !=
	TokenAdd         // +
	TokenSub         // -
	TokenMul         // *
	TokenDiv         // /
	TokenMod         // %
	TokenAddAssign   // +=
	TokenSubAssign   // -=
	TokenMulAssign   // *=
	TokenDivAssign   // /=
	TokenModAssign   // %=
	TokenColonAssign // :=
	TokenLOr         // ||
	TokenLAnd        // &&
	TokenNot         // !
	TokenXor         // ^
	TokenOr          // |
	TokenAnd         // &
	TokenShl         // <<
	TokenShr         // >>
	TokenShlAssign   // <<=
	TokenShrAssign   // >>=
	TokenXorAssign   // ^=
	TokenOrAssign    // |=
	TokenAndAssign   // &=
	TokenInc         // ++
	TokenDec         // --

	// Keywords (Go keywords that make sense in shader context)
	TokenConst
	TokenVar
	TokenType
	TokenStruct
	TokenFunc
	TokenReturn
	TokenContinue
	TokenBreak
	TokenFallthrough
	TokenCase
	TokenDefault
	TokenSwitch
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
	TokenDelimiterEnd   = TokenColon

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
	case TokenColon:
		return ":"
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
	case TokenColonAssign:
		return ":="
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
	case TokenFallthrough:
		return "fallthrough"
	case TokenCase:
		return "case"
	case TokenDefault:
		return "default"
	case TokenSwitch:
		return "switch"
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
	kind        TokenKind
	value       string
	sourceRange SourceRange
}

// Kind returns the token's kind/type
func (t Token) Kind() TokenKind {
	return t.kind
}

// Value returns the token's string value
func (t Token) Value() string {
	return t.value
}

// SourceRange returns the token's location range in the source
func (t Token) SourceRange() SourceRange {
	return t.sourceRange
}

// String returns a string representation of the token
func (t Token) String() string {
	if t.value != "" && t.value != t.kind.String() {
		if t.value == "\n" {
			t.value = "\\n"
		}
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

// Location specification

type SourcePosition struct {
	Line, Column int32
}

func (p SourcePosition) String() string {
	return fmt.Sprintf("%v:%v", p.Line, p.Column)
}

type SourceLocation struct {
	Position    SourcePosition
	BeginOffset int32
	File        *UnitFile
}

func (loc SourceLocation) String() string {
	if loc.File != nil {
		return fmt.Sprintf("%v:%v", loc.File.path, loc.Position)
	}
	return fmt.Sprintf("<nil>:%v", loc.Position)
}

type SourceRange struct {
	BeginPosition, EndPosition SourcePosition
	BeginOffset, EndOffset     int32
	File                       *UnitFile
}

func NewSourceRange(a, b SourceLocation) SourceRange {
	if a.File != b.File {
		panic(fmt.Sprintf("source range locations have different files '%v', '%v'", a, b))
	}

	return SourceRange{
		BeginPosition: a.Position,
		EndPosition:   b.Position,
		BeginOffset:   a.BeginOffset,
		EndOffset:     b.BeginOffset,
		File:          a.File,
	}
}

func (r SourceRange) Begin() SourceLocation {
	return SourceLocation{
		Position:    r.BeginPosition,
		BeginOffset: r.BeginOffset,
		File:        r.File,
	}
}

func (r SourceRange) End() SourceLocation {
	return SourceLocation{
		Position:    r.EndPosition,
		BeginOffset: r.EndOffset,
		File:        r.File,
	}
}

func (r SourceRange) Merge(b SourceRange) SourceRange {
	begin := r.Begin()
	if begin.BeginOffset > b.Begin().BeginOffset {
		begin = b.Begin()
	}

	end := r.End()
	if end.BeginOffset < b.End().BeginOffset {
		end = b.End()
	}

	return NewSourceRange(begin, end)
}

func (r SourceRange) String() string {
	if r.File != nil {
		return fmt.Sprintf("%v:%v:%v", r.File.path, r.BeginPosition, r.EndPosition)
	}
	return fmt.Sprintf("<nil>:%v:%v", r.BeginPosition, r.EndPosition)
}

func (r SourceRange) isInside(offset int) bool {
	if r.BeginOffset == r.EndOffset {
		return offset == int(r.BeginOffset)
	} else {
		return offset >= int(r.BeginOffset) && offset < int(r.EndOffset)
	}
}

func (r SourceRange) highlightLine(builder *strings.Builder, line string, byteOffset int) int {
	builder.WriteString(">> \t")
	builder.WriteString(line)
	builder.WriteString("\n")
	builder.WriteString(">> \t")

	// +1 for the option to highlight end of file
	for i := 0; i < len(line)+1; i++ {
		if r.isInside(byteOffset + i) {
			builder.WriteByte('^')
		} else {
			if i < len(line) && line[i] == '\t' {
				builder.WriteByte('\t')
			} else {
				builder.WriteByte(' ')
			}
		}
	}
	return byteOffset + len(line) + 1 // +1 for the \n
}

func (r SourceRange) HighlightCodeRange() string {
	if r.File == nil || int(r.BeginPosition.Line) > len(r.File.lines) || r.BeginPosition.Line < 1 {
		return ""
	}

	lines := r.File.lines[r.BeginPosition.Line-1 : r.EndPosition.Line] // Convert to 0-based index
	byteOffset := 0
	for i := 0; i < int(r.BeginPosition.Line-1); i++ {
		byteOffset += len(r.File.lines[i]) + 1 // +1 for the \n
	}

	// Build the highlight string
	var result strings.Builder
	for i, line := range lines {
		if i > 0 {
			fmt.Fprint(&result, "\n")
		}
		byteOffset = r.highlightLine(&result, line, byteOffset)
	}

	return result.String()
}

// Error specification

type Error struct {
	SourceRange SourceRange
	Message     string
}

func (e Error) String() string {
	return fmt.Sprintf("%v\nError[%v]: %v", e.SourceRange.HighlightCodeRange(), e.SourceRange.Begin(), e.Message)
}
