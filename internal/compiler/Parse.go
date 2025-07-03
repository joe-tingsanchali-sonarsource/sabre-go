package compiler

import (
	"slices"
)

type Parser struct {
	file              *UnitFile
	tokens            []Token
	currentTokenIndex int
}

func NewParser(file *UnitFile) *Parser {
	parser := &Parser{
		file: file,
		tokens: slices.DeleteFunc(slices.Clone(file.tokens), func(t Token) bool {
			return t.Kind() == TokenComment || t.Kind() == TokenEOF || t.Kind() == TokenInvalid
		}),
		currentTokenIndex: 0,
	}

	return parser
}

func (t Token) valid() bool {
	return t.kind != TokenEOF && t.kind != TokenInvalid
}

func (p *Parser) lookahead(k int) Token {
	if p.currentTokenIndex+k < len(p.tokens) {
		return p.tokens[p.currentTokenIndex+k]
	}

	return Token{
		kind: TokenInvalid,
	}
}

func (p *Parser) currentToken() Token {
	return p.tokens[p.currentTokenIndex]
}

func (p *Parser) eatToken() Token {
	if p.currentTokenIndex >= len(p.tokens) {
		return Token{
			kind: TokenInvalid,
		}
	}
	tkn := p.tokens[p.currentTokenIndex]
	p.currentTokenIndex++
	return tkn
}

func (p *Parser) eatTokenIfKind(kind TokenKind) Token {
	if p.currentToken().Kind() == kind {
		return p.eatToken()
	}
	return Token{
		kind: TokenInvalid,
	}
}

func (p *Parser) eatTokenOrError(kind TokenKind) Token {
	tkn := p.eatToken()
	if tkn.Kind() == kind {
		return tkn
	}
	p.file.errorf(tkn.SourceRange(), "expected '%v' but found '%v'", kind, tkn.Kind())
	return Token{
		kind: TokenInvalid,
	}
}

func (p *Parser) ParseExpr() Expr {
	return p.parseLiteralExpr()
}

func (p *Parser) parseLiteralExpr() *LiteralExpr {
	switch p.currentToken().Kind() {
	case TokenLiteralInt:
	case TokenLiteralFloat:
	case TokenLiteralString:
	case TokenTrue:
	case TokenFalse:
		return &LiteralExpr{
			Token: p.eatToken(),
		}
	}
	return nil
}
