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

	var invalidRange SourceRange
	if len(p.tokens) > 0 {
		loc := p.tokens[len(p.tokens)-1].SourceRange().End()
		invalidRange = NewSourceRange(loc, loc)
	}

	return Token{
		kind:        TokenInvalid,
		sourceRange: invalidRange,
	}
}

func (p *Parser) currentToken() Token {
	return p.lookahead(0)
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
	return p.parseBaseExpr()
}

func (p *Parser) parseAtom() Expr {
	switch p.currentToken().Kind() {
	case TokenLiteralInt:
		fallthrough
	case TokenLiteralFloat:
		fallthrough
	case TokenLiteralString:
		fallthrough
	case TokenTrue:
		fallthrough
	case TokenFalse:
		return p.parseLiteralExpr()
	case TokenIdentifier:
		return p.parseIdentiferExpr()
	case TokenLParen:
		return p.parseParenExpr()
	default:
		p.file.errorf(p.currentToken().SourceRange(), "expected and expression but found '%v'", p.currentToken())
	}
	return nil
}

func (p *Parser) parseBaseExpr() Expr {
	expr := p.parseAtom()
	for {
		switch p.currentToken().Kind() {
		case TokenDot:
			if selector := p.parseSelectorExpr(expr); selector != nil {
				expr = selector
			}
		default:
			return expr
		}
	}
}

func (p *Parser) parseLiteralExpr() *LiteralExpr {
	switch p.currentToken().Kind() {
	case TokenLiteralInt:
		fallthrough
	case TokenLiteralFloat:
		fallthrough
	case TokenLiteralString:
		fallthrough
	case TokenTrue:
		fallthrough
	case TokenFalse:
		return &LiteralExpr{
			Token: p.eatToken(),
		}
	default:
		p.file.errorf(p.currentToken().SourceRange(), "expected an expression but found '%v'", p.currentToken())
	}
	return nil
}

func (p *Parser) parseIdentiferExpr() *IdentifierExpr {
	switch p.currentToken().Kind() {
	case TokenIdentifier:
		return &IdentifierExpr{
			Token: p.eatToken(),
		}
	default:
		p.file.errorf(p.currentToken().SourceRange(), "expected an identifier but found '%v'", p.currentToken())
	}
	return nil
}

func (p *Parser) parseParenExpr() *ParenExpr {
	switch p.currentToken().Kind() {
	case TokenLParen:
		return &ParenExpr{
			Lparen: p.eatToken(),
			Base:   p.ParseExpr(),
			Rparen: p.eatTokenOrError(TokenRParen),
		}
	default:
		p.file.errorf(p.currentToken().SourceRange(), "expected an identifier but found '%v'", p.currentToken())
	}
	return nil
}

func (p *Parser) parseSelectorExpr(base Expr) *SelectorExpr {
	dot := p.eatTokenOrError(TokenDot)
	if !dot.valid() {
		return nil
	}

	return &SelectorExpr{
		Base:     base,
		Selector: p.parseIdentiferExpr(),
	}
}
