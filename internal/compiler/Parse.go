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

func (p *Parser) invalidTokenAtEOF() Token {
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

func (p *Parser) lookahead(k int) Token {
	if p.currentTokenIndex+k < len(p.tokens) {
		return p.tokens[p.currentTokenIndex+k]
	}

	return p.invalidTokenAtEOF()
}

func (p *Parser) currentToken() Token {
	return p.lookahead(0)
}

func (p *Parser) eatToken() Token {
	if p.currentTokenIndex >= len(p.tokens) {
		return p.invalidTokenAtEOF()
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
	return tkn
}

func (p *Parser) ParseExpr() Expr {
	return p.parseBinaryExpr()
}

func (p *Parser) parseBinaryExpr() Expr {
	var precedences = [][]TokenKind{
		{TokenLOr},
		{TokenLAnd},
		{TokenLT, TokenGT, TokenLE, TokenGE, TokenEQ, TokenNE},
		{TokenAdd, TokenSub, TokenXor, TokenOr},
		{TokenMul, TokenDiv, TokenMod, TokenAnd, TokenShl, TokenShr},
	}
	return p.parseBinaryExprWithPrecedenceLevels(precedences)
}

func (p *Parser) parseBinaryExprWithPrecedenceLevels(levels [][]TokenKind) Expr {
	if len(levels) == 0 {
		return p.parseUnaryExpr()
	}

	expr := p.parseBinaryExprWithPrecedenceLevels(levels[1:])
	for slices.Contains(levels[0], p.currentToken().Kind()) {
		op := p.eatToken()
		rhs := p.parseBinaryExprWithPrecedenceLevels(levels[1:])
		if rhs == nil {
			p.file.errorf(op.SourceRange(), "missing right handside")
			break
		}
		expr = &BinaryExpr{
			LHS:      expr,
			Operator: op,
			RHS:      rhs,
		}
	}
	return expr
}

func (p *Parser) parseUnaryExpr() Expr {
	switch p.currentToken().Kind() {
	case TokenAdd, TokenSub, TokenNot, TokenXor:
		return &UnaryExpr{
			Operator: p.eatToken(),
			Base:     p.parseUnaryExpr(),
		}
	default:
		return p.parseBaseExpr()
	}
}

func (p *Parser) parseBaseExpr() Expr {
	expr := p.parseAtom()
	for {
		switch p.currentToken().Kind() {
		case TokenDot:
			if selector := p.parseSelectorExpr(expr); selector != nil {
				expr = selector
			}
		case TokenLBracket:
			if index := p.parseIndexExpr(expr); index != nil {
				expr = index
			}
		case TokenLParen:
			if call := p.parseCallExpr(expr); call != nil {
				expr = call
			}
		case TokenLBrace:
			t := p.convertParsedExprToType(expr)
			if t == nil {
				p.file.errorf(expr.SourceRange(), "failed to parse type")
				return nil
			}
			if complit := p.parseComplitExpr(t); complit != nil {
				expr = complit
			}
		default:
			return expr
		}
	}
}

func (p *Parser) parseSelectorExpr(base Expr) *SelectorExpr {
	dot := p.eatTokenOrError(TokenDot)
	if !dot.valid() {
		return nil
	}

	return &SelectorExpr{
		Base:     base,
		Selector: p.parseIdentifierExpr(),
	}
}

func (p *Parser) parseIdentifierExpr() *IdentifierExpr {
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

func (p *Parser) parseIndexExpr(base Expr) *IndexExpr {
	lBracket := p.eatTokenOrError(TokenLBracket)
	if !lBracket.valid() {
		return nil
	}

	index := p.ParseExpr()
	if index == nil {
		return nil
	}

	rBracket := p.eatTokenOrError(TokenRBracket)
	if !rBracket.valid() {
		return nil
	}

	return &IndexExpr{
		Base:     base,
		LBracket: lBracket,
		Index:    index,
		RBracket: rBracket,
	}
}

func (p *Parser) parseCallExpr(base Expr) *CallExpr {
	lParen, args, rParen, ok := p.parseExprList()
	if !ok {
		return nil
	}

	return &CallExpr{
		Base:   base,
		LParen: lParen,
		Args:   args,
		RParen: rParen,
	}
}

func (p *Parser) parseExprList() (lParen Token, exprs []Expr, rParen Token, ok bool) {
	lParen = p.eatTokenIfKind(TokenLParen)
	if !lParen.valid() {
		ok = false
		return
	}

	for p.currentToken().valid() && p.currentToken().Kind() != TokenRParen {
		e := p.ParseExpr()
		if e == nil {
			ok = false
			return
		}
		exprs = append(exprs, e)
		p.eatTokenIfKind(TokenComma)
	}

	rParen = p.eatTokenOrError(TokenRParen)
	return lParen, exprs, rParen, true
}

func (p *Parser) parseAtom() Expr {
	switch p.currentToken().Kind() {
	case TokenLiteralInt, TokenLiteralFloat, TokenLiteralString, TokenTrue, TokenFalse:
		return p.parseLiteralExpr()
	case TokenIdentifier:
		return p.parseIdentifierExpr()
	case TokenLParen:
		return p.parseParenExpr()
	default:
		p.file.errorf(p.currentToken().SourceRange(), "expected an expression but found '%v'", p.currentToken())
	}
	return nil
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

func (p *Parser) parseParenExpr() *ParenExpr {
	switch p.currentToken().Kind() {
	case TokenLParen:
		return &ParenExpr{
			Lparen: p.eatToken(),
			Base:   p.ParseExpr(),
			Rparen: p.eatTokenOrError(TokenRParen),
		}
	default:
		p.file.errorf(p.currentToken().SourceRange(), "expected a left parenthesis but found '%v'", p.currentToken())
	}
	return nil
}

func (p *Parser) convertParsedExprToType(e Expr) Type {
	switch n := e.(type) {
	case *IdentifierExpr:
		return &NamedType{
			TypeName: n.Token,
		}
	case *SelectorExpr:
		if base, ok := n.Base.(*IdentifierExpr); ok {
			return &NamedType{
				Package:  base.Token,
				TypeName: n.Selector.Token,
			}
		}
	}
	return nil
}

func (p *Parser) parseComplitExpr(t Type) *ComplitExpr {
	lBrace := p.eatTokenOrError(TokenLBrace)
	if !lBrace.valid() {
		return nil
	}

	var elements []ComplitElement
	for p.currentToken().valid() && p.currentToken().Kind() != TokenRBrace {
		var element ComplitElement
		element.Name = p.ParseExpr()
		if element.Name == nil {
			return nil
		}

		if colon := p.eatTokenIfKind(TokenColon); colon.valid() {
			element.Colon = colon

			element.Value = p.ParseExpr()
			if element.Value == nil {
				return nil
			}
		} else {
			element.Value = element.Name
			element.Name = nil
		}

		p.eatTokenIfKind(TokenComma)

		elements = append(elements, element)
	}

	rBrace := p.eatTokenOrError(TokenRBrace)
	if !rBrace.valid() {
		return nil
	}

	return &ComplitExpr{
		Type:     t,
		LBrace:   lBrace,
		Elements: elements,
		RBrace:   rBrace,
	}
}

func (p *Parser) ParseStmt() Stmt {
	switch p.currentToken().Kind() {
	case TokenReturn:
		return p.parseReturnStmt()
	default:
		return p.parseExprStmt()
	}
}

func (p *Parser) parseExprStmt() *ExprStmt {
	expr := p.ParseExpr()
	if expr == nil {
		return nil
	}
	return &ExprStmt{
		Expr: expr,
	}
}

func (p *Parser) parseReturnStmt() *ReturnStmt {
	returnToken := p.eatTokenOrError(TokenReturn)
	if !returnToken.valid() {
		return nil
	}

	var exprs []Expr
	// TODO: Work on automatic semicolon placement/injection in token stream
	if p.currentToken().valid() {
		initialExpr := p.ParseExpr()
		if initialExpr == nil {
			return nil
		}
		exprs = append(exprs, initialExpr)
		for p.eatTokenIfKind(TokenComma).valid() {
			expr := p.ParseExpr()
			if expr == nil {
				return nil
			}
			exprs = append(exprs, expr)
		}
	}

	return &ReturnStmt{
		Return: returnToken,
		Exprs:  exprs,
	}
}
