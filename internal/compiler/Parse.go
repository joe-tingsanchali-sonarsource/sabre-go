package compiler

import (
	"slices"
)

type Parser struct {
	file              *UnitFile
	tokens            []Token
	currentTokenIndex int
	exprLevel         int
}

func NewParser(file *UnitFile) *Parser {
	parser := &Parser{
		file: file,
		tokens: slices.DeleteFunc(slices.Clone(file.tokens), func(t Token) bool {
			return t.Kind() == TokenComment || t.Kind() == TokenEOF || t.Kind() == TokenInvalid
		}),
		currentTokenIndex: 0,
		exprLevel:         0,
	}

	return parser
}

func (t Token) valid() bool {
	return t.kind != TokenEOF && t.kind != TokenInvalid
}

func (p *Parser) isInExpr() bool {
	return p.exprLevel >= 0
}

func (p *Parser) isInControlStmt() bool {
	return !p.isInExpr()
}

func (p *Parser) pushExprLevel() {
	p.exprLevel++
}

func (p *Parser) popExprLevel() {
	p.exprLevel--
}

func (p *Parser) pushExprLevelAsControlStmt() (prev int) {
	prev = p.exprLevel
	p.exprLevel = -1
	return
}

func (p *Parser) popExprLevelFromControlStmt(prev int) {
	p.exprLevel = prev
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
			if p.isInControlStmt() {
				return expr
			} else {
				t := p.convertParsedExprToType(expr)
				if t == nil {
					p.file.errorf(expr.SourceRange(), "failed to parse type")
					return nil
				}
				if complit := p.parseComplitExpr(t); complit != nil {
					expr = complit
				}
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
	p.pushExprLevel()
	defer p.popExprLevel()

	lParen, args, rParen, ok := p.parseArgList()
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

func (p *Parser) parseArgList() (lParen Token, exprs []Expr, rParen Token, ok bool) {
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
	case TokenLBracket:
		return p.parseArrayType()
	case TokenStruct:
		return p.parseStructType()
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
		p.pushExprLevel()
		defer p.popExprLevel()
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

func (p *Parser) parseArrayType() *ArrayType {
	lBracket := p.eatTokenOrError(TokenLBracket)
	if !lBracket.valid() {
		return nil
	}

	var length Expr
	if p.currentToken().Kind() != TokenRBracket {
		length = p.ParseExpr()
		if length == nil {
			return nil
		}
	}

	rBracket := p.eatTokenOrError(TokenRBracket)
	if !rBracket.valid() {
		return nil
	}

	elementType := p.parseType()
	if elementType == nil {
		return nil
	}

	return &ArrayType{
		LBracket:    lBracket,
		Length:      length,
		RBracket:    rBracket,
		ElementType: elementType,
	}
}

func (p *Parser) parseStructType() *StructType {
	structToken := p.eatTokenOrError(TokenStruct)
	if !structToken.valid() {
		return nil
	}

	p.eatTokenOrError(TokenLBrace)

	var fields []StructTypeField
	for p.currentToken().Kind() != TokenRBrace && p.currentToken().valid() {
		names := []*IdentifierExpr{p.parseIdentifierExpr()}
		for p.currentToken().Kind() == TokenComma {
			p.eatToken()
			names = append(names, p.parseIdentifierExpr())
		}

		fieldType := p.parseType()
		if fieldType == nil {
			return nil
		}

		var tag Token
		if p.currentToken().Kind() == TokenLiteralString {
			tag = p.eatToken()
		}

		p.eatTokenOrError(TokenSemicolon)

		fields = append(fields, StructTypeField{Names: names, Type: fieldType, Tag: tag})
	}

	p.eatTokenOrError(TokenRBrace)

	return &StructType{
		Struct: structToken,
		Fields: fields,
	}
}

func (p *Parser) parseType() Type {
	switch p.currentToken().Kind() {
	case TokenIdentifier:
		identifier := p.parseIdentifierExpr()
		if identifier == nil {
			return nil
		}
		if p.eatTokenIfKind(TokenDot).valid() {
			selector := p.parseIdentifierExpr()
			if selector == nil {
				return nil
			}
			return &NamedType{
				Package:  identifier.Token,
				TypeName: selector.Token,
			}
		} else {
			return &NamedType{
				TypeName: identifier.Token,
			}
		}
	case TokenLBracket:
		return p.parseArrayType()
	case TokenStruct:
		return p.parseStructType()
	default:
		p.file.errorf(p.currentToken().SourceRange(), "expected type but found %v", p.currentToken())
		return nil
	}
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
	case *NamedType:
		return n
	case *ArrayType:
		return n
	case *StructType:
		return n
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
	case TokenBreak:
		return p.parseBreakStmt()
	case TokenFallthrough:
		return p.parseFallthroughStmt()
	case TokenContinue:
		return p.parseContinueStmt()
	case TokenLBrace:
		stmt := p.parseBlockStmt()
		p.eatTokenOrError(TokenSemicolon)
		return stmt
	case TokenIf:
		stmt := p.parseIfStmt()
		return stmt
	default:
		stmt := p.parseSimpleStmt()
		p.eatTokenOrError(TokenSemicolon)
		return stmt
	}
}

func (p *Parser) parseSimpleStmt() Stmt {
	exprs := p.parseExprList()
	if len(exprs) == 0 {
		return nil
	}

	switch p.currentToken().Kind() {
	// TODO: Add &^ and not assignment
	case TokenColonAssign, TokenAssign, TokenAddAssign, TokenSubAssign,
		TokenMulAssign, TokenDivAssign, TokenModAssign, TokenAndAssign,
		TokenOrAssign, TokenXorAssign, TokenShlAssign, TokenShrAssign:
		operator := p.eatToken()
		rhs := p.parseExprList()
		return &AssignStmt{
			LHS:      exprs,
			Operator: operator,
			RHS:      rhs,
		}
	}

	if len(exprs) > 1 {
		p.file.errorf(exprs[0].SourceRange().Merge(exprs[len(exprs)-1].SourceRange()), "Expected 1 expression but found %v", len(exprs))
		// continue with first expression
	}

	expr := exprs[0]

	switch p.currentToken().Kind() {
	case TokenInc, TokenDec:
		op := p.eatToken()
		return &IncDecStmt{
			Expr:     expr,
			Operator: op,
		}
	default:
		return &ExprStmt{
			Expr: expr,
		}
	}
}

func (p *Parser) parseExprList() (list []Expr) {
	e := p.ParseExpr()
	if e == nil {
		return nil
	}

	list = append(list, e)
	for p.eatTokenIfKind(TokenComma).valid() {
		e = p.ParseExpr()
		if e == nil {
			return nil
		}
		list = append(list, e)
	}
	return
}

func (p *Parser) parseReturnStmt() *ReturnStmt {
	returnToken := p.eatTokenOrError(TokenReturn)
	if !returnToken.valid() {
		return nil
	}

	var exprs []Expr
	if p.currentToken().Kind() != TokenSemicolon && p.currentToken().Kind() != TokenRBrace {
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
	p.eatTokenOrError(TokenSemicolon)

	return &ReturnStmt{
		Return: returnToken,
		Exprs:  exprs,
	}
}

func (p *Parser) parseBreakStmt() *BreakStmt {
	breakToken := p.eatTokenOrError(TokenBreak)
	if !breakToken.valid() {
		return nil
	}

	labelToken := p.eatTokenIfKind(TokenIdentifier)

	p.eatTokenOrError(TokenSemicolon)

	return &BreakStmt{
		Break: breakToken,
		Label: labelToken,
	}
}

func (p *Parser) parseFallthroughStmt() *FallthroughStmt {
	fallthroughToken := p.eatTokenOrError(TokenFallthrough)
	if !fallthroughToken.valid() {
		return nil
	}

	p.eatTokenOrError(TokenSemicolon)

	return &FallthroughStmt{
		Fallthrough: fallthroughToken,
	}
}

func (p *Parser) parseContinueStmt() *ContinueStmt {
	continueToken := p.eatTokenOrError(TokenContinue)
	if !continueToken.valid() {
		return nil
	}

	labelToken := p.eatTokenIfKind(TokenIdentifier)

	p.eatTokenOrError(TokenSemicolon)

	return &ContinueStmt{
		Continue: continueToken,
		Label:    labelToken,
	}
}

func (p *Parser) parseBlockStmt() *BlockStmt {
	lBrace := p.eatTokenOrError(TokenLBrace)
	if !lBrace.valid() {
		return nil
	}

	stmts := p.parseStmtList()

	rBrace := p.eatTokenOrError(TokenRBrace)
	if !rBrace.valid() {
		return nil
	}

	return &BlockStmt{
		LBrace: lBrace,
		Stmts:  stmts,
		RBrace: rBrace,
	}
}

func (p *Parser) parseStmtList() []Stmt {
	var list []Stmt
	// TODO: Add != case and != default to this list when you work on switch
	for p.currentToken().Kind() != TokenRBrace && p.currentToken().valid() {
		list = append(list, p.ParseStmt())
	}
	return list
}

func (p *Parser) parseIfStmt() *IfStmt {
	ifToken := p.eatTokenOrError(TokenIf)
	if !ifToken.valid() {
		return nil
	}

	init, cond := p.parseIfHeader()
	if cond == nil {
		return nil
	}

	body := p.parseBlockStmt()
	if body == nil {
		return nil
	}

	var elseStmt Stmt
	if p.eatTokenIfKind(TokenElse).valid() {
		switch p.currentToken().Kind() {
		case TokenIf:
			elseStmt = p.parseIfStmt()
		case TokenLBrace:
			elseStmt = p.parseBlockStmt()
			p.eatTokenOrError(TokenSemicolon)
		default:
			p.file.errorf(p.currentToken().SourceRange(), "Expected if statement or block")
		}
	} else {
		p.eatTokenOrError(TokenSemicolon)
	}

	return &IfStmt{
		If:   ifToken,
		Init: init,
		Cond: cond,
		Body: body,
		Else: elseStmt,
	}
}

func (p *Parser) parseIfHeader() (init Stmt, cond Expr) {

	exprLevel := p.pushExprLevelAsControlStmt()
	defer p.popExprLevelFromControlStmt(exprLevel)

	// handle direct {
	if p.currentToken().Kind() != TokenLBrace && p.currentToken().Kind() != TokenSemicolon {
		init = p.parseSimpleStmt()
	}

	var condStmt Stmt
	if p.currentToken().Kind() != TokenLBrace {
		p.eatTokenOrError(TokenSemicolon)
		if p.currentToken().Kind() != TokenLBrace {
			condStmt = p.parseSimpleStmt()
		}
	} else {
		condStmt = init
		init = nil
	}

	if condStmt != nil {
		if exprStmt, ok := condStmt.(*ExprStmt); ok {
			cond = exprStmt.Expr
		} else {
			p.file.errorf(condStmt.SourceRange(), "Expected boolean expression as condition in if statement")
		}
	} else {
		p.file.errorf(p.currentToken().SourceRange(), "Missing condition in if statement")
	}

	return
}
