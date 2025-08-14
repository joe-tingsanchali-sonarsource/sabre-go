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
			return expr
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
	case TokenFunc:
		return p.parseFuncType()
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

	lBraceToken := p.eatTokenOrError(TokenLBrace)

	var fields []Field
	for p.currentToken().Kind() != TokenRBrace && p.currentToken().valid() {
		if f := p.parseFieldDecl(); f != nil {
			fields = append(fields, *f)
		} else {
			return nil
		}
	}

	rBraceToken := p.eatTokenOrError(TokenRBrace)

	return &StructType{
		Struct:    structToken,
		FieldList: FieldList{Open: lBraceToken, Fields: fields, Close: rBraceToken},
	}
}

// FieldDecl = (IdentifierList Type | EmbeddedField) [ Tag ] ";"
func (p *Parser) parseFieldDecl() *Field {
	// Implements: FieldDecl = (IdentifierList Type | EmbeddedField) [ Tag ] ';'
	// Start by parsing the first identifier (common prefix for both alternatives)
	firstIdent := p.parseIdentifierExpr()
	if firstIdent == nil {
		return nil
	}

	// Unified embedded field detection (qualified or unqualified).
	// If after first identifier we immediately see '.', ';', string tag, or '}', treat it as EmbeddedField.
	// '.' case will be consumed inside parseEmbeddedField.
	switch p.currentToken().Kind() {
	case TokenDot, TokenSemicolon, TokenLiteralString, TokenRBrace:
		embeddedType := p.parseEmbeddedField(firstIdent)
		if embeddedType == nil {
			return nil
		}
		tag := p.eatTokenIfKind(TokenLiteralString)
		p.eatTokenOrError(TokenSemicolon)
		return &Field{Type: embeddedType, Tag: tag}
	}

	// IdentifierList Type path
	names := p.parseIdentifierExprListWithFirstId(firstIdent)

	fieldType := p.parseType()
	if fieldType == nil {
		return nil
	}

	tag := p.eatTokenIfKind(TokenLiteralString)
	p.eatTokenOrError(TokenSemicolon)
	return &Field{Names: names, Type: fieldType, Tag: tag}
}

// EmbeddedField = TypeName ; (subset without pointer / type args).
func (p *Parser) parseEmbeddedField(first *IdentifierExpr) Type {
	return p.parseTypeNameWithFirstId(first)
}

func (p *Parser) parseFuncType() *FuncType {
	funcToken := p.eatTokenOrError(TokenFunc)
	if !funcToken.valid() {
		return nil
	}

	parameters, result := p.parseSignature()

	return &FuncType{
		Func:       funcToken,
		Parameters: parameters,
		Result:     result,
	}
}

func (p *Parser) parseSignature() (parameters *FieldList, result *FieldList) {
	parameters = p.parseParameters()
	result = p.parseResult()
	return
}

func (p *Parser) parseParameters() *FieldList {
	openToken := p.eatTokenOrError(TokenLParen)

	var fields []Field
	if p.currentToken().Kind() != TokenRParen {
		fields = p.parseParameterList()
		for p.eatTokenIfKind(TokenComma).valid() {
			fields = append(fields, p.parseParameterList()...)
		}
	}

	if len(fields) > 0 {
		named := 0
		for _, f := range fields {
			if len(f.Names) > 0 {
				named++
			} else if named > 0 {
				p.file.errorf(f.Type.SourceRange(), "missing parameter name")
				return nil
			}
		}
	}

	closeToken := p.eatTokenOrError(TokenRParen)

	return &FieldList{
		Open:   openToken,
		Fields: fields,
		Close:  closeToken,
	}
}

func (p *Parser) parseParameterList() (list []Field) {
	expr := p.tryParseIdentOrTypeExpr()
	if expr == nil {
		p.file.errorf(p.currentToken().SourceRange(), "expected an identifier but found '%v'", p.currentToken())
		return nil
	}

	list = p.parseParameterListWithFirstExpr(expr)
	return
}

func (p *Parser) parseParameterListWithFirstExpr(expr Expr) (list []Field) {
	if expr == nil {
		return nil
	}

	exprs := []Expr{expr}
	for p.eatTokenIfKind(TokenComma).valid() {
		if e := p.tryParseIdentOrTypeExpr(); e != nil {
			exprs = append(exprs, e)
		} else {
			p.file.errorf(p.currentToken().SourceRange(), "expected an identifier or type but found '%v'", p.currentToken())
			return nil
		}
	}

	if p.currentToken().Kind() != TokenRParen && p.currentToken().Kind() != TokenSemicolon {
		t := p.parseType()
		if t == nil {
			return nil
		}

		var names []*IdentifierExpr
		for _, e := range exprs {
			if n, ok := e.(*IdentifierExpr); ok {
				names = append(names, n)
			} else {
				p.file.errorf(e.SourceRange(), "missing parameter name")
				return nil
			}
		}

		list = []Field{{Names: names, Type: t}}
		return
	}

	for _, e := range exprs {
		list = append(list, Field{Type: p.convertParsedExprToType(e)})
	}
	return
}

func (p *Parser) parseResult() *FieldList {
	if p.currentToken().Kind() == TokenLParen {
		return p.parseParameters()
	}

	if t := p.convertParsedExprToType(p.tryParseIdentOrTypeExpr()); t != nil {
		return &FieldList{Fields: []Field{{Type: t}}}
	}

	return nil
}

func (p *Parser) tryParseIdentOrTypeExpr() Expr {
	switch p.currentToken().Kind() {
	case TokenIdentifier:
		expr := p.parseIdentifierExpr()
		if p.currentToken().Kind() == TokenDot {
			return p.parseTypeNameWithFirstId(expr)
		}
		return expr
	case TokenLBracket:
		return p.parseArrayType()
	case TokenStruct:
		return p.parseStructType()
	case TokenFunc:
		return p.parseFuncType()
	default:
		return nil
	}
}

func (p *Parser) parseType() Type {
	switch p.currentToken().Kind() {
	case TokenIdentifier:
		return p.parseTypeName()
	case TokenLBracket:
		return p.parseArrayType()
	case TokenStruct:
		return p.parseStructType()
	case TokenFunc:
		return p.parseFuncType()
	default:
		p.file.errorf(p.currentToken().SourceRange(), "expected type but found %v", p.currentToken())
		return nil
	}
}

// TypeName = identifier | identifier '.' identifier
func (p *Parser) parseTypeName() *NamedType {
	return p.parseTypeNameWithFirstId(p.parseIdentifierExpr())
}

func (p *Parser) parseTypeNameWithFirstId(firstId *IdentifierExpr) *NamedType {
	if firstId == nil {
		return nil
	}

	if p.eatTokenIfKind(TokenDot).valid() {
		selector := p.parseIdentifierExpr()
		if selector == nil {
			return nil
		}
		return &NamedType{Package: firstId.Token, TypeName: selector.Token}
	}

	return &NamedType{TypeName: firstId.Token}
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
	case *FuncType:
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
	case TokenSwitch:
		return p.parseSwitchStmt()
	case TokenContinue:
		return p.parseContinueStmt()
	case TokenLBrace:
		stmt := p.parseBlockStmt()
		p.eatTokenOrError(TokenSemicolon)
		return stmt
	case TokenIf:
		return p.parseIfStmt()
	case TokenFor:
		return p.parseForStmt()
	default:
		stmt, _ := p.parseSimpleStmt()
		p.eatTokenOrError(TokenSemicolon)
		return stmt
	}
}

func (p *Parser) parseSimpleStmt() (stmt Stmt, isRange bool) {
	exprs := p.parseExprList()
	if len(exprs) == 0 {
		return nil, false
	}

	switch p.currentToken().Kind() {
	// TODO: Add &^ and not assignment
	case TokenColonAssign, TokenAssign, TokenAddAssign, TokenSubAssign,
		TokenMulAssign, TokenDivAssign, TokenModAssign, TokenAndAssign,
		TokenOrAssign, TokenXorAssign, TokenShlAssign, TokenShrAssign:
		operator := p.eatToken()

		if p.currentToken().Kind() == TokenRange && (operator.Kind() == TokenColonAssign || operator.Kind() == TokenAssign) {
			rangeToken := p.eatToken()
			expr := p.ParseExpr()
			return &AssignStmt{
				LHS:      exprs,
				Operator: operator,
				RHS:      []Expr{&UnaryExpr{Operator: rangeToken, Base: expr}},
			}, true
		} else {
			return &AssignStmt{
				LHS:      exprs,
				Operator: operator,
				RHS:      p.parseExprList(),
			}, false
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
		}, false
	default:
		return &ExprStmt{
			Expr: expr,
		}, false
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

func (p *Parser) parseIdentifierExprList() []*IdentifierExpr {
	firstId := p.parseIdentifierExpr()
	if firstId == nil {
		return nil
	}
	return p.parseIdentifierExprListWithFirstId(firstId)
}

func (p *Parser) parseIdentifierExprListWithFirstId(firstIdentifier *IdentifierExpr) (list []*IdentifierExpr) {
	if firstIdentifier == nil {
		firstIdentifier = p.parseIdentifierExpr()
	}

	if firstIdentifier == nil {
		return nil
	}
	list = append(list, firstIdentifier)
	for p.eatTokenIfKind(TokenComma).valid() {
		id := p.parseIdentifierExpr()
		if id == nil {
			return nil
		}
		list = append(list, id)
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

func (p *Parser) parseSwitchCaseStmt() *SwitchCaseStmt {
	caseToken := p.eatTokenIfKind(TokenCase)

	var lhs []Expr
	if caseToken.valid() {
		lhs = p.parseExprList()
	} else {
		caseToken = p.eatTokenOrError(TokenDefault)
		if !caseToken.valid() {
			return nil
		}
	}

	colonToken := p.eatTokenOrError(TokenColon)
	if !colonToken.valid() {
		return nil
	}

	rhs := p.parseStmtList()

	return &SwitchCaseStmt{
		Case:  caseToken,
		LHS:   lhs,
		Colon: colonToken,
		RHS:   rhs,
	}
}

func (p *Parser) parseSwitchStmt() *SwitchStmt {
	switchToken := p.eatTokenOrError(TokenSwitch)
	if !switchToken.valid() {
		return nil
	}

	exprLevel := p.pushExprLevelAsControlStmt()
	defer p.popExprLevelFromControlStmt(exprLevel)

	var init Stmt = nil
	if p.currentToken().Kind() != TokenLBrace {
		init, _ = p.parseSimpleStmt()
	}

	var tag Expr = nil
	if exprStmt, ok := init.(*ExprStmt); ok {
		tag = exprStmt.Expr
		init = nil
	} else {
		if init != nil {
			p.eatTokenOrError(TokenSemicolon)
		}

		if p.currentToken().Kind() != TokenLBrace {
			tag = p.ParseExpr()
			p.eatTokenIfKind(TokenSemicolon)
		}
	}

	lBraceToken := p.eatTokenOrError(TokenLBrace)
	var list []Stmt
	for p.currentToken().Kind() == TokenCase || p.currentToken().Kind() == TokenDefault {
		list = append(list, p.parseSwitchCaseStmt())
	}
	rBraceToken := p.eatTokenOrError(TokenRBrace)
	p.eatTokenOrError(TokenSemicolon)

	return &SwitchStmt{
		Switch: switchToken,
		Init:   init,
		Tag:    tag,
		Body:   &BlockStmt{LBrace: lBraceToken, Stmts: list, RBrace: rBraceToken},
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
	for p.currentToken().Kind() != TokenRBrace && p.currentToken().Kind() != TokenCase && p.currentToken().Kind() != TokenDefault && p.currentToken().valid() {
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
		init, _ = p.parseSimpleStmt()
	}

	var condStmt Stmt
	if p.currentToken().Kind() != TokenLBrace {
		p.eatTokenOrError(TokenSemicolon)
		if p.currentToken().Kind() != TokenLBrace {
			condStmt, _ = p.parseSimpleStmt()
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

func (p *Parser) parseForStmt() Stmt {
	forToken := p.eatTokenOrError(TokenFor)
	if !forToken.valid() {
		return nil
	}

	exprLevel := p.pushExprLevelAsControlStmt()
	defer p.popExprLevelFromControlStmt(exprLevel)

	// for {}
	if p.currentToken().Kind() == TokenLBrace {
		return &ForStmt{
			For:  forToken,
			Body: p.parseBlockStmt(),
		}
	}

	// for range list {}
	if rangeToken := p.eatTokenIfKind(TokenRange); rangeToken.valid() {
		return &ForRangeStmt{
			For:   forToken,
			Range: rangeToken,
			Expr:  p.ParseExpr(),
			Body:  p.parseBlockStmt(),
		}
	}

	var init Stmt
	if p.currentToken().Kind() != TokenSemicolon {
		cond, isRange := p.parseSimpleStmt()
		if cond != nil {
			// for cond {}
			if exprStmt, ok := cond.(*ExprStmt); ok {
				return &ForStmt{
					For:  forToken,
					Cond: exprStmt.Expr,
					Body: p.parseBlockStmt(),
				}
				// for i, [_] := range 10 {}
			} else if assignStmt, ok := cond.(*AssignStmt); ok && isRange {
				switch len(assignStmt.LHS) {
				case 0:
					// nothing to do
				case 1:
					// nothing to do
				case 2:
					// nothing to do
				default:
					p.file.errorf(assignStmt.LHS[len(assignStmt.LHS)-1].SourceRange(), "expected at most two iteration variables in range for statement")
					return nil
				}

				rangeExpr := assignStmt.RHS[0].(*UnaryExpr)

				return &ForRangeStmt{
					For:   forToken,
					Init:  assignStmt,
					Range: rangeExpr.Operator,
					Expr:  rangeExpr.Base,
					Body:  p.parseBlockStmt(),
				}
			}

			init = cond
		}
	}

	// for [init]; [cond]; [post] {}
	p.eatTokenOrError(TokenSemicolon)

	var cond Expr
	if p.currentToken().Kind() != TokenSemicolon {
		cond = p.ParseExpr()
	}

	p.eatTokenOrError(TokenSemicolon)

	var post Stmt
	if p.currentToken().Kind() != TokenLBrace {
		postStmt, _ := p.parseSimpleStmt()
		post = postStmt
	}

	return &ForStmt{
		For:  forToken,
		Init: init,
		Cond: cond,
		Post: post,
		Body: p.parseBlockStmt(),
	}
}

func (p *Parser) ParseDecl() Decl {
	switch p.currentToken().Kind() {
	case TokenType:
		return p.parseGenericDecl(p.eatToken(), p.parseTypeSpec)
	case TokenConst:
		return p.parseGenericDecl(p.eatToken(), p.parseConstSpec)
	case TokenVar:
		return p.parseGenericDecl(p.eatToken(), p.parseVarSpec)
	case TokenFunc:
		return p.parseFuncDecl()
	default:
		p.file.errorf(p.currentToken().SourceRange(), "unexpected declaration")
		return nil
	}
}

func (p *Parser) parseGenericDecl(token Token, parseFunc func() Spec) *GenericDecl {
	if lParen := p.eatTokenIfKind(TokenLParen); lParen.valid() {
		var list []Spec
		for p.currentToken().Kind() != TokenRParen && p.currentToken().valid() {
			s := parseFunc()
			if s == nil {
				return nil
			}
			list = append(list, s)
		}
		rParen := p.eatTokenOrError(TokenRParen)
		p.eatTokenOrError(TokenSemicolon)
		return &GenericDecl{
			DeclToken: token,
			LParen:    lParen,
			Specs:     list,
			RParen:    rParen,
		}
	} else {
		s := parseFunc()
		if s == nil {
			return nil
		}
		return &GenericDecl{
			DeclToken: token,
			Specs:     []Spec{s},
		}
	}
}

func (p *Parser) parseTypeSpec() Spec {
	name := p.parseIdentifierExpr()
	if name == nil {
		return nil
	}

	assign := p.eatTokenIfKind(TokenAssign)

	t := p.parseType()
	if t == nil {
		return nil
	}

	p.eatTokenOrError(TokenSemicolon)

	return &TypeSpec{
		Name:   name,
		Assign: assign,
		Type:   t,
	}
}

func (p *Parser) parseConstSpec() Spec {
	lhs := p.parseIdentifierExprList()

	var constType Type
	if p.currentToken().Kind() != TokenAssign && p.currentToken().Kind() != TokenSemicolon {
		constType = p.parseType()
	}

	assignToken := p.eatTokenIfKind(TokenAssign)

	if constType != nil && !assignToken.valid() {
		p.file.errorf(p.currentToken().SourceRange(), "constant declaration must have an init value")
		return nil
	}

	var rhs []Expr
	if assignToken.valid() {
		rhs = p.parseExprList()
		if len(rhs) == 0 {
			return nil
		}
	}

	p.eatTokenOrError(TokenSemicolon)

	return &ValueSpec{
		LHS:    lhs,
		Type:   constType,
		Assign: assignToken,
		RHS:    rhs,
	}
}

func (p *Parser) parseVarSpec() Spec {
	lhs := p.parseIdentifierExprList()

	var varType Type
	if p.currentToken().Kind() != TokenAssign {
		varType = p.parseType()
	}

	assignToken := p.eatTokenIfKind(TokenAssign)

	var rhs []Expr
	if assignToken.valid() {
		rhs = p.parseExprList()
		if len(rhs) == 0 {
			return nil
		}
	}

	p.eatTokenOrError(TokenSemicolon)

	return &ValueSpec{
		LHS:    lhs,
		Type:   varType,
		Assign: assignToken,
		RHS:    rhs,
	}
}

func (p *Parser) parseFuncDecl() *FuncDecl {
	funcToken := p.eatTokenOrError(TokenFunc)
	if !funcToken.valid() {
		return nil
	}

	var receiver *FieldList
	if p.currentToken().Kind() == TokenLParen {
		receiver = p.parseParameters()
		if receiver != nil && len(receiver.Fields) > 1 {
			p.file.errorf(receiver.Open.SourceRange().Merge(receiver.Close.SourceRange()), "method is expected to have only one receiver")
			return nil
		}
	}

	name := p.parseIdentifierExpr()

	parameters, result := p.parseSignature()

	var body *BlockStmt
	if p.currentToken().Kind() == TokenLBrace {
		body = p.parseBlockStmt()
	}

	return &FuncDecl{
		Receiver: receiver,
		Name:     name,
		Type: &FuncType{
			Func:       funcToken,
			Parameters: parameters,
			Result:     result,
		},
		Body: body,
	}
}
