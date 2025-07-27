package compiler

type Node interface {
	SourceRange() SourceRange
	Visit(NodeVisitor)
}

// Expr Nodes
type Expr interface {
	Node
	exprNode()
}

type LiteralExpr struct {
	Token Token
}

func (e *LiteralExpr) exprNode()                {}
func (e *LiteralExpr) SourceRange() SourceRange { return e.Token.SourceRange() }
func (e *LiteralExpr) Visit(v NodeVisitor) {
	v.VisitLiteralExpr(e)
}

type IdentifierExpr struct {
	Token Token
}

func (e *IdentifierExpr) exprNode()                {}
func (e *IdentifierExpr) SourceRange() SourceRange { return e.Token.SourceRange() }
func (e *IdentifierExpr) Visit(v NodeVisitor) {
	v.VisitIdentifierExpr(e)
}

type ParenExpr struct {
	Lparen Token
	Base   Expr
	Rparen Token
}

func (e *ParenExpr) exprNode() {}
func (e *ParenExpr) SourceRange() SourceRange {
	return e.Lparen.SourceRange().Merge(e.Rparen.SourceRange())
}
func (e *ParenExpr) Visit(v NodeVisitor) {
	v.VisitParenExpr(e)
}

type SelectorExpr struct {
	Base     Expr
	Selector *IdentifierExpr
}

func (e *SelectorExpr) exprNode() {}
func (e *SelectorExpr) SourceRange() SourceRange {
	return e.Base.SourceRange().Merge(e.Selector.SourceRange())
}
func (e *SelectorExpr) Visit(v NodeVisitor) {
	v.VisitSelectorExpr(e)
}

type IndexExpr struct {
	Base     Expr
	LBracket Token
	Index    Expr
	RBracket Token
}

func (e *IndexExpr) exprNode() {}
func (e *IndexExpr) SourceRange() SourceRange {
	return e.Base.SourceRange().Merge(e.RBracket.SourceRange())
}
func (e *IndexExpr) Visit(v NodeVisitor) {
	v.VisitIndexExpr(e)
}

type CallExpr struct {
	Base   Expr
	LParen Token
	Args   []Expr
	RParen Token
}

func (e *CallExpr) exprNode() {}
func (e *CallExpr) SourceRange() SourceRange {
	return e.Base.SourceRange().Merge(e.RParen.SourceRange())
}
func (e *CallExpr) Visit(v NodeVisitor) {
	v.VisitCallExpr(e)
}

type UnaryExpr struct {
	Operator Token
	Base     Expr
}

func (e *UnaryExpr) exprNode() {}
func (e *UnaryExpr) SourceRange() SourceRange {
	return e.Operator.SourceRange().Merge(e.Base.SourceRange())
}
func (e *UnaryExpr) Visit(v NodeVisitor) {
	v.VisitUnaryExpr(e)
}

type BinaryExpr struct {
	LHS      Expr
	Operator Token
	RHS      Expr
}

func (e *BinaryExpr) exprNode() {}
func (e *BinaryExpr) SourceRange() SourceRange {
	return e.LHS.SourceRange().Merge(e.RHS.SourceRange())
}
func (e *BinaryExpr) Visit(v NodeVisitor) {
	v.VisitBinaryExpr(e)
}

type ComplitElement struct {
	Name  Expr
	Colon Token
	Value Expr
}

type ComplitExpr struct {
	Type     Type
	LBrace   Token
	Elements []ComplitElement
	RBrace   Token
}

func (e *ComplitExpr) exprNode() {}
func (e *ComplitExpr) SourceRange() SourceRange {
	return e.Type.SourceRange().Merge(e.RBrace.SourceRange())
}
func (e *ComplitExpr) Visit(v NodeVisitor) {
	v.VisitComplitExpr(e)
}

// Type expressions
type Type interface {
	Expr
	typeExpr()
}

type NamedType struct {
	Package  Token
	TypeName Token
}

func (e *NamedType) IsPackageQualified() bool { return e.Package.Kind() == TokenIdentifier }
func (e *NamedType) exprNode()                {}
func (e *NamedType) SourceRange() SourceRange {
	if e.IsPackageQualified() {
		return e.Package.SourceRange().Merge(e.TypeName.SourceRange())
	} else {
		return e.TypeName.SourceRange()
	}
}
func (e *NamedType) Visit(v NodeVisitor) {
	v.VisitNamedType(e)
}
func (e *NamedType) typeExpr() {}

// Stmt Nodes
type Stmt interface {
	Node
	stmtNode()
}

type ExprStmt struct {
	Expr Expr
}

func (e *ExprStmt) stmtNode()                {}
func (e *ExprStmt) SourceRange() SourceRange { return e.Expr.SourceRange() }
func (e *ExprStmt) Visit(v NodeVisitor) {
	v.VisitExprStmt(e)
}

type ReturnStmt struct {
	Return Token
	Exprs  []Expr
}

func (e *ReturnStmt) stmtNode() {}
func (e *ReturnStmt) SourceRange() SourceRange {
	res := e.Return.SourceRange()
	if len(e.Exprs) > 0 {
		res = res.Merge(e.Exprs[len(e.Exprs)-1].SourceRange())
	}
	return res
}
func (e *ReturnStmt) Visit(v NodeVisitor) {
	v.VisitReturnStmt(e)
}

type BreakStmt struct {
	Break Token
}

func (e *BreakStmt) stmtNode() {}
func (e *BreakStmt) SourceRange() SourceRange {
	return e.Break.SourceRange()
}
func (e *BreakStmt) Visit(v NodeVisitor) {
	v.VisitBreakStmt(e)
}

type IncDecStmt struct {
	Expr     Expr
	Operator Token
}

func (e *IncDecStmt) stmtNode() {}
func (e *IncDecStmt) SourceRange() SourceRange {
	return e.Expr.SourceRange().Merge(e.Operator.SourceRange())
}
func (e *IncDecStmt) Visit(v NodeVisitor) {
	v.VisitIncDecStmt(e)
}

type BlockStmt struct {
	LBrace Token
	Stmts  []Stmt
	RBrace Token
}

func (e *BlockStmt) stmtNode() {}
func (e *BlockStmt) SourceRange() SourceRange {
	return e.LBrace.SourceRange().Merge(e.RBrace.SourceRange())
}
func (e *BlockStmt) Visit(v NodeVisitor) {
	v.VisitBlockStmt(e)
}

// Visitor Interface
type NodeVisitor interface {
	VisitLiteralExpr(n *LiteralExpr)
	VisitIdentifierExpr(n *IdentifierExpr)
	VisitParenExpr(n *ParenExpr)
	VisitSelectorExpr(n *SelectorExpr)
	VisitIndexExpr(n *IndexExpr)
	VisitCallExpr(n *CallExpr)
	VisitUnaryExpr(n *UnaryExpr)
	VisitBinaryExpr(n *BinaryExpr)
	VisitComplitExpr(n *ComplitExpr)

	VisitNamedType(n *NamedType)

	VisitExprStmt(n *ExprStmt)
	VisitReturnStmt(n *ReturnStmt)
	VisitBreakStmt(n *BreakStmt)
	VisitIncDecStmt(n *IncDecStmt)
	VisitBlockStmt(n *BlockStmt)
}

type DefaultVisitor struct{}

func (v *DefaultVisitor) VisitLiteralExpr(n *LiteralExpr)       {}
func (v *DefaultVisitor) VisitIdentifierExpr(n *IdentifierExpr) {}
func (v *DefaultVisitor) VisitParenExpr(n *ParenExpr) {
	n.Base.Visit(v)
}
func (v *DefaultVisitor) VisitSelectorExpr(n *SelectorExpr) {
	n.Base.Visit(v)
	n.Selector.Visit(v)
}
func (v *DefaultVisitor) VisitIndexExpr(n *IndexExpr) {
	n.Base.Visit(v)
	n.Index.Visit(v)
}
func (v *DefaultVisitor) VisitCallExpr(n *CallExpr) {
	n.Base.Visit(v)
	for _, arg := range n.Args {
		arg.Visit(v)
	}
}
func (v *DefaultVisitor) VisitUnaryExpr(n *UnaryExpr) {
	n.Base.Visit(v)
}
func (v *DefaultVisitor) VisitBinaryExpr(n *BinaryExpr) {
	n.LHS.Visit(v)
	n.RHS.Visit(v)
}
func (v *DefaultVisitor) VisitComplitExpr(n *ComplitExpr) {
	n.Type.Visit(v)
	for _, element := range n.Elements {
		if element.Name != nil {
			element.Name.Visit(v)
		}
		element.Value.Visit(v)
	}
}

func (v *DefaultVisitor) VisitNamedType(n *NamedType) {}

func (v *DefaultVisitor) VisitExprStmt(n *ExprStmt) {
	n.Expr.Visit(v)
}
func (v *DefaultVisitor) VisitReturnStmt(n *ReturnStmt) {
	for _, e := range n.Exprs {
		e.Visit(v)
	}
}
func (v *DefaultVisitor) VisitBreakStmt(n *BreakStmt) {}
func (v *DefaultVisitor) VisitIncDecStmt(n *IncDecStmt) {
	n.Expr.Visit(v)
}
func (v *DefaultVisitor) VisitBlockStmt(n *BlockStmt) {
	for _, s := range n.Stmts {
		s.Visit(v)
	}
}
