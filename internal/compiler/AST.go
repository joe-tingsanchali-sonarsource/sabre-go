package compiler

type Node interface {
	SourceRange() SourceRange
	Parent() Node
	Children() []Node
	Visit(NodeVisitor)
}

type NodeBase struct {
	parent   Node
	children []Node
}

func (n *NodeBase) Parent() Node     { return n.parent }
func (n *NodeBase) Children() []Node { return n.children }

type Expr interface {
	Node
	exprNode()
}

type LiteralExpr struct {
	NodeBase
	Token Token
}

func (e *LiteralExpr) exprNode()                {}
func (e *LiteralExpr) SourceRange() SourceRange { return e.Token.SourceRange() }
func (e *LiteralExpr) Visit(v NodeVisitor) {
	v.VisitLiteralExpr(e)
}

type IdentifierExpr struct {
	NodeBase
	Token Token
}

func (e *IdentifierExpr) exprNode()                {}
func (e *IdentifierExpr) SourceRange() SourceRange { return e.Token.SourceRange() }
func (e *IdentifierExpr) Visit(v NodeVisitor) {
	v.VisitIdentifierExpr(e)
}

type ParenExpr struct {
	NodeBase
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
	NodeBase
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
	NodeBase
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
	NodeBase
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

// Visitor Interface
type NodeVisitor interface {
	VisitLiteralExpr(n *LiteralExpr) bool
	VisitIdentifierExpr(n *IdentifierExpr) bool
	VisitParenExpr(n *ParenExpr) bool
	VisitSelectorExpr(n *SelectorExpr) bool
	VisitIndexExpr(n *IndexExpr) bool
	VisitCallExpr(n *CallExpr) bool
}

type DefaultVisitor struct{}

func (v *DefaultVisitor) VisitLiteralExpr(n *LiteralExpr) bool       { return true }
func (v *DefaultVisitor) VisitIdentifierExpr(n *IdentifierExpr) bool { return true }
func (v *DefaultVisitor) VisitParenExpr(n *ParenExpr) bool {
	n.Base.Visit(v)
	return true
}
func (v *DefaultVisitor) VisitSelectorExpr(n *SelectorExpr) bool {
	n.Base.Visit(v)
	n.Selector.Visit(v)
	return true
}
func (v *DefaultVisitor) VisitIndexExpr(n *IndexExpr) bool {
	n.Base.Visit(v)
	n.Index.Visit(v)
	return true
}
func (v *DefaultVisitor) VisitCallExpr(n *CallExpr) bool {
	n.Base.Visit(v)
	for _, arg := range n.Args {
		arg.Visit(v)
	}
	return true
}
