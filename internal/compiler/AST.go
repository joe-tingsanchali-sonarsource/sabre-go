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

// Visitor Interface
type NodeVisitor interface {
	VisitLiteralExpr(n *LiteralExpr) bool
}

type DefaultVisitor struct{}

func (v *DefaultVisitor) VisitLiteralExpr() bool { return true }
