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

// Visitor Interface
type NodeVisitor interface {
	VisitLiteralExpr(n *LiteralExpr) bool
	VisitIdentifierExpr(n *IdentifierExpr) bool
}

type DefaultVisitor struct{}

func (v *DefaultVisitor) VisitLiteralExpr(n *LiteralExpr) bool       { return true }
func (v *DefaultVisitor) VisitIdentifierExpr(n *IdentifierExpr) bool { return true }
