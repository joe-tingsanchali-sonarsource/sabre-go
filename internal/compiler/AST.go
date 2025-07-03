package compiler

type Node interface {
	SourceRange() SourceRange
	Parent() Node
	Children() []Node
}

type NodeBase struct {
	parent   Node
	children []Node
}

func (n NodeBase) Parent() Node     { return n.parent }
func (n NodeBase) Children() []Node { return n.children }

type Expr interface {
	Node
	exprNode()
}

type LiteralExpr struct {
	NodeBase
	Token Token
}

func (e LiteralExpr) exprNode()                {}
func (e LiteralExpr) SourceRange() SourceRange { return e.Token.SourceRange() }
