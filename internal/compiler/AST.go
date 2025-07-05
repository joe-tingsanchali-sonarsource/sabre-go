package compiler

import (
	"fmt"
	"io"
)

type Node interface {
	SourceRange() SourceRange
	Parent() Node
	Children() []Node
	ASTDump(out io.Writer)
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
func (e LiteralExpr) ASTDump(out io.Writer) {
	fmt.Fprintf(out, "(LiteralExpr %v)", e.Token)
}
