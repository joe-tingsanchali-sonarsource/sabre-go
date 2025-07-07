package compiler

import (
	"fmt"
	"io"
)

type ASTPrinter struct {
	DefaultVisitor
	out io.Writer
}

func NewASTPrinter(out io.Writer) *ASTPrinter {
	return &ASTPrinter{out: out}
}

func (v *ASTPrinter) VisitLiteralExpr(n *LiteralExpr) bool {
	fmt.Fprintf(v.out, "(LiteralExpr %v)", n.Token)
	return true
}

func (v *ASTPrinter) VisitIdentifierExpr(n *IdentifierExpr) bool {
	fmt.Fprintf(v.out, "(IdentifierExpr %v)", n.Token)
	return true
}
