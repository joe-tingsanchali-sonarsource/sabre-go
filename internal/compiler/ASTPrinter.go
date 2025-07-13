package compiler

import (
	"fmt"
	"io"
)

type Indentor struct {
	indentLevel int
	out         io.Writer
}

func NewIndentor(out io.Writer) Indentor {
	return Indentor{
		indentLevel: 0,
		out:         out,
	}
}

func (ind Indentor) indent() {
	for i := 0; i < ind.indentLevel; i++ {
		fmt.Fprint(ind.out, "  ")
	}
}

func (ind Indentor) NewLine() {
	fmt.Fprint(ind.out, "\n")
	ind.indent()
}

func (ind *Indentor) Push() {
	ind.indentLevel++
}

func (ind *Indentor) Pop() {
	ind.indentLevel--
}

func (ind *Indentor) printf(format string, a ...any) {
	fmt.Fprintf(ind.out, format, a...)
}

func (ind *Indentor) print(a ...any) {
	fmt.Fprint(ind.out, a...)
}

type ASTPrinter struct {
	DefaultVisitor
	indentor Indentor
}

func NewASTPrinter(out io.Writer) *ASTPrinter {
	return &ASTPrinter{indentor: NewIndentor(out)}
}

func (v *ASTPrinter) VisitLiteralExpr(n *LiteralExpr) bool {
	v.indentor.printf("(LiteralExpr %v)", n.Token)
	return true
}

func (v *ASTPrinter) VisitIdentifierExpr(n *IdentifierExpr) bool {
	v.indentor.printf("(IdentifierExpr %v)", n.Token)
	return true
}

func (v *ASTPrinter) VisitParenExpr(n *ParenExpr) bool {
	v.indentor.print("(ParenExpr")
	v.indentor.Push()
	v.indentor.NewLine()

	n.Base.Visit(v)

	v.indentor.Pop()
	v.indentor.NewLine()
	v.indentor.print(")")
	return true
}

func (v *ASTPrinter) VisitSelectorExpr(n *SelectorExpr) bool {
	v.indentor.print("(SelectorExpr")
	v.indentor.Push()
	v.indentor.NewLine()

	n.Base.Visit(v)
	v.indentor.NewLine()
	n.Selector.Visit(v)

	v.indentor.Pop()
	v.indentor.NewLine()
	v.indentor.print(")")
	return true
}

func (v *ASTPrinter) VisitIndexExpr(n *IndexExpr) bool {
	v.indentor.print("(IndexExpr")
	v.indentor.Push()
	v.indentor.NewLine()

	n.Base.Visit(v)
	v.indentor.NewLine()
	n.Index.Visit(v)

	v.indentor.Pop()
	v.indentor.NewLine()
	v.indentor.print(")")
	return true
}
