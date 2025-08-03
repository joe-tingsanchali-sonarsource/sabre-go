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

func (v *ASTPrinter) VisitLiteralExpr(n *LiteralExpr) {
	v.indentor.printf("(LiteralExpr %v)", n.Token)
}

func (v *ASTPrinter) VisitIdentifierExpr(n *IdentifierExpr) {
	v.indentor.printf("(IdentifierExpr %v)", n.Token)
}

func (v *ASTPrinter) VisitParenExpr(n *ParenExpr) {
	v.indentor.print("(ParenExpr")
	v.indentor.Push()
	v.indentor.NewLine()

	n.Base.Visit(v)

	v.indentor.Pop()
	v.indentor.NewLine()
	v.indentor.print(")")
}

func (v *ASTPrinter) VisitSelectorExpr(n *SelectorExpr) {
	v.indentor.print("(SelectorExpr")
	v.indentor.Push()
	v.indentor.NewLine()

	n.Base.Visit(v)
	v.indentor.NewLine()
	n.Selector.Visit(v)

	v.indentor.Pop()
	v.indentor.NewLine()
	v.indentor.print(")")
}

func (v *ASTPrinter) VisitIndexExpr(n *IndexExpr) {
	v.indentor.print("(IndexExpr")
	v.indentor.Push()
	v.indentor.NewLine()

	n.Base.Visit(v)
	v.indentor.NewLine()
	n.Index.Visit(v)

	v.indentor.Pop()
	v.indentor.NewLine()
	v.indentor.print(")")
}

func (v *ASTPrinter) VisitCallExpr(n *CallExpr) {
	v.indentor.print("(CallExpr")
	v.indentor.Push()
	v.indentor.NewLine()

	n.Base.Visit(v)
	for _, arg := range n.Args {
		v.indentor.NewLine()
		arg.Visit(v)
	}

	v.indentor.Pop()
	v.indentor.NewLine()
	v.indentor.print(")")
}

func (v *ASTPrinter) VisitUnaryExpr(n *UnaryExpr) {
	v.indentor.printf("(UnaryExpr %v", n.Operator)
	v.indentor.Push()
	v.indentor.NewLine()

	n.Base.Visit(v)

	v.indentor.Pop()
	v.indentor.NewLine()
	v.indentor.print(")")
}

func (v *ASTPrinter) VisitBinaryExpr(n *BinaryExpr) {
	v.indentor.printf("(BinaryExpr %v", n.Operator)
	v.indentor.Push()
	v.indentor.NewLine()

	n.LHS.Visit(v)
	v.indentor.NewLine()
	n.RHS.Visit(v)

	v.indentor.Pop()
	v.indentor.NewLine()
	v.indentor.print(")")
}

func (v *ASTPrinter) VisitComplitExpr(n *ComplitExpr) {
	v.indentor.print("(ComplitExpr")
	v.indentor.Push()
	v.indentor.NewLine()

	n.Type.Visit(v)
	for _, element := range n.Elements {
		v.indentor.NewLine()
		v.indentor.print("(Element")
		v.indentor.Push()

		if element.Name != nil {
			v.indentor.NewLine()
			element.Name.Visit(v)
		}

		if element.Value != nil {
			v.indentor.NewLine()
			element.Value.Visit(v)
		}

		v.indentor.Pop()
		v.indentor.NewLine()
		v.indentor.print(")")
	}

	v.indentor.Pop()
	v.indentor.NewLine()
	v.indentor.print(")")
}

func (v *ASTPrinter) VisitNamedType(n *NamedType) {
	if n.Package.valid() {
		v.indentor.printf("(NamedType %v.%v)", n.Package, n.TypeName)
	} else {
		v.indentor.printf("(NamedType %v)", n.TypeName)
	}
}

func (v *ASTPrinter) VisitArrayType(n *ArrayType) {
	v.indentor.print("(ArrayType")
	v.indentor.Push()

	if n.Length != nil {
		v.indentor.NewLine()
		n.Length.Visit(v)
	}

	v.indentor.NewLine()
	n.ElementType.Visit(v)

	v.indentor.Pop()
	v.indentor.NewLine()
	v.indentor.print(")")
}

func (v *ASTPrinter) VisitExprStmt(n *ExprStmt) {
	v.indentor.print("(ExprStmt")
	v.indentor.Push()
	v.indentor.NewLine()

	n.Expr.Visit(v)

	v.indentor.Pop()
	v.indentor.NewLine()
	v.indentor.print(")")
}

func (v *ASTPrinter) VisitReturnStmt(n *ReturnStmt) {
	v.indentor.print("(ReturnStmt")
	if len(n.Exprs) > 0 {
		v.indentor.Push()

		for _, e := range n.Exprs {
			v.indentor.NewLine()
			e.Visit(v)
		}

		v.indentor.Pop()
		v.indentor.NewLine()
	}
	v.indentor.print(")")
}

func (v *ASTPrinter) VisitBreakStmt(n *BreakStmt) {
	if n.IsLabeled() {
		v.indentor.printf("(BreakStmt %v)", n.Label)
	} else {
		v.indentor.print("(BreakStmt)")
	}
}

func (v *ASTPrinter) VisitFallthroughStmt(n *FallthroughStmt) {
	v.indentor.print("(FallthroughStmt)")
}

func (v *ASTPrinter) VisitContinueStmt(n *ContinueStmt) {
	if n.IsLabeled() {
		v.indentor.printf("(ContinueStmt %v)", n.Label)
	} else {
		v.indentor.print("(ContinueStmt)")
	}
}

func (v *ASTPrinter) VisitIncDecStmt(n *IncDecStmt) {
	v.indentor.printf("(IncDecStmt %v", n.Operator.Value())
	v.indentor.Push()
	v.indentor.NewLine()

	n.Expr.Visit(v)

	v.indentor.Pop()
	v.indentor.NewLine()
	v.indentor.print(")")
}

func (v *ASTPrinter) VisitBlockStmt(n *BlockStmt) {
	v.indentor.printf("(Block %v", len(n.Stmts))
	if len(n.Stmts) > 0 {
		v.indentor.Push()

		for _, s := range n.Stmts {
			v.indentor.NewLine()
			s.Visit(v)
		}

		v.indentor.Pop()
		v.indentor.NewLine()
	}
	v.indentor.print(")")
}

func (v *ASTPrinter) VisitAssignStmt(n *AssignStmt) {
	v.indentor.print("(AssignStmt")
	v.indentor.Push()
	for _, e := range n.LHS {
		v.indentor.NewLine()
		e.Visit(v)
	}

	v.indentor.NewLine()
	v.indentor.print(n.Operator)

	for _, e := range n.RHS {
		v.indentor.NewLine()
		e.Visit(v)
	}

	v.indentor.Pop()
	v.indentor.NewLine()
	v.indentor.print(")")
}

func (v *ASTPrinter) VisitSwitchCaseStmt(n *SwitchCaseStmt) {
	if n.Case.Kind() == TokenCase {
		v.indentor.print("(SwitchCaseStmt")
	} else {
		v.indentor.print("(DefaultCaseStmt")
	}

	v.indentor.Push()
	for _, e := range n.LHS {
		v.indentor.NewLine()
		e.Visit(v)
	}

	v.indentor.NewLine()
	v.indentor.print(n.Colon)

	for _, e := range n.RHS {
		v.indentor.NewLine()
		e.Visit(v)
	}

	v.indentor.Pop()
	v.indentor.NewLine()
	v.indentor.print(")")
}

func (v *ASTPrinter) VisitSwitchStmt(n *SwitchStmt) {
	v.indentor.print("(SwitchStmt")
	v.indentor.Push()

	if n.Init != nil {
		v.indentor.NewLine()
		v.indentor.print("(SwitchStmt-Init")
		v.indentor.Push()
		v.indentor.NewLine()

		n.Init.Visit(v)

		v.indentor.Pop()
		v.indentor.NewLine()
		v.indentor.print(")")
	}

	if n.Tag != nil {
		v.indentor.NewLine()
		v.indentor.print("(SwitchStmt-Tag")
		v.indentor.Push()
		v.indentor.NewLine()

		n.Tag.Visit(v)

		v.indentor.Pop()
		v.indentor.NewLine()
		v.indentor.print(")")
	}

	v.indentor.NewLine()
	n.Body.Visit(v)

	v.indentor.Pop()
	v.indentor.NewLine()
	v.indentor.print(")")
}

func (v *ASTPrinter) VisitIfStmt(n *IfStmt) {
	v.indentor.print("(IfStmt")
	v.indentor.Push()

	if n.Init != nil {
		v.indentor.NewLine()
		v.indentor.print("(IfStmt-Init")
		v.indentor.Push()
		v.indentor.NewLine()

		n.Init.Visit(v)

		v.indentor.Pop()
		v.indentor.NewLine()
		v.indentor.print(")")
	}

	// Condition
	v.indentor.NewLine()
	v.indentor.print("(IfStmt-Cond")
	v.indentor.Push()
	v.indentor.NewLine()

	n.Cond.Visit(v)

	v.indentor.Pop()
	v.indentor.NewLine()
	v.indentor.print(")")

	// Body
	v.indentor.NewLine()
	v.indentor.print("(IfStmt-Body")
	v.indentor.Push()
	v.indentor.NewLine()

	n.Body.Visit(v)

	v.indentor.Pop()
	v.indentor.NewLine()
	v.indentor.print(")")

	if n.Else != nil {
		v.indentor.NewLine()
		v.indentor.print("(IfStmt-Else")
		v.indentor.Push()
		v.indentor.NewLine()

		n.Else.Visit(v)

		v.indentor.Pop()
		v.indentor.NewLine()
		v.indentor.print(")")
	}

	v.indentor.Pop()
	v.indentor.NewLine()
	v.indentor.print(")")
}
