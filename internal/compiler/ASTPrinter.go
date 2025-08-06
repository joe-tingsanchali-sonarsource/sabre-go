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

func (v *ASTPrinter) visitPhonyNode(n Node, name string) {
	v.indentor.printf("(%v", name)
	v.indentor.Push()
	v.indentor.NewLine()

	n.Visit(v)

	v.indentor.Pop()
	v.indentor.NewLine()
	v.indentor.print(")")
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

func (v *ASTPrinter) VisitStructType(n *StructType) {
	v.indentor.printf("(StructType %v", len(n.FieldList.Fields))

	if len(n.FieldList.Fields) > 0 {
		v.indentor.Push()

		for _, f := range n.FieldList.Fields {
			v.indentor.NewLine()
			v.indentor.print("(StructTypeField")
			v.indentor.Push()

			// Identifiers
			for _, name := range f.Names {
				v.indentor.NewLine()
				name.Visit(v)
			}

			// Type
			v.indentor.NewLine()
			f.Type.Visit(v)

			// Tag
			if f.Tag.valid() {
				v.indentor.NewLine()
				v.indentor.printf("(Tag %v)", f.Tag)
			}

			v.indentor.Pop()
			v.indentor.NewLine()
			v.indentor.print(")")
		}

		v.indentor.Pop()
		v.indentor.NewLine()
	}
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
		v.visitPhonyNode(n.Init, "SwitchStmt-Init")
	}

	if n.Tag != nil {
		v.indentor.NewLine()
		v.visitPhonyNode(n.Tag, "SwitchStmt-Tag")
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
		v.visitPhonyNode(n.Init, "IfStmt-Init")
	}

	v.indentor.NewLine()
	v.visitPhonyNode(n.Cond, "IfStmt-Cond")
	v.indentor.NewLine()
	v.visitPhonyNode(n.Body, "IfStmt-Body")

	if n.Else != nil {
		v.indentor.NewLine()
		v.visitPhonyNode(n.Else, "IfStmt-Else")
	}

	v.indentor.Pop()
	v.indentor.NewLine()
	v.indentor.print(")")
}

func (v *ASTPrinter) VisitForStmt(n *ForStmt) {
	v.indentor.print("(ForStmt")
	v.indentor.Push()

	if n.Init != nil {
		v.indentor.NewLine()
		v.visitPhonyNode(n.Init, "ForStmt-Init")
	}

	if n.Cond != nil {
		v.indentor.NewLine()
		v.visitPhonyNode(n.Cond, "ForStmt-Cond")
	}

	if n.Post != nil {
		v.indentor.NewLine()
		v.visitPhonyNode(n.Post, "ForStmt-Post")
	}

	v.indentor.NewLine()
	v.visitPhonyNode(n.Body, "ForStmt-Body")

	v.indentor.Pop()
	v.indentor.NewLine()
	v.indentor.print(")")
}

func (v *ASTPrinter) VisitForRangeStmt(n *ForRangeStmt) {
	v.indentor.print("(ForRangeStmt")
	v.indentor.Push()

	if n.Init != nil {
		v.indentor.NewLine()
		v.visitPhonyNode(n.Init, "ForRangeStmt-Init")
	} else {
		v.indentor.NewLine()
		v.visitPhonyNode(n.Expr, "ForRangeStmt-Expr")
	}

	v.indentor.NewLine()
	v.visitPhonyNode(n.Body, "ForRangeStmt-Body")

	v.indentor.Pop()
	v.indentor.NewLine()
	v.indentor.print(")")
}

func (v *ASTPrinter) VisitTypeSpec(n *TypeSpec) {
	v.indentor.print("(TypeSpec")
	if n.Assign.valid() {
		v.indentor.print(" alias")
	}
	v.indentor.Push()
	v.indentor.NewLine()

	n.Name.Visit(v)
	v.indentor.NewLine()
	n.Type.Visit(v)

	v.indentor.Pop()
	v.indentor.NewLine()
	v.indentor.print(")")
}

func (v *ASTPrinter) VisitGenericDecl(n *GenericDecl) {
	v.indentor.printf("(GenericDecl %v", n.DeclToken.Value())
	v.indentor.Push()

	for _, s := range n.Specs {
		v.indentor.NewLine()
		s.Visit(v)
	}

	v.indentor.Pop()
	v.indentor.NewLine()
	v.indentor.print(")")
}
