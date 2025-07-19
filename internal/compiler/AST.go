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

type UnaryExpr struct {
	NodeBase
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
	NodeBase
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
	NodeBase
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
	NodeBase
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

// Visitor Interface
type NodeVisitor interface {
	VisitLiteralExpr(n *LiteralExpr) bool
	VisitIdentifierExpr(n *IdentifierExpr) bool
	VisitParenExpr(n *ParenExpr) bool
	VisitSelectorExpr(n *SelectorExpr) bool
	VisitIndexExpr(n *IndexExpr) bool
	VisitCallExpr(n *CallExpr) bool
	VisitUnaryExpr(n *UnaryExpr) bool
	VisitBinaryExpr(n *BinaryExpr) bool
	VisitComplitExpr(n *ComplitExpr) bool

	VisitNamedType(n *NamedType) bool
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
func (v *DefaultVisitor) VisitUnaryExpr(n *UnaryExpr) bool {
	n.Base.Visit(v)
	return true
}
func (v *DefaultVisitor) VisitBinaryExpr(n *BinaryExpr) bool {
	n.LHS.Visit(v)
	n.RHS.Visit(v)
	return true
}
func (v *DefaultVisitor) VisitComplitExpr(n *ComplitExpr) bool {
	n.Type.Visit(v)
	for _, element := range n.Elements {
		if element.Name != nil {
			element.Name.Visit(v)
		}
		element.Value.Visit(v)
	}
	return true
}

func (v *DefaultVisitor) VisitNamedType(n *NamedType) bool { return true }
