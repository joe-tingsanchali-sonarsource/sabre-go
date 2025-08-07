package compiler

type Node interface {
	SourceRange() SourceRange
	Visit(NodeVisitor)
}

// Expr Nodes
type Expr interface {
	Node
	exprNode()
}

type LiteralExpr struct {
	Token Token
}

func (e *LiteralExpr) exprNode()                {}
func (e *LiteralExpr) SourceRange() SourceRange { return e.Token.SourceRange() }
func (e *LiteralExpr) Visit(v NodeVisitor) {
	v.VisitLiteralExpr(e)
}

type IdentifierExpr struct {
	Token Token
}

func (e *IdentifierExpr) exprNode()                {}
func (e *IdentifierExpr) SourceRange() SourceRange { return e.Token.SourceRange() }
func (e *IdentifierExpr) Visit(v NodeVisitor) {
	v.VisitIdentifierExpr(e)
}

type ParenExpr struct {
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
	Package  Token
	TypeName Token
	Expr     *IdentifierExpr
}

func (e *NamedType) IsPackageQualified() bool { return e.Package.Kind() == TokenIdentifier }
func (e *NamedType) exprNode()                {}
func (e *NamedType) typeExpr()                {}
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

type ArrayType struct {
	LBracket    Token
	Length      Expr
	RBracket    Token
	ElementType Type
}

func (e *ArrayType) exprNode() {}
func (e *ArrayType) typeExpr() {}
func (e *ArrayType) SourceRange() SourceRange {
	return e.LBracket.SourceRange().Merge(e.ElementType.SourceRange())
}
func (e *ArrayType) Visit(v NodeVisitor) {
	v.VisitArrayType(e)
}

type Field struct {
	Names []*IdentifierExpr
	Type  Type
	Tag   Token
}

type FieldList struct {
	Open   Token // { or (
	Fields []Field
	Close  Token // } or )
}

type StructType struct {
	Struct    Token
	FieldList FieldList
}

func (e *StructType) exprNode() {}
func (e *StructType) typeExpr() {}
func (e *StructType) SourceRange() SourceRange {
	return e.Struct.SourceRange().Merge(e.FieldList.Close.SourceRange())
}
func (e *StructType) Visit(v NodeVisitor) {
	v.VisitStructType(e)
}

type FuncType struct {
	Func           Token
	TypeParameters FieldList
	Parameters     FieldList
	Results        FieldList
}

func (e *FuncType) exprNode() {}
func (e *FuncType) typeExpr() {}
func (e *FuncType) SourceRange() SourceRange {
	if len(e.Results.Fields) > 0 {
		return e.Func.SourceRange().Merge(e.Results.Fields[len(e.Results.Fields)-1].Type.SourceRange())
	} else {
		return e.Func.SourceRange().Merge(e.Parameters.Close.SourceRange())
	}
}
func (e *FuncType) Visit(v NodeVisitor) {
	v.VisitFuncType(e)
}

// Stmt Nodes
type Stmt interface {
	Node
	stmtNode()
}

type ExprStmt struct {
	Expr Expr
}

func (e *ExprStmt) stmtNode()                {}
func (e *ExprStmt) SourceRange() SourceRange { return e.Expr.SourceRange() }
func (e *ExprStmt) Visit(v NodeVisitor) {
	v.VisitExprStmt(e)
}

type ReturnStmt struct {
	Return Token
	Exprs  []Expr
}

func (e *ReturnStmt) stmtNode() {}
func (e *ReturnStmt) SourceRange() SourceRange {
	res := e.Return.SourceRange()
	if len(e.Exprs) > 0 {
		res = res.Merge(e.Exprs[len(e.Exprs)-1].SourceRange())
	}
	return res
}
func (e *ReturnStmt) Visit(v NodeVisitor) {
	v.VisitReturnStmt(e)
}

type BreakStmt struct {
	Break Token
	Label Token
}

func (e *BreakStmt) IsLabeled() bool {
	return e.Label.Kind() == TokenIdentifier
}
func (e *BreakStmt) stmtNode() {}
func (e *BreakStmt) SourceRange() SourceRange {
	if e.IsLabeled() {
		return e.Break.SourceRange().Merge(e.Label.SourceRange())
	} else {
		return e.Break.SourceRange()
	}
}
func (e *BreakStmt) Visit(v NodeVisitor) {
	v.VisitBreakStmt(e)
}

type FallthroughStmt struct {
	Fallthrough Token
}

func (e *FallthroughStmt) stmtNode() {}
func (e *FallthroughStmt) SourceRange() SourceRange {
	return e.Fallthrough.SourceRange()
}
func (e *FallthroughStmt) Visit(v NodeVisitor) {
	v.VisitFallthroughStmt(e)
}

type ContinueStmt struct {
	Continue Token
	Label    Token
}

func (e *ContinueStmt) IsLabeled() bool {
	return e.Label.Kind() == TokenIdentifier
}
func (e *ContinueStmt) stmtNode() {}
func (e *ContinueStmt) SourceRange() SourceRange {
	if e.IsLabeled() {
		return e.Continue.SourceRange().Merge(e.Label.SourceRange())
	} else {
		return e.Continue.SourceRange()
	}
}
func (e *ContinueStmt) Visit(v NodeVisitor) {
	v.VisitContinueStmt(e)
}

type IncDecStmt struct {
	Expr     Expr
	Operator Token
}

func (e *IncDecStmt) stmtNode() {}
func (e *IncDecStmt) SourceRange() SourceRange {
	return e.Expr.SourceRange().Merge(e.Operator.SourceRange())
}
func (e *IncDecStmt) Visit(v NodeVisitor) {
	v.VisitIncDecStmt(e)
}

type BlockStmt struct {
	LBrace Token
	Stmts  []Stmt
	RBrace Token
}

func (e *BlockStmt) stmtNode() {}
func (e *BlockStmt) SourceRange() SourceRange {
	return e.LBrace.SourceRange().Merge(e.RBrace.SourceRange())
}
func (e *BlockStmt) Visit(v NodeVisitor) {
	v.VisitBlockStmt(e)
}

// AssignStmt represents assignment statements as well as short
// variable declarations
type AssignStmt struct {
	LHS      []Expr
	Operator Token
	RHS      []Expr
}

func (e *AssignStmt) stmtNode() {}
func (e *AssignStmt) SourceRange() SourceRange {
	if len(e.LHS) == 0 || len(e.RHS) == 0 {
		panic("AssignStmt LHS and RHS shouldn't be empty")
	}
	return e.LHS[0].SourceRange().Merge(e.RHS[len(e.RHS)-1].SourceRange())
}
func (e *AssignStmt) Visit(v NodeVisitor) {
	v.VisitAssignStmt(e)
}

type SwitchCaseStmt struct {
	Case  Token
	LHS   []Expr
	Colon Token
	RHS   []Stmt
}

func (e *SwitchCaseStmt) stmtNode() {}
func (e *SwitchCaseStmt) SourceRange() SourceRange {
	if len(e.RHS) == 0 {
		panic("SwitchCaseStmt RHS shouldn't be empty")
	}
	return e.Case.SourceRange().Merge(e.RHS[len(e.RHS)-1].SourceRange())
}
func (e *SwitchCaseStmt) Visit(v NodeVisitor) {
	v.VisitSwitchCaseStmt(e)
}

type SwitchStmt struct {
	Switch Token
	Init   Stmt
	Tag    Expr
	Body   Stmt
}

func (e *SwitchStmt) stmtNode() {}
func (e *SwitchStmt) SourceRange() SourceRange {
	return e.Switch.SourceRange().Merge(e.Body.SourceRange())
}
func (e *SwitchStmt) Visit(v NodeVisitor) {
	v.VisitSwitchStmt(e)
}

type IfStmt struct {
	If   Token
	Init Stmt // init statement or nil
	Cond Expr
	Body *BlockStmt
	Else Stmt // else branch or nil
}

func (e *IfStmt) stmtNode() {}
func (e *IfStmt) SourceRange() SourceRange {
	if e.Else != nil {
		return e.If.SourceRange().Merge(e.Else.SourceRange())
	} else {
		return e.If.SourceRange().Merge(e.Body.SourceRange())
	}
}
func (e *IfStmt) Visit(v NodeVisitor) {
	v.VisitIfStmt(e)
}

type ForStmt struct {
	For  Token
	Init Stmt
	Cond Expr
	Post Stmt
	Body *BlockStmt
}

func (e *ForStmt) stmtNode() {}
func (e *ForStmt) SourceRange() SourceRange {
	return e.For.SourceRange().Merge(e.Body.SourceRange())
}
func (e *ForStmt) Visit(v NodeVisitor) {
	v.VisitForStmt(e)
}

type ForRangeStmt struct {
	For   Token
	Init  *AssignStmt
	Range Token
	Expr  Expr
	Body  *BlockStmt
}

func (e *ForRangeStmt) stmtNode() {}
func (e *ForRangeStmt) SourceRange() SourceRange {
	return e.For.SourceRange().Merge(e.Body.SourceRange())
}
func (e *ForRangeStmt) Visit(v NodeVisitor) {
	v.VisitForRangeStmt(e)
}

// Specs
type Spec interface {
	Node
	specNode()
}

type TypeSpec struct {
	Name   *IdentifierExpr
	Assign Token // = token or nil
	Type   Type
}

func (e *TypeSpec) specNode() {}
func (e *TypeSpec) SourceRange() SourceRange {
	return e.Name.SourceRange().Merge(e.Type.SourceRange())
}
func (e *TypeSpec) Visit(v NodeVisitor) {
	v.VisitTypeSpec(e)
}

// Declarations
type Decl interface {
	Node
	declNode()
}

type GenericDecl struct {
	DeclToken Token // import, const, var, type
	LParen    Token // if any
	Specs     []Spec
	RParen    Token // if any
}

func (e *GenericDecl) declNode() {}
func (e *GenericDecl) SourceRange() SourceRange {
	if e.RParen.valid() {
		return e.DeclToken.SourceRange().Merge(e.RParen.SourceRange())
	}
	if len(e.Specs) > 0 {
		return e.DeclToken.SourceRange().Merge(e.Specs[len(e.Specs)-1].SourceRange())
	}
	panic("generic decl is expected to have RParen or at least one Spec")
}
func (e *GenericDecl) Visit(v NodeVisitor) {
	v.VisitGenericDecl(e)
}

// type FuncDecl struct {
// 	Name *IdentifierExpr
// 	// Type *FuncType
// 	Body *BlockStmt
// }

// func (e *FuncDecl) declNode() {}
// func (e *FuncDecl) SourceRange() SourceRange {
// 	if e.RParen.valid() {
// 		return e.DeclToken.SourceRange().Merge(e.RParen.SourceRange())
// 	}
// 	if len(e.Specs) > 0 {
// 		return e.DeclToken.SourceRange().Merge(e.Specs[len(e.Specs)-1].SourceRange())
// 	}
// 	panic("generic decl is expected to have RParen or at least one Spec")
// }
// func (e *FuncDecl) Visit(v NodeVisitor) {
// 	v.VisitFuncDecl(e)
// }

// Visitor Interface
type NodeVisitor interface {
	VisitLiteralExpr(n *LiteralExpr)
	VisitIdentifierExpr(n *IdentifierExpr)
	VisitParenExpr(n *ParenExpr)
	VisitSelectorExpr(n *SelectorExpr)
	VisitIndexExpr(n *IndexExpr)
	VisitCallExpr(n *CallExpr)
	VisitUnaryExpr(n *UnaryExpr)
	VisitBinaryExpr(n *BinaryExpr)
	VisitComplitExpr(n *ComplitExpr)

	VisitNamedType(n *NamedType)
	VisitArrayType(n *ArrayType)
	VisitStructType(n *StructType)
	VisitFuncType(n *FuncType)

	VisitExprStmt(n *ExprStmt)
	VisitReturnStmt(n *ReturnStmt)
	VisitBreakStmt(n *BreakStmt)
	VisitFallthroughStmt(n *FallthroughStmt)
	VisitContinueStmt(n *ContinueStmt)
	VisitIncDecStmt(n *IncDecStmt)
	VisitBlockStmt(n *BlockStmt)
	VisitAssignStmt(n *AssignStmt)
	VisitSwitchCaseStmt(n *SwitchCaseStmt)
	VisitSwitchStmt(n *SwitchStmt)
	VisitIfStmt(n *IfStmt)
	VisitForStmt(n *ForStmt)
	VisitForRangeStmt(n *ForRangeStmt)

	VisitTypeSpec(n *TypeSpec)

	VisitGenericDecl(n *GenericDecl)
}

type DefaultVisitor struct{}

func (v *DefaultVisitor) VisitLiteralExpr(n *LiteralExpr)       {}
func (v *DefaultVisitor) VisitIdentifierExpr(n *IdentifierExpr) {}
func (v *DefaultVisitor) VisitParenExpr(n *ParenExpr) {
	n.Base.Visit(v)
}
func (v *DefaultVisitor) VisitSelectorExpr(n *SelectorExpr) {
	n.Base.Visit(v)
	n.Selector.Visit(v)
}
func (v *DefaultVisitor) VisitIndexExpr(n *IndexExpr) {
	n.Base.Visit(v)
	n.Index.Visit(v)
}
func (v *DefaultVisitor) VisitCallExpr(n *CallExpr) {
	n.Base.Visit(v)
	for _, arg := range n.Args {
		arg.Visit(v)
	}
}
func (v *DefaultVisitor) VisitUnaryExpr(n *UnaryExpr) {
	n.Base.Visit(v)
}
func (v *DefaultVisitor) VisitBinaryExpr(n *BinaryExpr) {
	n.LHS.Visit(v)
	n.RHS.Visit(v)
}
func (v *DefaultVisitor) VisitComplitExpr(n *ComplitExpr) {
	n.Type.Visit(v)
	for _, element := range n.Elements {
		if element.Name != nil {
			element.Name.Visit(v)
		}
		element.Value.Visit(v)
	}
}

func (v *DefaultVisitor) VisitNamedType(n *NamedType) {}
func (v *DefaultVisitor) VisitArrayType(n *ArrayType) {
	n.Length.Visit(v)
	n.ElementType.Visit(v)
}
func (v *DefaultVisitor) VisitStructType(n *StructType) {
	for _, e := range n.FieldList.Fields {
		for _, name := range e.Names {
			name.Visit(v)
		}
		e.Type.Visit(v)
	}
}

func (v *DefaultVisitor) VisitFuncType(n *FuncType) {
	for _, e := range n.TypeParameters.Fields {
		for _, name := range e.Names {
			name.Visit(v)
		}
		e.Type.Visit(v)
	}

	for _, e := range n.Parameters.Fields {
		for _, name := range e.Names {
			name.Visit(v)
		}
		e.Type.Visit(v)
	}

	for _, e := range n.Results.Fields {
		for _, name := range e.Names {
			name.Visit(v)
		}
		e.Type.Visit(v)
	}
}

func (v *DefaultVisitor) VisitExprStmt(n *ExprStmt) {
	n.Expr.Visit(v)
}
func (v *DefaultVisitor) VisitReturnStmt(n *ReturnStmt) {
	for _, e := range n.Exprs {
		e.Visit(v)
	}
}
func (v *DefaultVisitor) VisitBreakStmt(n *BreakStmt)             {}
func (v *DefaultVisitor) VisitFallthroughStmt(n *FallthroughStmt) {}
func (v *DefaultVisitor) VisitContinueStmt(n *ContinueStmt)       {}
func (v *DefaultVisitor) VisitIncDecStmt(n *IncDecStmt) {
	n.Expr.Visit(v)
}
func (v *DefaultVisitor) VisitBlockStmt(n *BlockStmt) {
	for _, s := range n.Stmts {
		s.Visit(v)
	}
}
func (v *DefaultVisitor) VisitAssignStmt(n *AssignStmt) {
	for _, e := range n.LHS {
		e.Visit(v)
	}
	for _, e := range n.RHS {
		e.Visit(v)
	}
}
func (v *DefaultVisitor) VisitSwitchCaseStmt(n *SwitchCaseStmt) {
	for _, e := range n.LHS {
		e.Visit(v)
	}
	for _, e := range n.RHS {
		e.Visit(v)
	}
}
func (v *DefaultVisitor) VisitSwitchStmt(n *SwitchStmt) {
	n.Init.Visit(v)
	n.Tag.Visit(v)
	n.Body.Visit(v)
}
func (v *DefaultVisitor) VisitIfStmt(n *IfStmt) {
	if n.Init != nil {
		n.Init.Visit(v)
	}
	n.Cond.Visit(v)
	n.Body.Visit(v)
	if n.Else != nil {
		n.Else.Visit(v)
	}
}

func (v *DefaultVisitor) VisitForStmt(n *ForStmt) {
	if n.Init != nil {
		n.Init.Visit(v)
	}

	if n.Cond != nil {
		n.Cond.Visit(v)
	}

	if n.Post != nil {
		n.Post.Visit(v)
	}

	n.Body.Visit(v)
}

func (v *DefaultVisitor) VisitForRangeStmt(n *ForRangeStmt) {
	if n.Init != nil {
		n.Init.Visit(v)
	}
	n.Expr.Visit(v)
}

func (v *DefaultVisitor) VisitTypeSpec(n *TypeSpec) {
	n.Name.Visit(v)
	n.Type.Visit(v)
}

func (v *DefaultVisitor) VisitGenericDecl(n *GenericDecl) {
	for _, s := range n.Specs {
		s.Visit(v)
	}
}
