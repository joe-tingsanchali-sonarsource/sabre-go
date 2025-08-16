package compiler

import "go/constant"

type SemanticInfo struct {
	Types  map[Expr]TypeAndValue
	Scopes map[any]*Scope
}

func NewSemanticInfo() *SemanticInfo {
	return &SemanticInfo{
		Types:  make(map[Expr]TypeAndValue),
		Scopes: make(map[any]*Scope),
	}
}

func (info SemanticInfo) TypeOf(e Expr) Type {
	if t, ok := info.Types[e]; ok {
		return t.Type
	}
	return nil
}

func (info SemanticInfo) ScopeOf(n any) *Scope {
	if s, ok := info.Scopes[n]; ok {
		return s
	}
	return nil
}

func (info *SemanticInfo) createScopeFor(n any, parent *Scope, name string) *Scope {
	if scope, ok := info.Scopes[n]; ok {
		return scope
	}
	scope := NewScope(parent, name)
	info.Scopes[n] = scope
	return scope
}

type AddressMode byte

const (
	AddressModeInvalid AddressMode = iota
	AddressModeNoValue
	AddressModeType
	AddressModeConstant
	AddressModeVariable
	AddressModeComputedValue
)

type TypeAndValue struct {
	Mode  AddressMode
	Type  Type
	Value constant.Value
}

func (v TypeAndValue) IsVoid() bool {
	return v.Mode == AddressModeNoValue
}

func (v TypeAndValue) IsType() bool {
	return v.Mode == AddressModeType
}

func (v TypeAndValue) IsValue() bool {
	return v.Mode == AddressModeConstant || v.Mode == AddressModeVariable || v.Mode == AddressModeComputedValue
}

func (v TypeAndValue) IsAddressable() bool {
	return v.Mode == AddressModeVariable
}

func (v TypeAndValue) IsAssignable() bool {
	return v.Mode == AddressModeVariable
}

type Checker struct {
	DefaultVisitor
	unit       *Unit
	scopeStack []*Scope
}

func NewChecker(u *Unit) *Checker {
	return &Checker{
		unit: u,
	}
}

func (checker *Checker) currentScope() *Scope {
	return checker.scopeStack[len(checker.scopeStack)-1]
}

func (checker *Checker) enterScope(scope *Scope) {
	if scope == nil {
		panic("entering nil scope")
	}
	checker.scopeStack = append(checker.scopeStack, scope)
}

func (checker *Checker) leaveScope() {
	checker.scopeStack = checker.scopeStack[:len(checker.scopeStack)-1]
}

func (checker *Checker) Check() bool {
	checker.unit.semanticInfo = NewSemanticInfo()
	globalScope := checker.unit.semanticInfo.createScopeFor(checker.unit.rootFile, nil, "global")

	checker.enterScope(globalScope)
	defer checker.leaveScope()

	checker.shallowWalk()

	for _, sym := range globalScope.Table {
		checker.resolveSymbol(sym)
	}

	return !checker.unit.HasErrors()
}

func (checker *Checker) shallowWalk() {
	for _, d := range checker.unit.rootFile.decls {
		switch decl := d.(type) {
		case *GenericDecl:
			checker.shallowWalkGenericDecl(decl)
		case *FuncDecl:
			checker.shallowWalkFuncDecl(decl)
		default:
			panic("unexpected decl kind")
		}
	}
}

func (checker *Checker) shallowWalkGenericDecl(d *GenericDecl) {
	switch d.DeclToken.Kind() {
	case TokenConst:
		for _, s := range d.Specs {
			spec := s.(*ValueSpec)
			for _, name := range spec.LHS {
				sym := NewConstSymbol(name.Token, d, d.SourceRange())
				checker.addSymbol(sym)
			}
		}
	case TokenVar:
		for _, s := range d.Specs {
			spec := s.(*ValueSpec)
			for _, name := range spec.LHS {
				sym := NewVarSymbol(name.Token, d, d.SourceRange())
				checker.addSymbol(sym)
			}
		}
	case TokenType:
		// do nothing for now
	default:
		panic("unexpected GenericDecl kind")
	}
}

func (checker *Checker) shallowWalkFuncDecl(d *FuncDecl) {
	sym := NewFuncSymbol(d.Name.Token, d, d.SourceRange())
	checker.addSymbol(sym)
}

func (checker *Checker) addSymbol(sym Symbol) Symbol {
	scope := checker.currentScope()
	if oldSym := scope.ShallowFind(sym.Name()); oldSym != nil {
		checker.errorf(
			sym.SourceRange(),
			"symbol '%v' redefinition, first declared in %v",
			sym.Name(),
			oldSym.SourceRange().Begin(),
		)
		return oldSym
	}
	scope.Add(sym)
	sym.SetScope(scope)
	return nil
}

func (checker *Checker) errorf(sourceRange SourceRange, format string, a ...any) {
	checker.unit.rootFile.errorf(
		sourceRange,
		format,
		a...,
	)
}

func (checker *Checker) resolveSymbol(sym Symbol) {
	if sym.ResolveState() == ResolveStateResolved {
		return
	} else if sym.ResolveState() == ResolveStateResolving {
		checker.errorf(sym.SourceRange(), "symbol %v has a cyclic dependency", sym.Name())
	}

	sym.SetResolveState(ResolveStateResolving)
	switch symbol := sym.(type) {
	case *FuncSymbol:
		checker.resolveFuncSymbol(symbol)
	default:
		panic("unexpected symbol type")
	}
	sym.SetResolveState(ResolveStateResolved)

	switch symbol := sym.(type) {
	case *FuncSymbol:
		checker.resolveFuncBody(symbol)
	default:
		panic("unexpected symbol type")
	}
}

func (checker *Checker) resolveFuncSymbol(sym *FuncSymbol) {
	scope := checker.unit.semanticInfo.createScopeFor(sym, checker.currentScope(), sym.Name())

	checker.enterScope(scope)
	defer checker.leaveScope()

	funcDecl := sym.SymDecl.(*FuncDecl)

	for _, field := range funcDecl.Type.Parameters.Fields {
		for _, name := range field.Names {
			v := NewVarSymbol(name.Token, nil, name.SourceRange())
			v.Type = checker.resolveExpr(field.Type)
			v.SetResolveState(ResolveStateResolved)
			checker.addSymbol(v)
		}
	}

	// TODO: in the future we should resolve the function type and return it to caller
}

func (checker *Checker) resolveFuncBody(sym *FuncSymbol) {
	scope := checker.unit.semanticInfo.ScopeOf(sym)
	checker.enterScope(scope)
	defer checker.leaveScope()

	funcDecl := sym.SymDecl.(*FuncDecl)

	for _, stmt := range funcDecl.Body.Stmts {
		checker.resolveStmt(stmt)
	}
}

func (checker *Checker) resolveExpr(expr Expr) Type {
	if t := checker.unit.semanticInfo.TypeOf(expr); t != nil {
		return t
	}

	switch e := expr.(type) {
	case *NamedTypeExpr:
		return checker.resolveNamedTypeExpr(e)
	default:
		panic("unexpected expr type")
	}
}

func (checker *Checker) resolveNamedTypeExpr(e *NamedTypeExpr) Type {
	if e.Package.valid() {
		panic("we don't support packages yet")
	}
	return typeFromName(e.TypeName)
}

func typeFromName(name Token) Type {
	switch name.Value() {
	case "bool":
		return BuiltinBoolType
	case "int":
		return BuiltinIntType
	case "uint":
		return BuiltinUintType
	case "float32":
		return BuiltinFloat32Type
	case "float64":
		return BuiltinFloat64Type
	default:
		return BuiltinVoidType
	}
}

func (checker *Checker) resolveStmt(stmt Stmt) Type {
	switch s := stmt.(type) {
	case *ExprStmt:
		return checker.resolveExpr(s.Expr)
	default:
		panic("unexpected stmt type")
	}
}
