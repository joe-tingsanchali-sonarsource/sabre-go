package compiler

import (
	"go/constant"
)

type SemanticInfo struct {
	Types        map[any]*TypeAndValue
	Scopes       map[any]*Scope
	TypeInterner *TypeInterner
}

func NewSemanticInfo() *SemanticInfo {
	return &SemanticInfo{
		Types:        make(map[any]*TypeAndValue),
		Scopes:       make(map[any]*Scope),
		TypeInterner: NewTypeInterner(),
	}
}

func (info *SemanticInfo) SetTypeOf(e any, t *TypeAndValue) {
	info.Types[e] = t
}

func (info SemanticInfo) TypeOf(e any) *TypeAndValue {
	if t, ok := info.Types[e]; ok {
		return t
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
	return sym
}

func (checker *Checker) errorf(sourceRange SourceRange, format string, a ...any) {
	checker.unit.rootFile.errorf(
		sourceRange,
		format,
		a...,
	)
}

func (checker *Checker) resolveSymbol(sym Symbol) *TypeAndValue {
	if sym.ResolveState() == ResolveStateResolved {
		return checker.unit.semanticInfo.TypeOf(sym)
	} else if sym.ResolveState() == ResolveStateResolving {
		checker.errorf(sym.SourceRange(), "symbol %v has a cyclic dependency", sym.Name())
	}

	var symType *TypeAndValue
	sym.SetResolveState(ResolveStateResolving)
	switch symbol := sym.(type) {
	case *FuncSymbol:
		symType = checker.resolveFuncSymbol(symbol)
	default:
		panic("unexpected symbol type")
	}
	sym.SetResolveState(ResolveStateResolved)

	checker.unit.semanticInfo.SetTypeOf(sym, symType)

	switch symbol := sym.(type) {
	case *FuncSymbol:
		checker.resolveFuncBody(symbol)
	default:
		panic("unexpected symbol type")
	}

	return symType
}

func (checker *Checker) resolveFuncSymbol(sym *FuncSymbol) *TypeAndValue {
	scope := checker.unit.semanticInfo.createScopeFor(sym, checker.currentScope(), sym.Name())

	checker.enterScope(scope)
	defer checker.leaveScope()

	funcDecl := sym.SymDecl.(*FuncDecl)

	processFields := func(fields []Field) (types []Type) {
		for _, field := range fields {
			for _, name := range field.Names {
				v := NewVarSymbol(name.Token, nil, name.SourceRange())
				v.SetResolveState(ResolveStateResolved)
				fieldType := checker.resolveExpr(field.Type)
				checker.unit.semanticInfo.SetTypeOf(v, fieldType)
				checker.addSymbol(v)
				types = append(types, fieldType.Type)
			}
		}
		return types
	}

	argTypes := processFields(funcDecl.Type.Parameters.Fields)
	var returnTypes []Type
	if funcDecl.Type.Result != nil {
		returnTypes = processFields(funcDecl.Type.Result.Fields)
	}

	funcType := &TypeAndValue{
		Mode:  AddressModeType,
		Type:  checker.unit.semanticInfo.TypeInterner.InternFuncType(argTypes, returnTypes),
		Value: nil,
	}

	checker.unit.semanticInfo.SetTypeOf(sym.SymDecl, funcType)
	return funcType
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

func (checker *Checker) resolveExpr(expr Expr) (t *TypeAndValue) {
	if exprType := checker.unit.semanticInfo.TypeOf(expr); exprType != nil {
		return exprType
	}

	switch e := expr.(type) {
	case *NamedTypeExpr:
		t = checker.resolveNamedTypeExpr(e)
	default:
		panic("unexpected expr type")
	}

	checker.unit.semanticInfo.SetTypeOf(expr, t)
	return t
}

func (checker *Checker) resolveNamedTypeExpr(e *NamedTypeExpr) *TypeAndValue {
	if e.Package.valid() {
		panic("we don't support packages yet")
	}
	return &TypeAndValue{
		Mode:  AddressModeType,
		Type:  typeFromName(e.TypeName),
		Value: nil,
	}
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

func (checker *Checker) resolveStmt(stmt Stmt) {
	switch s := stmt.(type) {
	case *ExprStmt:
		checker.resolveExpr(s.Expr)
	default:
		panic("unexpected stmt type")
	}
}
