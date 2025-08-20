package compiler

import (
	"go/constant"
	"strconv"
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
	unit          *Unit
	scopeStack    []*Scope
	functionStack []*FuncDecl
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

func (checker *Checker) currentFunction() *FuncDecl {
	return checker.functionStack[len(checker.functionStack)-1]
}

func (checker *Checker) enterFunction(function *FuncDecl) {
	if function == nil {
		panic("entering nil function")
	}
	checker.functionStack = append(checker.functionStack, function)
}

func (checker *Checker) leaveFunction() {
	checker.functionStack = checker.functionStack[:len(checker.functionStack)-1]
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
		checker.error(
			NewError(sym.SourceRange(), "symbol '%v' redefinition", sym.Name()).
				Note(oldSym.SourceRange(), "first declared here"),
		)
		return oldSym
	}
	scope.Add(sym)
	sym.SetScope(scope)
	return sym
}

func (checker *Checker) error(e Error) {
	checker.unit.rootFile.error(e)
}

func (checker *Checker) resolveSymbol(sym Symbol) *TypeAndValue {
	if sym.ResolveState() == ResolveStateResolved {
		return checker.unit.semanticInfo.TypeOf(sym)
	} else if sym.ResolveState() == ResolveStateResolving {
		checker.error(
			NewError(sym.SourceRange(), "symbol %v has a cyclic dependency", sym.Name()),
		)
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

	checker.enterFunction(funcDecl)
	defer checker.leaveFunction()

	funcType := checker.resolveFuncTypeExpr(funcDecl.Type)

	checker.unit.semanticInfo.SetTypeOf(sym.SymDecl, funcType)
	return funcType
}

func (checker *Checker) resolveFuncBody(sym *FuncSymbol) {
	scope := checker.unit.semanticInfo.ScopeOf(sym)
	checker.enterScope(scope)
	defer checker.leaveScope()

	funcDecl := sym.SymDecl.(*FuncDecl)
	checker.enterFunction(funcDecl)
	defer checker.leaveFunction()

	for _, stmt := range funcDecl.Body.Stmts {
		checker.resolveStmt(stmt)
	}
}

func (checker *Checker) resolveExpr(expr Expr) (t *TypeAndValue) {
	if exprType := checker.unit.semanticInfo.TypeOf(expr); exprType != nil {
		return exprType
	}

	switch e := expr.(type) {
	case *LiteralExpr:
		t = checker.resolveLiteralExpr(e)
	case *IdentifierExpr:
		t = checker.resolveIdentifierExpr(e)
	case *ParenExpr:
		t = checker.resolveParenExpr(e)
	case *NamedTypeExpr:
		t = checker.resolveNamedTypeExpr(e)
	case *FuncTypeExpr:
		t = checker.resolveFuncTypeExpr(e)
	default:
		panic("unexpected expr type")
	}

	checker.unit.semanticInfo.SetTypeOf(expr, t)
	return t
}

func (checker *Checker) resolveLiteralExpr(e *LiteralExpr) *TypeAndValue {
	switch e.Token.Kind() {
	case TokenLiteralInt:
		i, err := strconv.ParseInt(e.Token.Value(), 0, 64)
		if err == nil {
			return &TypeAndValue{
				Mode:  AddressModeConstant,
				Type:  BuiltinIntType,
				Value: constant.MakeInt64(i),
			}
		} else {
			checker.error(NewError(e.Token.SourceRange(), "invalid integer value").
				Note(e.Token.SourceRange(), "%v", err),
			)
			return &TypeAndValue{
				Mode:  AddressModeInvalid,
				Type:  BuiltinVoidType,
				Value: nil,
			}
		}
	case TokenLiteralFloat:
		f, err := strconv.ParseFloat(e.Token.Value(), 64)
		if err == nil {
			return &TypeAndValue{
				Mode:  AddressModeConstant,
				Type:  BuiltinFloat32Type,
				Value: constant.MakeFloat64(f),
			}
		} else {
			checker.error(NewError(e.Token.SourceRange(), "invalid float value").
				Note(e.Token.SourceRange(), "%v", err),
			)
			return &TypeAndValue{
				Mode:  AddressModeInvalid,
				Type:  BuiltinVoidType,
				Value: nil,
			}
		}
	case TokenTrue:
		return &TypeAndValue{
			Mode:  AddressModeConstant,
			Type:  BuiltinBoolType,
			Value: constant.MakeBool(true),
		}
	case TokenFalse:
		return &TypeAndValue{
			Mode:  AddressModeConstant,
			Type:  BuiltinBoolType,
			Value: constant.MakeBool(false),
		}
	default:
		return &TypeAndValue{
			Mode:  AddressModeInvalid,
			Type:  BuiltinVoidType,
			Value: nil,
		}
	}
}

func (checker *Checker) resolveIdentifierExpr(e *IdentifierExpr) *TypeAndValue {
	scope := checker.currentScope()
	symbol := scope.Find(e.Token.Value())
	if symbol == nil {
		checker.error(NewError(e.SourceRange(), "undeclared identifier"))
		return &TypeAndValue{
			Mode:  AddressModeInvalid,
			Type:  BuiltinVoidType,
			Value: nil,
		}
	}

	return checker.resolveSymbol(symbol)
}

func (checker *Checker) resolveParenExpr(e *ParenExpr) *TypeAndValue {
	return checker.resolveExpr(e.Base)
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

func (checker *Checker) resolveFuncTypeExpr(e *FuncTypeExpr) *TypeAndValue {
	processFields := func(fields []Field) (types []Type) {
		for _, field := range fields {
			fieldType := checker.resolveExpr(field.Type)
			if len(field.Names) > 0 {
				for _, name := range field.Names {
					v := NewVarSymbol(name.Token, nil, name.SourceRange())
					v.SetResolveState(ResolveStateResolved)
					checker.unit.semanticInfo.SetTypeOf(v, fieldType)
					checker.addSymbol(v)
					types = append(types, fieldType.Type)
				}
			} else {
				types = append(types, fieldType.Type)
			}
		}
		return types
	}

	argTypes := processFields(e.Parameters.Fields)
	var returnTypes []Type
	if e.Result != nil {
		returnTypes = processFields(e.Result.Fields)
	}

	return &TypeAndValue{
		Mode:  AddressModeType,
		Type:  checker.unit.semanticInfo.TypeInterner.InternFuncType(argTypes, returnTypes),
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
	case *ReturnStmt:
		checker.resolveReturnStmt(s)
	default:
		panic("unexpected stmt type")
	}
}

func (checker *Checker) resolveReturnStmt(s *ReturnStmt) {
	funcDecl := checker.currentFunction()
	if funcDecl == nil {
		checker.error(NewError(s.SourceRange(), "unexpected return statement"))
		return
	}

	var returnTypes []Type
	for _, e := range s.Exprs {
		returnTypes = append(returnTypes, checker.resolveExpr(e).Type)
	}

	expectedReturnTypes := checker.unit.semanticInfo.TypeOf(funcDecl).Type.(*FuncType).ReturnTypes
	if len(returnTypes) == len(expectedReturnTypes) {
		for i, et := range expectedReturnTypes {
			t := returnTypes[i]
			if t != et {
				checker.error(NewError(s.Exprs[i].SourceRange(), "incorrect return type '%v', expected '%v'", t, et))
			}
		}
	} else {
		named := funcDecl.Type.Result != nil && len(funcDecl.Type.Result.Fields[0].Names) > 0
		if len(returnTypes) != 0 || !named {
			checker.error(NewError(s.SourceRange(), "expected %v return values, but found %v", len(expectedReturnTypes), len(returnTypes)))
		}
	}
}
