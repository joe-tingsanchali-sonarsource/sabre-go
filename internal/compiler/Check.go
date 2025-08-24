package compiler

import (
	"go/constant"
	"go/token"
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

func (a AddressMode) Combine(b AddressMode) AddressMode {
	switch a {
	case AddressModeInvalid, AddressModeNoValue, AddressModeType:
		return a
	case AddressModeConstant:
		switch b {
		case AddressModeConstant:
			return AddressModeConstant
		case AddressModeVariable, AddressModeComputedValue:
			return AddressModeComputedValue
		default:
			return b
		}
	case AddressModeVariable, AddressModeComputedValue:
		switch b {
		case AddressModeConstant, AddressModeVariable, AddressModeComputedValue:
			return AddressModeComputedValue
		default:
			return b
		}
	default:
		panic("unexpected AddressMode")
	}
}

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

func convertTokenToConstantToken(op TokenKind) token.Token {
	switch op {
	case TokenLT:
		return token.LSS
	case TokenGT:
		return token.GTR
	case TokenLE:
		return token.LEQ
	case TokenGE:
		return token.GEQ
	case TokenEQ:
		return token.EQL
	case TokenNE:
		return token.NEQ
	case TokenAdd:
		return token.ADD
	case TokenSub:
		return token.SUB
	case TokenMul:
		return token.MUL
	case TokenDiv:
		return token.QUO
	case TokenMod:
		return token.REM
	case TokenLOr:
		return token.LOR
	case TokenLAnd:
		return token.LAND
	case TokenXor:
		return token.XOR
	case TokenOr:
		return token.OR
	case TokenNot:
		return token.NOT
	case TokenAnd:
		return token.AND
	case TokenAndNot:
		return token.AND_NOT
	case TokenShl:
		return token.SHL
	case TokenShr:
		return token.SHR
	default:
		panic("unexpected binary operator token")
	}
}

func (a *TypeAndValue) UnaryOp(op TokenKind) (res *TypeAndValue) {
	res = &TypeAndValue{
		Mode: a.Mode,
		Type: a.Type,
	}
	if res.Mode == AddressModeConstant {
		res.Value = constant.UnaryOp(convertTokenToConstantToken(op), a.Value, 0)
	} else {
		res.Mode = AddressModeComputedValue
	}
	return
}

func (a *TypeAndValue) BinaryOpWithType(op TokenKind, b *TypeAndValue, t Type) (res *TypeAndValue) {
	res = &TypeAndValue{
		Mode: a.Mode.Combine(b.Mode),
		Type: t,
	}
	if res.Mode == AddressModeConstant {
		res.Value = constant.BinaryOp(a.Value, convertTokenToConstantToken(op), b.Value)
	}
	return
}

func (a *TypeAndValue) CompareWithType(op TokenKind, b *TypeAndValue, t Type) (res *TypeAndValue) {
	res = &TypeAndValue{
		Mode: a.Mode.Combine(b.Mode),
		Type: t,
	}
	if res.Mode == AddressModeConstant {
		res.Value = constant.MakeBool(constant.Compare(a.Value, convertTokenToConstantToken(op), b.Value))
	}
	return
}

func (a *TypeAndValue) ShiftWithType(op TokenKind, b *TypeAndValue, t Type) (res *TypeAndValue) {
	res = &TypeAndValue{
		Mode: a.Mode.Combine(b.Mode),
		Type: t,
	}
	if res.Mode == AddressModeConstant {
		v, ok := constant.Int64Val(b.Value)
		if !ok || v < 0 {
			panic("unexpected shift value")
		}
		res.Value = constant.Shift(a.Value, convertTokenToConstantToken(op), uint(v))
	}
	return
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

func (checker *Checker) findScopeWithName(name string) *Scope {
	for i := len(checker.scopeStack) - 1; i >= 0; i-- {
		scope := checker.scopeStack[i]
		if scope.Name == name {
			return scope
		}
	}
	return nil
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
	case *UnaryExpr:
		t = checker.resolveUnaryExpr(e)
	case *BinaryExpr:
		t = checker.resolveBinaryExpr(e)
	case *NamedTypeExpr:
		t = checker.resolveNamedTypeExpr(e)
	case *CallExpr:
		t = checker.resolveCallExpr(e)
	case *ArrayTypeExpr:
		t = checker.resolveArrayTypeExpr(e)
	case *FuncTypeExpr:
		t = checker.resolveFuncTypeExpr(e)
	default:
		panic("unexpected expr type")
	}

	checker.unit.semanticInfo.SetTypeOf(expr, t)
	return t
}

func (checker *Checker) resolveAndUnpackTypesFromExprList(exprs []Expr) (types []Type, sourceRanges []SourceRange) {
	if len(exprs) == 1 {
		e := exprs[0]
		switch t := checker.resolveExpr(e).Type.(type) {
		case *TupleType:
			for _, tt := range t.Types {
				types = append(types, tt)
				sourceRanges = append(sourceRanges, e.SourceRange())
			}
		default:
			types = append(types, t)
			sourceRanges = append(sourceRanges, e.SourceRange())
		}
	} else {
		for _, e := range exprs {
			types = append(types, checker.resolveExpr(e).Type)
			sourceRanges = append(sourceRanges, e.SourceRange())
		}
	}
	return
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

func (checker *Checker) resolveBinaryExpr(e *BinaryExpr) *TypeAndValue {
	lhsType := checker.resolveExpr(e.LHS)
	rhsType := checker.resolveExpr(e.RHS)

	equalTypes := func(e Expr, lhsType, rhsType Type) bool {
		if lhsType != rhsType {
			checker.error(NewError(
				e.SourceRange(),
				"type mismatch in binary expression, lhs is '%v' and rhs is '%v'",
				lhsType,
				rhsType,
			))
			return false
		}
		return true
	}

	hasTypeProperty := func(e Expr, t Type, hasFeature bool, capName string) bool {
		if !hasFeature {
			checker.error(NewError(
				e.SourceRange(),
				"type '%v' doesn't support %v",
				t,
				capName,
			))
			return false
		}
		return true
	}

	invalidResult := &TypeAndValue{
		Mode: AddressModeInvalid,
		Type: BuiltinVoidType,
	}

	switch e.Operator.Kind() {
	case TokenOr, TokenAnd, TokenXor, TokenAndNot:
		if equalTypes(e, lhsType.Type, rhsType.Type) &&
			hasTypeProperty(e.LHS, lhsType.Type, lhsType.Type.Properties().HasBitOps, "bitwise operations") &&
			hasTypeProperty(e.RHS, rhsType.Type, rhsType.Type.Properties().HasBitOps, "bitwise operations") {
			return lhsType.BinaryOpWithType(e.Operator.Kind(), rhsType, lhsType.Type)
		}
	case TokenAdd, TokenSub, TokenMul, TokenDiv, TokenMod:
		if equalTypes(e, lhsType.Type, rhsType.Type) &&
			hasTypeProperty(e.LHS, lhsType.Type, lhsType.Type.Properties().HasArithmetic, "arithmetic operations") &&
			hasTypeProperty(e.RHS, rhsType.Type, rhsType.Type.Properties().HasArithmetic, "arithmetic operations") {
			return lhsType.BinaryOpWithType(e.Operator.Kind(), rhsType, lhsType.Type)
		}
	case TokenLOr, TokenLAnd:
		if equalTypes(e, lhsType.Type, rhsType.Type) &&
			hasTypeProperty(e.LHS, lhsType.Type, lhsType.Type.Properties().HasLogicOps, "logic operations") &&
			hasTypeProperty(e.RHS, rhsType.Type, rhsType.Type.Properties().HasLogicOps, "logic operations") {
			return lhsType.BinaryOpWithType(e.Operator.Kind(), rhsType, lhsType.Type)
		}
	case TokenLT, TokenGT, TokenLE, TokenGE:
		if equalTypes(e, lhsType.Type, rhsType.Type) &&
			hasTypeProperty(e.LHS, lhsType.Type, lhsType.Type.Properties().HasCompare, "compare operations") &&
			hasTypeProperty(e.RHS, rhsType.Type, rhsType.Type.Properties().HasCompare, "compare operations") {
			return lhsType.CompareWithType(e.Operator.Kind(), rhsType, BuiltinBoolType)
		}
	case TokenEQ, TokenNE:
		if equalTypes(e, lhsType.Type, rhsType.Type) &&
			hasTypeProperty(e.LHS, lhsType.Type, lhsType.Type.Properties().HasEquality, "equality operations") &&
			hasTypeProperty(e.RHS, rhsType.Type, rhsType.Type.Properties().HasEquality, "equality operations") {
			return lhsType.CompareWithType(e.Operator.Kind(), rhsType, BuiltinBoolType)
		}
	case TokenShl, TokenShr:
		if !rhsType.Type.Properties().Integral {
			checker.error(NewError(
				e.RHS.SourceRange(),
				"shift operator should be integral type instead of '%v'",
				rhsType.Type,
			))
			return invalidResult
		}

		if rhsType.Mode == AddressModeConstant && constant.Compare(rhsType.Value, token.LSS, constant.MakeInt64(0)) {
			checker.error(NewError(
				e.RHS.SourceRange(),
				"shift operator should not be negative, but it has value '%v'",
				rhsType.Value,
			))
			return invalidResult
		}

		if hasTypeProperty(e.LHS, lhsType.Type, lhsType.Type.Properties().HasBitOps, "bitwise operations") {
			return lhsType.ShiftWithType(e.Operator.Kind(), rhsType, lhsType.Type)
		}
	default:
		panic("unexpected binary operator")
	}
	return invalidResult
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

func (checker *Checker) resolveUnaryExpr(e *UnaryExpr) *TypeAndValue {
	t := checker.resolveExpr(e.Base)

	invalidResult := &TypeAndValue{
		Mode:  AddressModeInvalid,
		Type:  BuiltinVoidType,
		Value: nil,
	}

	hasTypeProperty := func(e Expr, t Type, hasFeature bool, capName string) bool {
		if !hasFeature {
			checker.error(NewError(
				e.SourceRange(),
				"type '%v' doesn't support %v",
				t,
				capName,
			))
			return false
		}
		return true
	}

	switch e.Operator.Kind() {
	case TokenAdd:
		fallthrough
	case TokenSub:
		if !hasTypeProperty(e.Base, t.Type, t.Type.Properties().HasArithmetic, "arithmetic operations") {
			return invalidResult
		}
	case TokenNot:
		if !hasTypeProperty(e.Base, t.Type, t.Type.Properties().HasLogicOps, "logic operations") {
			return invalidResult
		}
	case TokenXor:
		if !hasTypeProperty(e.Base, t.Type, t.Type.Properties().HasBitOps, "bitwise operations") {
			return invalidResult
		}
	default:
		panic("invalid unary operator")
	}

	return t.UnaryOp(e.Operator.Kind())
}

func (checker *Checker) resolveCallExpr(e *CallExpr) *TypeAndValue {
	t := checker.resolveExpr(e.Base)

	res := &TypeAndValue{
		Mode:  AddressModeInvalid,
		Type:  BuiltinVoidType,
		Value: nil,
	}

	funcType, ok := t.Type.(*FuncType)
	if !ok {
		checker.error(NewError(e.SourceRange(), "invalid call expression, expected function type but found '%v'", t.Type))
		return res
	}

	arguments, sourceRanges := checker.resolveAndUnpackTypesFromExprList(e.Args)
	if len(arguments) != len(funcType.ParameterTypes) {
		checker.error(NewError(e.SourceRange(), "expected %v arguments, but found %v", len(funcType.ParameterTypes), len(e.Args)).
			Note(e.SourceRange(), "have %v, want %v", TupleType{Types: arguments}, TupleType{Types: funcType.ParameterTypes}),
		)
		return res
	}

	for i, a := range arguments {
		parameterType := funcType.ParameterTypes[i]
		if a != parameterType {
			checker.error(NewError(sourceRanges[i], "incorrect argument type '%v', expected '%v'", a, parameterType))
			return res
		}
	}

	res.Mode = AddressModeComputedValue
	if len(funcType.ReturnTypes) == 1 {
		res.Type = funcType.ReturnTypes[0]
	} else if len(funcType.ReturnTypes) > 1 {
		res.Type = checker.unit.semanticInfo.TypeInterner.InternTupleType(funcType.ReturnTypes)
	}
	return res
}

func (checker *Checker) resolveArrayTypeExpr(e *ArrayTypeExpr) *TypeAndValue {
	elementType := checker.resolveExpr(e.ElementType)

	res := &TypeAndValue{
		Mode:  AddressModeType,
		Type:  BuiltinVoidType,
		Value: nil,
	}

	lengthAsInt := 0
	if e.Length != nil {
		lengthType := checker.resolveExpr(e.Length)
		if lengthType.Mode != AddressModeConstant {
			checker.error(NewError(e.Length.SourceRange(), "array type length should be constant"))
			return res
		}
		if lengthType.Value != nil {
			if lengthType.Value.Kind() != constant.Int {
				checker.error(NewError(e.Length.SourceRange(), "array type length should be integer"))
				return res
			}
			valueAsInt, exact := constant.Int64Val(lengthType.Value)
			if !exact {
				checker.error(NewError(e.Length.SourceRange(), "array type length does not fit in 64bit integer"))
				return res
			}
			lengthAsInt = int(valueAsInt)
		}
	}

	res.Type = checker.unit.semanticInfo.TypeInterner.InternArrayType(lengthAsInt, elementType.Type)
	return res
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

	parameterTypes := processFields(e.Parameters.Fields)
	var returnTypes []Type
	if e.Result != nil {
		returnTypes = processFields(e.Result.Fields)
	}

	return &TypeAndValue{
		Mode:  AddressModeType,
		Type:  checker.unit.semanticInfo.TypeInterner.InternFuncType(parameterTypes, returnTypes),
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
	case *BreakStmt:
		checker.resolveBreakStmt(s)
	case *FallthroughStmt:
		checker.resolveFallthroughStmt(s)
	case *ContinueStmt:
		checker.resolveContinueStmt(s)
	case *BlockStmt:
		checker.resolveBlockStmt(s)
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

	returnTypes, sourceRanges := checker.resolveAndUnpackTypesFromExprList(s.Exprs)
	expectedReturnTypes := checker.unit.semanticInfo.TypeOf(funcDecl).Type.(*FuncType).ReturnTypes
	if len(returnTypes) == len(expectedReturnTypes) {
		for i, et := range expectedReturnTypes {
			if t := returnTypes[i]; t != et {
				checker.error(NewError(sourceRanges[i], "incorrect return type '%v', expected '%v'", t, et))
			}
		}
	} else {
		named := funcDecl.Type.Result != nil && len(funcDecl.Type.Result.Fields[0].Names) > 0
		if len(returnTypes) != 0 || !named {
			checker.error(NewError(s.SourceRange(), "expected %v return values, but found %v", len(expectedReturnTypes), len(returnTypes)).
				Note(s.SourceRange(), "have %v, want %v", TupleType{Types: returnTypes}, TupleType{Types: expectedReturnTypes}),
			)
		}
	}
}

func (checker *Checker) resolveBlockStmt(s *BlockStmt) {
	scope := checker.unit.semanticInfo.createScopeFor(s, checker.currentScope(), "block")
	checker.enterScope(scope)
	defer checker.leaveScope()

	for _, stmt := range s.Stmts {
		checker.resolveStmt(stmt)
	}
}

func (checker *Checker) resolveBreakStmt(s *BreakStmt) {
	if s.Label.valid() {
		panic("labeled break not supported yet")
	}

	switchScope := checker.findScopeWithName("switch")
	if switchScope != nil {
		return
	}

	forScope := checker.findScopeWithName("for")
	if forScope != nil {
		return
	}

	checker.error(NewError(s.SourceRange(), "break statement not within loop or switch"))
}

// TODO:
// Once we have switch cases and for loops implemented, we need to add the following checks for fallthrough statements:
// Check fallthrough is the last statement in a switch case and that the next case exists.
// Check fallthrough is not in the default case.
func (checker *Checker) resolveFallthroughStmt(s *FallthroughStmt) {
	switchScope := checker.findScopeWithName("switch")
	if switchScope != nil {
		return
	}

	checker.error(NewError(s.SourceRange(), "fallthrough statement not within switch"))
}

func (checker *Checker) resolveContinueStmt(s *ContinueStmt) {
	if s.Label.valid() {
		panic("labeled continue not supported yet")
	}

	forScope := checker.findScopeWithName("for")
	if forScope != nil {
		return
	}

	checker.error(NewError(s.SourceRange(), "continue statement not within for loop"))
}
