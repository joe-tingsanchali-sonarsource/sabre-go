package compiler

type ResolveState byte

const (
	ResolveStateUnresolved ResolveState = iota
	ResolveStateResolving
	ResolveStateResolved
)

type Symbol interface {
	aSymbol()
	Scope() *Scope
	SetScope(scope *Scope)
	Name() string
	Decl() Decl
	SourceRange() SourceRange
	ResolveState() ResolveState
	SetResolveState(r ResolveState)
}

type SymbolBase struct {
	SymScope        *Scope
	SymName         string
	SymDecl         Decl
	SymSourceRange  SourceRange
	SymResolveState ResolveState
}

func (sym SymbolBase) Scope() *Scope {
	return sym.SymScope
}
func (sym *SymbolBase) SetScope(scope *Scope) {
	sym.SymScope = scope
}
func (sym SymbolBase) Name() string {
	return sym.SymName
}
func (sym SymbolBase) Decl() Decl {
	return sym.SymDecl
}
func (sym SymbolBase) SourceRange() SourceRange {
	return sym.SymSourceRange
}
func (sym SymbolBase) ResolveState() ResolveState {
	return sym.SymResolveState
}
func (sym *SymbolBase) SetResolveState(r ResolveState) {
	sym.SymResolveState = r
}

type FuncSymbol struct {
	SymbolBase
}

func (FuncSymbol) aSymbol() {}
func NewFuncSymbol(name Token, decl Decl, sourceRange SourceRange) *FuncSymbol {
	return &FuncSymbol{
		SymbolBase: SymbolBase{
			SymScope:       nil,
			SymName:        name.Value(),
			SymDecl:        decl,
			SymSourceRange: sourceRange,
		},
	}
}

type VarSymbol struct {
	SymbolBase
}

func (VarSymbol) aSymbol() {}
func NewVarSymbol(name Token, decl Decl, sourceRange SourceRange) *VarSymbol {
	return &VarSymbol{
		SymbolBase: SymbolBase{
			SymScope:       nil,
			SymName:        name.Value(),
			SymDecl:        decl,
			SymSourceRange: sourceRange,
		},
	}
}

type ConstSymbol struct {
	SymbolBase
}

func (ConstSymbol) aSymbol() {}
func NewConstSymbol(name Token, decl Decl, sourceRange SourceRange) *ConstSymbol {
	return &ConstSymbol{
		SymbolBase: SymbolBase{
			SymScope:       nil,
			SymName:        name.Value(),
			SymDecl:        decl,
			SymSourceRange: sourceRange,
		},
	}
}

type Scope struct {
	Parent *Scope
	Name   string
	Table  map[string]Symbol
}

func NewScope(parent *Scope, name string) *Scope {
	return &Scope{
		Parent: parent,
		Name:   name,
		Table:  make(map[string]Symbol),
	}
}

func (s Scope) ShallowFind(name string) Symbol {
	if symbol, ok := s.Table[name]; ok {
		return symbol
	}
	return nil
}

func (s *Scope) Add(sym Symbol) bool {
	if s.ShallowFind(sym.Name()) != nil {
		return false
	}

	s.Table[sym.Name()] = sym
	return true
}

func (s Scope) Find(name string) Symbol {
	for it := &s; it != nil; it = it.Parent {
		if sym := it.ShallowFind(name); sym != nil {
			return sym
		}
	}
	return nil
}
