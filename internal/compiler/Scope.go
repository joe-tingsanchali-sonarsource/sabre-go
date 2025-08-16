package compiler

type ResolveState byte

const (
	ResolveStateUnresolved ResolveState = iota
	ResolveStateResolving
	ResolveStateResolved
)

type Symbol interface {
	aSymbol()
	Name() string
	SourceRange() SourceRange
	Scope() *Scope
	SetScope(scope *Scope)
	ResolveState() ResolveState
	SetResolveState(r ResolveState)
}

type SymbolBase struct {
	SymScope        *Scope
	SymName         string
	Decl            Decl
	Type            Type
	SymResolveState ResolveState
}

func (sym SymbolBase) Name() string {
	return sym.SymName
}
func (sym SymbolBase) SourceRange() SourceRange {
	return sym.Decl.SourceRange()
}
func (sym SymbolBase) Scope() *Scope {
	return sym.SymScope
}
func (sym *SymbolBase) SetScope(scope *Scope) {
	sym.SymScope = scope
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
func NewFuncSymbol(name Token, decl Decl) *FuncSymbol {
	return &FuncSymbol{
		SymbolBase: SymbolBase{
			SymScope: nil,
			SymName:  name.Value(),
			Decl:     decl,
			Type:     BuiltinVoidType,
		},
	}
}

type VarSymbol struct {
	SymbolBase
}

func (VarSymbol) aSymbol() {}
func NewVarSymbol(name Token, decl Decl) *VarSymbol {
	return &VarSymbol{
		SymbolBase: SymbolBase{
			SymScope: nil,
			SymName:  name.Value(),
			Decl:     decl,
			Type:     BuiltinVoidType,
		},
	}
}

type ConstSymbol struct {
	SymbolBase
}

func (ConstSymbol) aSymbol() {}
func NewConstSymbol(name Token, decl Decl) *ConstSymbol {
	return &ConstSymbol{
		SymbolBase: SymbolBase{
			SymScope: nil,
			SymName:  name.Value(),
			Decl:     decl,
			Type:     BuiltinVoidType,
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
	for it := &s; it != nil; it = s.Parent {
		if sym := it.ShallowFind(name); sym != nil {
			return sym
		}
	}
	return nil
}
