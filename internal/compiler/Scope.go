package compiler

type Symbol interface {
	aSymbol()
	Name() string
}

type SymbolBase struct {
	Scope   *Scope
	SymName string
	Decl    Decl
	Type    Type
}

func (sym SymbolBase) Name() string {
	return sym.SymName
}

type FuncSymbol struct {
	SymbolBase
}

func (FuncSymbol) aSymbol() {}
func NewFuncSymbol(name Token, decl Decl) *FuncSymbol {
	return &FuncSymbol{
		SymbolBase: SymbolBase{
			Scope:   nil,
			SymName: name.Value(),
			Decl:    decl,
			Type:    BuiltinVoidType,
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
			Scope:   nil,
			SymName: name.Value(),
			Decl:    decl,
			Type:    BuiltinVoidType,
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
			Scope:   nil,
			SymName: name.Value(),
			Decl:    decl,
			Type:    BuiltinVoidType,
		},
	}
}

type Scope struct {
	Parent *Scope
	Name   string
	Table  map[string]Symbol
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
