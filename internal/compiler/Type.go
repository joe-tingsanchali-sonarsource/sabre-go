package compiler

import "strings"

type Type interface {
	aType()
	Size() int
	Align() int
	Signed() bool
	HashKey() string
}

type VoidType struct{}

var BuiltinVoidType = &VoidType{}

func (VoidType) aType()          {}
func (VoidType) Size() int       { return 0 }
func (VoidType) Align() int      { return 0 }
func (VoidType) Signed() bool    { return false }
func (VoidType) HashKey() string { return "void" }

type BoolType struct{}

var BuiltinBoolType = &BoolType{}

func (BoolType) aType()          {}
func (BoolType) Size() int       { return 1 }
func (BoolType) Align() int      { return 4 }
func (BoolType) Signed() bool    { return false }
func (BoolType) HashKey() string { return "bool" }

type IntType struct{}

var BuiltinIntType = &IntType{}

func (IntType) aType()          {}
func (IntType) Size() int       { return 4 }
func (IntType) Align() int      { return 4 }
func (IntType) Signed() bool    { return true }
func (IntType) HashKey() string { return "int" }

type UintType struct{}

var BuiltinUintType = &UintType{}

func (UintType) aType()          {}
func (UintType) Size() int       { return 4 }
func (UintType) Align() int      { return 4 }
func (UintType) Signed() bool    { return false }
func (UintType) HashKey() string { return "uint" }

type Float32Type struct{}

var BuiltinFloat32Type = &Float32Type{}

func (Float32Type) aType()          {}
func (Float32Type) Size() int       { return 4 }
func (Float32Type) Align() int      { return 4 }
func (Float32Type) Signed() bool    { return true }
func (Float32Type) HashKey() string { return "float32" }

type Float64Type struct{}

var BuiltinFloat64Type = &Float64Type{}

func (Float64Type) aType()          {}
func (Float64Type) Size() int       { return 8 }
func (Float64Type) Align() int      { return 8 }
func (Float64Type) Signed() bool    { return true }
func (Float64Type) HashKey() string { return "float64" }

type StringType struct{}

var BuiltinStringType = &StringType{}

func (StringType) aType()          {}
func (StringType) Size() int       { return 0 }
func (StringType) Align() int      { return 0 }
func (StringType) Signed() bool    { return false }
func (StringType) HashKey() string { return "string" }

type FuncType struct {
	ArgTypes    []Type
	ReturnTypes []Type
}

func (FuncType) aType()       {}
func (FuncType) Size() int    { return 0 }
func (FuncType) Align() int   { return 0 }
func (FuncType) Signed() bool { return false }
func (f FuncType) HashKey() string {
	var b strings.Builder
	b.WriteString("func(")
	for i, a := range f.ArgTypes {
		if i > 0 {
			b.WriteRune(',')
		}
		b.WriteString(a.HashKey())
	}
	b.WriteString(")(")
	for i, a := range f.ReturnTypes {
		if i > 0 {
			b.WriteRune(',')
		}
		b.WriteString(a.HashKey())
	}
	b.WriteString(")")
	return b.String()
}

type TypeInterner struct {
	types map[string]Type
}

func NewTypeInterner() *TypeInterner {
	return &TypeInterner{
		types: make(map[string]Type),
	}
}

func (t *TypeInterner) InternFuncType(args []Type, returns []Type) Type {
	funcType := FuncType{
		ArgTypes:    args,
		ReturnTypes: returns,
	}
	key := funcType.HashKey()

	if v, ok := t.types[key]; ok {
		return v
	}

	t.types[key] = &funcType
	return &funcType
}
