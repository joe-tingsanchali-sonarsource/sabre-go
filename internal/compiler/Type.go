package compiler

import (
	"fmt"
	"strings"
)

type Type interface {
	aType()
	Size() int
	Align() int
	Signed() bool
	String() string
	HashKey() string
}

type VoidType struct{}

var BuiltinVoidType = &VoidType{}

func (VoidType) aType()            {}
func (VoidType) Size() int         { return 0 }
func (VoidType) Align() int        { return 0 }
func (VoidType) Signed() bool      { return false }
func (VoidType) String() string    { return "void" }
func (t VoidType) HashKey() string { return t.String() }

type BoolType struct{}

var BuiltinBoolType = &BoolType{}

func (BoolType) aType()            {}
func (BoolType) Size() int         { return 1 }
func (BoolType) Align() int        { return 4 }
func (BoolType) Signed() bool      { return false }
func (BoolType) String() string    { return "bool" }
func (t BoolType) HashKey() string { return t.String() }

type IntType struct{}

var BuiltinIntType = &IntType{}

func (IntType) aType()            {}
func (IntType) Size() int         { return 4 }
func (IntType) Align() int        { return 4 }
func (IntType) Signed() bool      { return true }
func (IntType) String() string    { return "int" }
func (t IntType) HashKey() string { return t.String() }

type UintType struct{}

var BuiltinUintType = &UintType{}

func (UintType) aType()            {}
func (UintType) Size() int         { return 4 }
func (UintType) Align() int        { return 4 }
func (UintType) Signed() bool      { return false }
func (UintType) String() string    { return "uint" }
func (t UintType) HashKey() string { return t.String() }

type Float32Type struct{}

var BuiltinFloat32Type = &Float32Type{}

func (Float32Type) aType()            {}
func (Float32Type) Size() int         { return 4 }
func (Float32Type) Align() int        { return 4 }
func (Float32Type) Signed() bool      { return true }
func (Float32Type) String() string    { return "float32" }
func (t Float32Type) HashKey() string { return t.String() }

type Float64Type struct{}

var BuiltinFloat64Type = &Float64Type{}

func (Float64Type) aType()            {}
func (Float64Type) Size() int         { return 8 }
func (Float64Type) Align() int        { return 8 }
func (Float64Type) Signed() bool      { return true }
func (Float64Type) String() string    { return "float64" }
func (t Float64Type) HashKey() string { return t.String() }

type StringType struct{}

var BuiltinStringType = &StringType{}

func (StringType) aType()            {}
func (StringType) Size() int         { return 0 }
func (StringType) Align() int        { return 0 }
func (StringType) Signed() bool      { return false }
func (StringType) String() string    { return "string" }
func (t StringType) HashKey() string { return t.String() }

type FuncType struct {
	ArgTypes    []Type
	ReturnTypes []Type
}

func (FuncType) aType()       {}
func (FuncType) Size() int    { return 0 }
func (FuncType) Align() int   { return 0 }
func (FuncType) Signed() bool { return false }
func (t FuncType) String() string {
	var b strings.Builder
	b.WriteString("func(")
	for i, a := range t.ArgTypes {
		if i > 0 {
			b.WriteRune(',')
		}
		b.WriteString(a.HashKey())
	}
	b.WriteString(")(")
	for i, a := range t.ReturnTypes {
		if i > 0 {
			b.WriteRune(',')
		}
		b.WriteString(a.HashKey())
	}
	b.WriteString(")")
	return b.String()
}
func (t FuncType) HashKey() string { return t.String() }

type ArrayType struct {
	Length      int
	ElementType Type
}

func (ArrayType) aType()         {}
func (t ArrayType) Size() int    { return t.ElementType.Size() * t.Length }
func (t ArrayType) Align() int   { return t.ElementType.Align() }
func (t ArrayType) Signed() bool { return t.ElementType.Signed() }
func (t ArrayType) String() string {
	return fmt.Sprintf("[%v]%v", t.Length, t.ElementType.String())
}
func (t ArrayType) HashKey() string {
	return fmt.Sprintf("[%v]%v", t.Length, t.ElementType.HashKey())
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

func (t *TypeInterner) InternArrayType(length int, elementType Type) Type {
	arrayType := ArrayType{
		Length:      length,
		ElementType: elementType,
	}
	key := arrayType.HashKey()

	if v, ok := t.types[key]; ok {
		return v
	}

	t.types[key] = &arrayType
	return &arrayType
}
