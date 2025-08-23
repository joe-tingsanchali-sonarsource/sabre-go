package compiler

import (
	"fmt"
	"strings"
)

type TypeProperties struct {
	Size, Align                           int
	Signed, Integral, Floating            bool
	HasBitOps, HasArithmetic, HasLogicOps bool
	HasCompare, HasEquality               bool
}

type Type interface {
	aType()
	Properties() TypeProperties
	String() string
	HashKey() string
}

type VoidType struct{}

var BuiltinVoidType = &VoidType{}

func (VoidType) aType() {}
func (VoidType) Properties() TypeProperties {
	return TypeProperties{}
}
func (VoidType) String() string    { return "void" }
func (t VoidType) HashKey() string { return t.String() }

type BoolType struct{}

var BuiltinBoolType = &BoolType{}

func (BoolType) aType() {}
func (BoolType) Properties() TypeProperties {
	return TypeProperties{
		Size:        1,
		Align:       4,
		HasLogicOps: true,
		HasEquality: true,
	}
}
func (BoolType) String() string    { return "bool" }
func (t BoolType) HashKey() string { return t.String() }

type IntType struct{}

var BuiltinIntType = &IntType{}

func (IntType) aType() {}
func (IntType) Properties() TypeProperties {
	return TypeProperties{
		Size:          4,
		Align:         4,
		Signed:        true,
		Integral:      true,
		HasBitOps:     true,
		HasArithmetic: true,
		HasCompare:    true,
		HasEquality:   true,
	}
}
func (IntType) String() string    { return "int" }
func (t IntType) HashKey() string { return t.String() }

type UintType struct{}

var BuiltinUintType = &UintType{}

func (UintType) aType() {}
func (UintType) Properties() TypeProperties {
	return TypeProperties{
		Size:          4,
		Align:         4,
		Integral:      true,
		HasBitOps:     true,
		HasArithmetic: true,
		HasCompare:    true,
		HasEquality:   true,
	}
}
func (UintType) String() string    { return "uint" }
func (t UintType) HashKey() string { return t.String() }

type Float32Type struct{}

var BuiltinFloat32Type = &Float32Type{}

func (Float32Type) aType() {}
func (Float32Type) Properties() TypeProperties {
	return TypeProperties{
		Size:          4,
		Align:         4,
		Signed:        true,
		Floating:      true,
		HasArithmetic: true,
		HasCompare:    true,
		HasEquality:   true,
	}
}
func (Float32Type) String() string    { return "float32" }
func (t Float32Type) HashKey() string { return t.String() }

type Float64Type struct{}

var BuiltinFloat64Type = &Float64Type{}

func (Float64Type) aType() {}
func (Float64Type) Properties() TypeProperties {
	return TypeProperties{
		Size:          8,
		Align:         8,
		Signed:        true,
		Floating:      true,
		HasArithmetic: true,
		HasCompare:    true,
		HasEquality:   true,
	}
}
func (Float64Type) String() string    { return "float64" }
func (t Float64Type) HashKey() string { return t.String() }

type StringType struct{}

var BuiltinStringType = &StringType{}

func (StringType) aType() {}
func (StringType) Properties() TypeProperties {
	return TypeProperties{}
}
func (StringType) String() string    { return "string" }
func (t StringType) HashKey() string { return t.String() }

type FuncType struct {
	ArgTypes    []Type
	ReturnTypes []Type
}

func (FuncType) aType() {}
func (FuncType) Properties() TypeProperties {
	return TypeProperties{}
}
func (t FuncType) String() string {
	var b strings.Builder
	b.WriteString("func(")
	for i, a := range t.ArgTypes {
		if i > 0 {
			b.WriteRune(',')
		}
		b.WriteString(a.String())
	}
	b.WriteRune(')')
	if len(t.ReturnTypes) > 0 {
		b.WriteRune('(')
		for i, a := range t.ReturnTypes {
			if i > 0 {
				b.WriteRune(',')
			}
			b.WriteString(a.String())
		}
		b.WriteRune(')')
	}
	return b.String()
}
func (t FuncType) HashKey() string {
	var b strings.Builder
	b.WriteString("func(")
	for i, a := range t.ArgTypes {
		if i > 0 {
			b.WriteRune(',')
		}
		b.WriteString(a.HashKey())
	}
	b.WriteRune(')')
	if len(t.ReturnTypes) > 0 {
		b.WriteRune('(')
		for i, a := range t.ReturnTypes {
			if i > 0 {
				b.WriteRune(',')
			}
			b.WriteString(a.HashKey())
		}
		b.WriteRune(')')
	}
	return b.String()
}

type ArrayType struct {
	Length      int
	ElementType Type
}

func (ArrayType) aType() {}
func (t ArrayType) Properties() TypeProperties {
	return TypeProperties{
		Size:  t.ElementType.Properties().Size * t.Length,
		Align: t.ElementType.Properties().Align,
	}
}
func (t ArrayType) String() string {
	return fmt.Sprintf("[%v]%v", t.Length, t.ElementType.String())
}
func (t ArrayType) HashKey() string {
	return fmt.Sprintf("[%v]%v", t.Length, t.ElementType.HashKey())
}

type TupleType struct {
	Types []Type
}

func (TupleType) aType() {}
func (t TupleType) Properties() TypeProperties {
	size := 0
	align := 0
	for _, typ := range t.Types {
		size += typ.Properties().Size
		if typ.Properties().Align > align {
			align = typ.Properties().Align
		}
	}
	return TypeProperties{
		Size:  size,
		Align: align,
	}
}
func (t TupleType) String() string {
	var b strings.Builder
	b.WriteRune('(')
	for i, typ := range t.Types {
		if i > 0 {
			b.WriteRune(',')
		}
		b.WriteString(typ.String())
	}
	b.WriteRune(')')
	return b.String()
}
func (t TupleType) HashKey() string {
	var b strings.Builder
	b.WriteRune('(')
	for i, typ := range t.Types {
		if i > 0 {
			b.WriteRune(',')
		}
		b.WriteString(typ.HashKey())
	}
	b.WriteRune(')')
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

func (t *TypeInterner) InternTupleType(types []Type) Type {
	tupleType := TupleType{
		Types: types,
	}
	key := tupleType.HashKey()

	if v, ok := t.types[key]; ok {
		return v
	}

	t.types[key] = &tupleType
	return &tupleType
}
