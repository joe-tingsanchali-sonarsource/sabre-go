package compiler

type Type interface {
	aType()
	Size() int
	Align() int
	Signed() bool
}

type VoidType struct{}

var BuiltinVoidType = &VoidType{}

func (VoidType) aType()       {}
func (VoidType) Size() int    { return 0 }
func (VoidType) Align() int   { return 0 }
func (VoidType) Signed() bool { return false }

type BoolType struct{}

var BuiltinBoolType = &BoolType{}

func (BoolType) aType()       {}
func (BoolType) Size() int    { return 1 }
func (BoolType) Align() int   { return 4 }
func (BoolType) Signed() bool { return false }

type IntType struct{}

var BuiltinIntType = &IntType{}

func (IntType) aType()       {}
func (IntType) Size() int    { return 4 }
func (IntType) Align() int   { return 4 }
func (IntType) Signed() bool { return true }

type UintType struct{}

var BuiltinUintType = &UintType{}

func (UintType) aType()       {}
func (UintType) Size() int    { return 4 }
func (UintType) Align() int   { return 4 }
func (UintType) Signed() bool { return false }

type Float32Type struct{}

var BuiltinFloat32Type = &Float32Type{}

func (Float32Type) aType()       {}
func (Float32Type) Size() int    { return 4 }
func (Float32Type) Align() int   { return 4 }
func (Float32Type) Signed() bool { return true }

type Float64Type struct{}

var BuiltinFloat64Type = &Float64Type{}

func (Float64Type) aType()       {}
func (Float64Type) Size() int    { return 8 }
func (Float64Type) Align() int   { return 8 }
func (Float64Type) Signed() bool { return true }
