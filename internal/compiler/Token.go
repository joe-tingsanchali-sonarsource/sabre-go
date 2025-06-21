package compiler

import "fmt"

type Position struct {
	Line, Column int32
}

func (p Position) String() string {
	return fmt.Sprintf("%v:%v", p.Line, p.Column)
}

type Range struct {
	Begin, End int32
}

type Location struct {
	Position Position
	Range    Range
	File     *UnitFile
}
