package compiler

import (
	"fmt"
	"strings"
)

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

func (loc Location) HighlightCodeRange() string {
	if loc.File == nil || int(loc.Position.Line) > len(loc.File.lines) || loc.Position.Line < 1 {
		return ""
	}

	line := loc.File.lines[loc.Position.Line-1] // Convert to 0-based index

	// Build the highlight string
	var result strings.Builder
	result.WriteString(">> \t")
	result.WriteString(line)
	result.WriteString("\n")
	result.WriteString(">> \t")

	// Add spaces up to the beginning of the range
	for i := int32(0); i < loc.Range.Begin; i++ {
		if i < int32(len(line)) && line[i] == '\t' {
			result.WriteString("\t")
		} else {
			result.WriteString(" ")
		}
	}

	// Add carets for the range
	rangeLength := loc.Range.End - loc.Range.Begin
	if rangeLength <= 0 {
		rangeLength = 1 // At least one caret
	}
	result.WriteString(strings.Repeat("^", int(rangeLength)))

	return result.String()
}

func (loc Location) String() string {
	if loc.File != nil {
		return fmt.Sprintf("%v:%v", loc.File.path, loc.Position)
	}
	return fmt.Sprintf("<nil>:%v", loc.Position)
}
