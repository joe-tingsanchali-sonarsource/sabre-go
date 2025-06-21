package compiler

import (
	"strings"
	"testing"
)

func TestPosition_String(t *testing.T) {
	tests := []struct {
		name     string
		position Position
		expected string
	}{
		{
			name:     "basic position",
			position: Position{Line: 10, Column: 5},
			expected: "10:5",
		},
		{
			name:     "zero position",
			position: Position{Line: 0, Column: 0},
			expected: "0:0",
		},
		{
			name:     "large numbers",
			position: Position{Line: 1000, Column: 999},
			expected: "1000:999",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := tt.position.String()
			if result != tt.expected {
				t.Errorf("Position.String() = %q, want %q", result, tt.expected)
			}
		})
	}
}

func TestLocation_String(t *testing.T) {
	unitFile := &UnitFile{
		path: "/path/to/test.go",
	}

	tests := []struct {
		name     string
		location Location
		expected string
	}{
		{
			name: "basic location",
			location: Location{
				Position: Position{Line: 10, Column: 5},
				File:     unitFile,
			},
			expected: "/path/to/test.go:10:5",
		},
		{
			name: "nil file",
			location: Location{
				Position: Position{Line: 1, Column: 1},
				File:     nil,
			},
			expected: "<nil>:1:1",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := tt.location.String()
			if result != tt.expected {
				t.Errorf("Location.String() = %q, want %q", result, tt.expected)
			}
		})
	}
}

func TestLocation_HighlightCodeRange(t *testing.T) {
	tests := []struct {
		name     string
		location Location
		expected string
	}{
		{
			name: "basic highlight",
			location: Location{
				Position: Position{Line: 1, Column: 1},
				Range:    Range{Begin: 0, End: 3},
				File: &UnitFile{
					lines: []string{"add(x, y) = z;"},
				},
			},
			expected: ">> \tadd(x, y) = z;\n>> \t^^^",
		},
		{
			name: "highlight with offset",
			location: Location{
				Position: Position{Line: 1, Column: 1},
				Range:    Range{Begin: 4, End: 8},
				File: &UnitFile{
					lines: []string{"add(x, y) = z;"},
				},
			},
			expected: ">> \tadd(x, y) = z;\n>> \t    ^^^^",
		},
		{
			name: "highlight with tabs",
			location: Location{
				Position: Position{Line: 1, Column: 1},
				Range:    Range{Begin: 1, End: 4},
				File: &UnitFile{
					lines: []string{"\tadd(x, y);"},
				},
			},
			expected: ">> \t\tadd(x, y);\n>> \t\t^^^",
		},
		{
			name: "invalid line number (too high)",
			location: Location{
				Position: Position{Line: 5, Column: 1},
				Range:    Range{Begin: 0, End: 3},
				File: &UnitFile{
					lines: []string{"line1", "line2"},
				},
			},
			expected: "",
		},
		{
			name: "invalid line number (zero)",
			location: Location{
				Position: Position{Line: 0, Column: 1},
				Range:    Range{Begin: 0, End: 3},
				File: &UnitFile{
					lines: []string{"line1"},
				},
			},
			expected: "",
		},
		{
			name: "nil file",
			location: Location{
				Position: Position{Line: 1, Column: 1},
				Range:    Range{Begin: 0, End: 3},
				File:     nil,
			},
			expected: "",
		},
		{
			name: "zero length range",
			location: Location{
				Position: Position{Line: 1, Column: 1},
				Range:    Range{Begin: 5, End: 5},
				File: &UnitFile{
					lines: []string{"hello world"},
				},
			},
			expected: ">> \thello world\n>> \t     ^",
		},
		{
			name: "multiline file, select second line",
			location: Location{
				Position: Position{Line: 2, Column: 1},
				Range:    Range{Begin: 0, End: 6},
				File: &UnitFile{
					lines: []string{"first line", "second line", "third line"},
				},
			},
			expected: ">> \tsecond line\n>> \t^^^^^^",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := tt.location.HighlightCodeRange()
			t.Logf("Got lines:\n%s", strings.ReplaceAll(result, "\t", "  "))
			t.Logf("Expected lines:\n%s", strings.ReplaceAll(tt.expected, "\t", "  "))
			if result != tt.expected {
				t.Errorf("Location.HighlightCodeRange() = %q, want %q", result, tt.expected)
			}
		})
	}
}
