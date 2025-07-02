package compiler

import (
	"strings"
	"testing"
)

func TestSourcePosition_String(t *testing.T) {
	tests := []struct {
		name     string
		position SourcePosition
		expected string
	}{
		{
			name:     "basic position",
			position: SourcePosition{Line: 10, Column: 5},
			expected: "10:5",
		},
		{
			name:     "zero position",
			position: SourcePosition{Line: 0, Column: 0},
			expected: "0:0",
		},
		{
			name:     "large numbers",
			position: SourcePosition{Line: 1000, Column: 999},
			expected: "1000:999",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := tt.position.String()
			if result != tt.expected {
				t.Errorf("SourcePosition.String() = %q, want %q", result, tt.expected)
			}
		})
	}
}

func TestSourceLocation_String(t *testing.T) {
	unitFile := &UnitFile{
		path: "/path/to/test.go",
	}

	tests := []struct {
		name     string
		location SourceLocation
		expected string
	}{
		{
			name: "basic location",
			location: SourceLocation{
				Position: SourcePosition{Line: 10, Column: 5},
				File:     unitFile,
			},
			expected: "/path/to/test.go:10:5",
		},
		{
			name: "nil file",
			location: SourceLocation{
				Position: SourcePosition{Line: 1, Column: 1},
				File:     nil,
			},
			expected: "<nil>:1:1",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := tt.location.String()
			if result != tt.expected {
				t.Errorf("SourceLocation.String() = %q, want %q", result, tt.expected)
			}
		})
	}
}

func concatLines(lines ...string) string {
	var builder strings.Builder
	for i, line := range lines {
		if i > 0 {
			builder.WriteByte('\n')
		}
		builder.WriteString(line)
	}
	return builder.String()
}

func TestSourceRange_HighlightCodeRange(t *testing.T) {
	tests := []struct {
		name        string
		sourceRange SourceRange
		expected    string
	}{
		{
			name: "basic highlight",
			sourceRange: SourceRange{
				BeginPosition: SourcePosition{Line: 1, Column: 1},
				EndPosition:   SourcePosition{Line: 1, Column: 4},
				BeginOffset:   0,
				EndOffset:     3,
				File: &UnitFile{
					lines: []string{"add(x, y) = z;"},
				},
			},
			expected: concatLines(
				">> 	add(x, y) = z;",
				">> 	^^^           ",
			),
		},
		{
			name: "highlight with offset",
			sourceRange: SourceRange{
				BeginPosition: SourcePosition{Line: 1, Column: 1},
				EndPosition:   SourcePosition{Line: 1, Column: 5},
				BeginOffset:   4,
				EndOffset:     8,
				File: &UnitFile{
					lines: []string{"add(x, y) = z;"},
				},
			},
			expected: concatLines(
				">> 	add(x, y) = z;",
				">> 	    ^^^^      ",
			),
		},
		{
			name: "highlight with tabs",
			sourceRange: SourceRange{
				BeginPosition: SourcePosition{Line: 1, Column: 1},
				EndPosition:   SourcePosition{Line: 1, Column: 4},
				BeginOffset:   1,
				EndOffset:     4,
				File: &UnitFile{
					lines: []string{"\tadd(x, y);"},
				},
			},
			expected: concatLines(
				">> 		add(x, y);",
				">> 		^^^       ",
			),
		},
		{
			name: "invalid line number (too high)",
			sourceRange: SourceRange{
				BeginPosition: SourcePosition{Line: 5, Column: 1},
				EndPosition:   SourcePosition{Line: 5, Column: 4},
				BeginOffset:   0,
				EndOffset:     3,
				File: &UnitFile{
					lines: []string{"line1", "line2"},
				},
			},
			expected: "",
		},
		{
			name: "invalid line number (zero)",
			sourceRange: SourceRange{
				BeginPosition: SourcePosition{Line: 0, Column: 1},
				EndPosition:   SourcePosition{Line: 0, Column: 4},
				BeginOffset:   0,
				EndOffset:     3,
				File: &UnitFile{
					lines: []string{"line1"},
				},
			},
			expected: "",
		},
		{
			name: "nil file",
			sourceRange: SourceRange{
				BeginPosition: SourcePosition{Line: 1, Column: 1},
				EndPosition:   SourcePosition{Line: 1, Column: 4},
				BeginOffset:   0,
				EndOffset:     3,
				File:          nil,
			},
			expected: "",
		},
		{
			name: "zero length range",
			sourceRange: SourceRange{
				BeginPosition: SourcePosition{Line: 1, Column: 6},
				EndPosition:   SourcePosition{Line: 1, Column: 7},
				BeginOffset:   5,
				EndOffset:     6,
				File: &UnitFile{
					lines: []string{"hello world"},
				},
			},
			expected: concatLines(
				">> 	hello world",
				">> 	     ^     ",
			),
		},
		{
			name: "multiline file, select second line",
			sourceRange: SourceRange{
				BeginPosition: SourcePosition{Line: 2, Column: 1},
				EndPosition:   SourcePosition{Line: 2, Column: 7},
				BeginOffset:   11,
				EndOffset:     17,
				File: &UnitFile{
					lines: []string{"first line", "second line", "third line"},
				},
			},
			expected: concatLines(
				">> 	second line",
				">> 	^^^^^^     ",
			),
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := tt.sourceRange.HighlightCodeRange()
			t.Logf("Got lines:\n%s", strings.ReplaceAll(result, "\t", "  "))
			t.Logf("Expected lines:\n%s", strings.ReplaceAll(tt.expected, "\t", "  "))
			if result != tt.expected {
				t.Errorf("Location.HighlightCodeRange() = %q, want %q", result, tt.expected)
			}
		})
	}
}
