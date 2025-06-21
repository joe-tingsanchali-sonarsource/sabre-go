package compiler

import (
	"path/filepath"
	"strings"
	"testing"
)

func TestUnitFileFromFile(t *testing.T) {
	tests := []struct {
		name          string
		filename      string
		expectError   bool
		expectedLines int
		expectedPath  string
	}{
		{
			name:          "simple shader file",
			filename:      "simple_shader.sabre",
			expectError:   false,
			expectedLines: 11,
			expectedPath:  "simple_shader.sabre",
		},
		{
			name:          "multiline file",
			filename:      "multiline.sabre",
			expectError:   false,
			expectedLines: 14,
			expectedPath:  "multiline.sabre",
		},
		{
			name:          "single line file",
			filename:      "single_line.sabre",
			expectError:   false,
			expectedLines: 2,
			expectedPath:  "single_line.sabre",
		},
		{
			name:          "empty file",
			filename:      "empty.sabre",
			expectError:   false,
			expectedLines: 1, // empty string splits to one empty element
			expectedPath:  "empty.sabre",
		},
		{
			name:        "non-existent file",
			filename:    "non_existent.sabre",
			expectError: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var filePath string
			if tt.expectError {
				filePath = filepath.Join("testdata", "Unit", tt.filename)
			} else {
				filePath = filepath.Join("testdata", "Unit", tt.filename)
			}

			unitFile, err := UnitFileFromFile(filePath)

			if tt.expectError {
				if err == nil {
					t.Errorf("UnitFileFromFile() expected error, but got none")
				}
				return
			}

			if err != nil {
				t.Errorf("UnitFileFromFile() unexpected error: %v", err)
				return
			}

			if unitFile == nil {
				t.Errorf("UnitFileFromFile() returned nil unitFile")
				return
			}

			// Check path
			if !strings.HasSuffix(unitFile.path, tt.expectedPath) {
				t.Errorf("UnitFileFromFile() path = %q, want suffix %q", unitFile.path, tt.expectedPath)
			}

			// Check absolute path is set and is absolute
			if unitFile.absolutePath == "" {
				t.Errorf("UnitFileFromFile() absolutePath is empty")
			}
			if !filepath.IsAbs(unitFile.absolutePath) {
				t.Errorf("UnitFileFromFile() absolutePath %q is not absolute", unitFile.absolutePath)
			}

			// Check lines count
			if len(unitFile.lines) != tt.expectedLines {
				t.Errorf("UnitFileFromFile() lines count = %d, want %d", len(unitFile.lines), tt.expectedLines)
			}

			// Check content matches lines
			expectedContent := strings.Join(unitFile.lines, "\n")
			if unitFile.content != expectedContent {
				t.Errorf("UnitFileFromFile() content doesn't match joined lines")
			}
		})
	}
}

func TestUnitFileFromFile_ContentParsing(t *testing.T) {
	// Test specific content parsing
	unitFile, err := UnitFileFromFile(filepath.Join("testdata", "Unit", "simple_shader.sabre"))
	if err != nil {
		t.Fatalf("UnitFileFromFile() unexpected error: %v", err)
	}

	// Check that the content contains expected text
	if !strings.Contains(unitFile.content, "vertexShader") {
		t.Errorf("UnitFileFromFile() content doesn't contain 'vertexShader'")
	}

	if !strings.Contains(unitFile.content, "fragmentShader") {
		t.Errorf("UnitFileFromFile() content doesn't contain 'fragmentShader'")
	}

	// Check first line
	if len(unitFile.lines) > 0 && unitFile.lines[0] != "package main" {
		t.Errorf("UnitFileFromFile() first line = %q, want %q", unitFile.lines[0], "package main")
	}
}

func TestUnitFromFile(t *testing.T) {
	tests := []struct {
		name        string
		filename    string
		expectError bool
	}{
		{
			name:        "valid file",
			filename:    "simple_shader.sabre",
			expectError: false,
		},
		{
			name:        "empty file",
			filename:    "empty.sabre",
			expectError: false,
		},
		{
			name:        "non-existent file",
			filename:    "non_existent.sabre",
			expectError: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			filePath := filepath.Join("testdata", "Unit", tt.filename)

			unit, err := UnitFromFile(filePath)

			if tt.expectError {
				if err == nil {
					t.Errorf("UnitFromFile() expected error, but got none")
				}
				return
			}

			if err != nil {
				t.Errorf("UnitFromFile() unexpected error: %v", err)
				return
			}

			if unit == nil {
				t.Errorf("UnitFromFile() returned nil unit")
				return
			}

			if unit.rootFile == nil {
				t.Errorf("UnitFromFile() rootFile is nil")
				return
			}

			// Check that the root file has the expected path
			if !strings.HasSuffix(unit.rootFile.path, tt.filename) {
				t.Errorf("UnitFromFile() rootFile.path = %q, want suffix %q", unit.rootFile.path, tt.filename)
			}
		})
	}
}
