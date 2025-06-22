package main

import (
	"bytes"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"strings"

	"github.com/MoustaphaSaad/sabre-go/internal/compiler"
)

const commandUsageTemplate = `Usage: %s <command> [flags]

Commands:
  scan        scans the given file and prints tokens to stdout
              "sabre scan <file>"
  test-scan   tests the scan phase against golden output
              "sabre test-scan <test-data-dir>"
`

type TokenDesc struct {
	Kind      string
	Value     string
	Line      int32
	Column    int32
	ByteBegin int32
	ByteEnd   int32
}

func helpString() string {
	return fmt.Sprintf(commandUsageTemplate, os.Args[0])
}

func help() {
	fmt.Fprint(os.Stderr, helpString())
}

func cleanString(s string) string {
	return strings.TrimSpace(strings.ReplaceAll(s, "\r\n", "\n"))
}

func scan(args []string, out io.Writer) error {
	if len(args) < 1 {
		return fmt.Errorf("no file provided\n%v", helpString())
	}

	file := args[0]
	unit, err := compiler.UnitFromFile(file)
	if err != nil {
		return fmt.Errorf("failed to create unit from file '%s': %v", file, err)
	}

	scanner := compiler.NewScanner(unit.RootFile())
	var tokens []TokenDesc
	for {
		token := scanner.Scan()

		tokenDesc := TokenDesc{
			Kind:      token.Kind().String(),
			Value:     token.Value(),
			Line:      token.Location().Position.Line,
			Column:    token.Location().Position.Column,
			ByteBegin: token.Location().Range.Begin,
			ByteEnd:   token.Location().Range.End,
		}

		tokens = append(tokens, tokenDesc)
		if token.Kind() == compiler.TokenEOF || token.Kind() == compiler.TokenInvalid {
			break
		}
	}

	for _, token := range tokens {
		fmt.Fprintf(out, "%-15s %-20s %4d:%-4d [%d-%d]\n",
			token.Kind,
			fmt.Sprintf(`"%s"`, token.Value),
			token.Line,
			token.Column,
			token.ByteBegin,
			token.ByteEnd)
	}
	return nil
}

func testScan(args []string, out io.Writer) error {
	if len(args) < 1 {
		return fmt.Errorf("no test data directory provided\n%v", helpString())
	}

	dir := args[0]
	var goldenFiles []string
	err := filepath.WalkDir(dir, func(path string, d fs.DirEntry, err error) error {
		if !d.IsDir() && filepath.Ext(path) == ".golden" {
			goldenFiles = append(goldenFiles, path)
		}
		return nil
	})
	if err != nil {
		return fmt.Errorf("failed to walk test data directory: %v", err)
	}

	for _, goldenFile := range goldenFiles {
		testFile := strings.TrimSuffix(goldenFile, filepath.Ext(goldenFile))
		expectedBytes, err := os.ReadFile(goldenFile)
		if err != nil {
			return fmt.Errorf("failed to read test file '%v': %v", testFile, err)
		}
		expectedOutput := cleanString(string(expectedBytes))

		fmt.Fprintf(out, "===testing %v===\n", testFile)

		var actualOutputBuffer bytes.Buffer
		err = scan([]string{testFile}, &actualOutputBuffer)
		if err != nil {
			return err
		}
		actualOutput := cleanString(actualOutputBuffer.String())

		if expectedOutput != actualOutput {
			fmt.Fprintln(out, "FAIL: expected != actual")
			fmt.Fprintln(out, "expected:")
			fmt.Fprintln(out, expectedOutput)
			fmt.Fprintln(out, "actual:")
			fmt.Fprintln(out, actualOutput)
		} else {
			fmt.Fprintln(out, "SUCCESS")
		}
	}

	return nil
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintf(os.Stderr, "Error: no command found\n")
		help()
		return
	}

	subArgs := os.Args[2:]
	var err error
	switch os.Args[1] {
	case "help":
		help()
	case "scan":
		err = scan(subArgs, os.Stdout)
	case "test-scan":
		err = testScan(subArgs, os.Stdout)
	default:
		fmt.Fprintf(os.Stderr, "Error: unknown command '%s'\n", os.Args[1])
		help()
		os.Exit(1)
	}

	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}
