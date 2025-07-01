package main

import (
	"bytes"
	"flag"
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

	file := filepath.Clean(args[0])
	unit, err := compiler.UnitFromFile(file)
	if err != nil {
		return fmt.Errorf("failed to create unit from file '%s': %v", file, err)
	}

	if !unit.Scan() {
		unit.PrintErrors(out)
		return nil
	}

	for _, token := range unit.RootFile().Tokens() {
		fmt.Fprintf(out, "%-15s %-20s %4d:%-4d [%d-%d]\n",
			token.Kind().String(),
			fmt.Sprintf(`"%s"`, token.Value()),
			token.Location().Position.Line,
			token.Location().Position.Column,
			token.Location().Range.Begin,
			token.Location().Range.End)
	}
	return nil
}

func testScan(args []string, out io.Writer) error {
	flagSet := flag.NewFlagSet("test-scan", flag.ContinueOnError)
	update := flagSet.Bool("update", false, "updates test outputs")
	err := flagSet.Parse(args)
	if err != nil {
		return err
	}

	if flagSet.NArg() < 1 {
		return fmt.Errorf("no test data directory provided\n%v", helpString())
	}

	dir := flagSet.Arg(0)
	var goldenFiles []string
	err = filepath.WalkDir(dir, func(path string, d fs.DirEntry, err error) error {
		if !d.IsDir() && filepath.Ext(path) == ".golden" {
			goldenFiles = append(goldenFiles, path)
		}
		return nil
	})
	if err != nil {
		return fmt.Errorf("failed to walk test data directory: %v", err)
	}

	for i, goldenFile := range goldenFiles {
		testFile := strings.TrimSuffix(goldenFile, filepath.Ext(goldenFile))
		expectedBytes, err := os.ReadFile(goldenFile)
		if err != nil {
			return fmt.Errorf("failed to read test file '%v': %v", testFile, err)
		}
		expectedOutput := cleanString(string(expectedBytes))

		fmt.Fprintf(out, "%v/%v) testing %v\n", i, len(goldenFiles), testFile)

		var actualOutputBuffer bytes.Buffer
		err = scan([]string{testFile}, &actualOutputBuffer)
		if err != nil {
			return err
		}
		actualOutput := cleanString(actualOutputBuffer.String())

		if expectedOutput != actualOutput {
			if *update {
				f, err := os.Create(goldenFile)
				if err != nil {
					return err
				}
				defer f.Close()

				fmt.Fprintf(f, "%s\n", actualOutput)
				fmt.Fprintln(out, "UPDATED")
			} else {
				fmt.Fprintln(out, "FAILURE")
			}
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
