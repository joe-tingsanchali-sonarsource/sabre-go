package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"os"

	"github.com/MoustaphaSaad/sabre-go/internal/compiler"
)

const usageTemplate = `Usage: %s [flags] <file>

Arguments:
  file	input file path to compile

Flags:
`

type TokenOutput struct {
	Kind      string `json:"kind"`
	Value     string `json:"value"`
	Line      int32  `json:"line"`
	Column    int32  `json:"column"`
	ByteBegin int32  `json:"byte_begin"`
	ByteEnd   int32  `json:"byte_end"`
}

func printUsage() {
	fmt.Fprintf(os.Stderr, usageTemplate, os.Args[0])
	flag.PrintDefaults()
}

func scanFile(filePath string, outputFormat string) error {
	unit, err := compiler.UnitFromFile(filePath)
	if err != nil {
		return fmt.Errorf("failed to create unit from file '%s': %v", filePath, err)
	}

	scanner := compiler.NewScanner(unit.File())
	var tokens []TokenOutput

	for {
		token := scanner.Scan()

		tokenOut := TokenOutput{
			Kind:      token.Kind().String(),
			Value:     token.Value(),
			Line:      token.Location().Position.Line,
			Column:    token.Location().Position.Column,
			ByteBegin: token.Location().Range.Begin,
			ByteEnd:   token.Location().Range.End,
		}

		tokens = append(tokens, tokenOut)

		if token.Kind() == compiler.TokenEOF {
			break
		}
	}

	switch outputFormat {
	case "json":
		encoder := json.NewEncoder(os.Stdout)
		encoder.SetIndent("", "  ")
		return encoder.Encode(tokens)
	case "text":
		for _, token := range tokens {
			fmt.Printf("%-15s %-20s %d:%d [%d-%d]\n",
				token.Kind,
				fmt.Sprintf(`"%s"`, token.Value),
				token.Line,
				token.Column,
				token.ByteBegin,
				token.ByteEnd)
		}
		return nil
	default:
		return fmt.Errorf("unsupported output format: %s", outputFormat)
	}
}

func main() {
	var scanMode = flag.Bool("scan", false, "scan and output tokens instead of compiling")
	var outputFormat = flag.String("format", "text", "output format for scan mode (text, json)")

	flag.Usage = printUsage
	flag.Parse()

	if flag.NArg() < 1 {
		fmt.Fprintf(os.Stderr, "Error: Please provide a file path\n")
		flag.Usage()
		os.Exit(1)
	}

	filePath := flag.Arg(0)

	if *scanMode {
		if err := scanFile(filePath, *outputFormat); err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			os.Exit(1)
		}
		return
	}

	// Normal compilation mode
	unit, err := compiler.UnitFromFile(filePath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: Failed to create unit from file '%s': %v\n", filePath, err)
		os.Exit(1)
	}

	fmt.Printf("Successfully created unit from file: %s\n", filePath)
	_ = unit
}
