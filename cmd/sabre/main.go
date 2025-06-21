package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"os"

	"github.com/MoustaphaSaad/sabre-go/internal/compiler"
)

const commandUsageTemplate = `Usage: %s <command> [flags]

Commands:
  scan	scans the given file and prints tokens to stdout
      	"sabre scan <file>"
`

type TokenDesc struct {
	Kind      string `json:"kind"`
	Value     string `json:"value"`
	Line      int32  `json:"line"`
	Column    int32  `json:"column"`
	ByteBegin int32  `json:"byte_begin"`
	ByteEnd   int32  `json:"byte_end"`
}

func helpString() string {
	return fmt.Sprintf(commandUsageTemplate, os.Args[0])
}

func help() {
	fmt.Fprint(os.Stderr, helpString())
}

func scan(args []string) error {
	fs := flag.NewFlagSet("scan", flag.ExitOnError)
	outputFormat := fs.String("format", "text", "output format for scan mode (test, json)")
	fs.Parse(args)

	if fs.NArg() < 1 {
		return fmt.Errorf("no file provided\n%v", helpString())
	}

	file := fs.Arg(0)
	unit, err := compiler.UnitFromFile(fs.Arg(0))
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
		if token.Kind() == compiler.TokenEOF {
			break
		}
	}

	switch *outputFormat {
	case "json":
		encoder := json.NewEncoder(os.Stdout)
		encoder.SetIndent("", "  ")
		err := encoder.Encode(tokens)
		if err != nil {
			return fmt.Errorf("failed to encode tokens: %v", err)
		}
	case "text":
		for _, token := range tokens {
			fmt.Printf("%-15s %-20s %4d:%-4d [%d-%d]\n",
				token.Kind,
				fmt.Sprintf(`"%s"`, token.Value),
				token.Line,
				token.Column,
				token.ByteBegin,
				token.ByteEnd)
		}
	default:
		return fmt.Errorf("unsupported output format: %s", *outputFormat)
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
		err = scan(subArgs)
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
