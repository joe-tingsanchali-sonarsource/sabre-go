package main

import (
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

func printUsage() {
	fmt.Fprintf(os.Stderr, usageTemplate, os.Args[0])
	flag.PrintDefaults()
}

func main() {
	flag.Usage = printUsage

	flag.Parse()

	if flag.NArg() < 1 {
		fmt.Fprintf(os.Stderr, "Error: Please provide a file path\n")
		flag.Usage()
		os.Exit(1)
	}

	filePath := flag.Arg(0)

	unit, err := compiler.UnitFromFile(filePath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: Failed to create unit from file '%s': %v\n", filePath, err)
		os.Exit(1)
	}

	fmt.Printf("Successfully created unit from file: %s\n", filePath)
	_ = unit
}
