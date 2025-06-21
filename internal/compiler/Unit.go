package compiler

import (
	"io"
	"os"
	"path/filepath"
)

type UnitFile struct {
	path         string
	absolutePath string
	content      string
}

func UnitFileFromFile(path string) (unitFile *UnitFile, err error) {
	file, err := os.Open(path)
	if err != nil {
		return nil, err
	}

	content, err := io.ReadAll(file)
	if err != nil {
		return nil, err
	}

	absolutePath, err := filepath.Abs(path)
	if err != nil {
		return nil, err
	}

	unitFile = &UnitFile{
		path:         path,
		absolutePath: absolutePath,
		content:      string(content),
	}

	return
}

type Unit struct {
	rootFile *UnitFile
}

func UnitFromFile(path string) (unit *Unit, err error) {
	unitFile, err := UnitFileFromFile(path)
	if err != nil {
		return nil, err
	}

	unit = &Unit{
		rootFile: unitFile,
	}
	return
}
