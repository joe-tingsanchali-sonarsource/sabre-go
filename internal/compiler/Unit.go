package compiler

import (
	"io"
	"os"
	"path/filepath"
	"strings"
)

type UnitFile struct {
	path         string
	absolutePath string
	content      string
	lines        []string
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
	contentStr := strings.ReplaceAll(string(content), "\r\n", "\n")

	absolutePath, err := filepath.Abs(path)
	if err != nil {
		return nil, err
	}
	unitFile = &UnitFile{
		path:         path,
		absolutePath: absolutePath,
		content:      contentStr,
		lines:        strings.Split(contentStr, "\n"),
	}

	return
}

type Unit struct {
	rootFile *UnitFile
}

// File returns the root file of the unit
func (u *Unit) File() *UnitFile {
	return u.rootFile
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
