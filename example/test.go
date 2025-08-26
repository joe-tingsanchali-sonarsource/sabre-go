package main

import "fmt"

func foo() (int, float32) {
	return 1, 1.5
}

func main() {
	var f [3]int
	x, y := foo()
	fmt.Println(x, y)

	f[1], y = foo()
	fmt.Println(f[1], y)

	// a, b, c := foo(), 3
	// fmt.Println(a, b, c)

	// a2, b2, c2 := 1, foo()
	// fmt.Println(a2, b2, c2)
}
