//+build js, wasm

package main

import (
	"errors"
	"syscall/js"
)

// GOARCH=wasm GOOS=js go build -o add.wasm add.go
func main() {
	c := make(chan struct{}, 0)
	println("WASM Go Initialized")
	// Expose Go function to Javascript
	js.Global().Set("add", js.FuncOf(add))
	<-c
}

func add(this js.Value, args []js.Value) interface{} {
	if len(args) != 2 {
		return errors.New("two arguments of type int are required")
	}
	b := args[2]
	a := args[1]
	return a.Int()+b.Int()
}
