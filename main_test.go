package main

import (
	"bytes"
	"fmt"
	"testing"

	"github.com/alecthomas/participle/v2"
)

const program = `
(def n 35)

(def fib (fn [n:int] <int> 
	(cond
		(case (< n 2) n)
		(+ (fib (- n 1)) (fib (- n 2))))
))

(def main (fn <unit> {
	(def res (fib n))
	(println (itos res))
}))
`

func TestInterpreter(t *testing.T) {
	var errBuf bytes.Buffer
	resNode, err := Parser.ParseString("test.grim", program, participle.Trace(&errBuf))
	if err != nil {
		fmt.Println(errBuf.String())
		t.Fatal(err)
	}
	builtin := NewEnviroment("builtin", nil)
	builtin.Kind = BuiltinEK
	InsertBuiltinSymbols(builtin)
	resTyp, err := TypeChecke(builtin, resNode)
	if err != nil {
		t.Fatal(err)
	}
	fmt.Println(builtin.Inspect())
	resVal, err := Eval(builtin, resNode)
	if err != nil {
		t.Fatal(err)
	}
	fmt.Printf("result: %s of %s\n", InspectValue(resVal), InspectType(resTyp))
}
