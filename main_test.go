package main

import (
	"bytes"
	"fmt"
	"testing"

	"github.com/alecthomas/participle/v2"
)

const program = `
(def printer
	(fn [val:string] <unit> (println val))	
	(fn [val:int] <unit> (println (itos val)))	
	(fn [val:float] <unit> (println (ftos val)))	
	(fn [val:bool] <unit> 
		(println (cond
			(case val "true")
			"false")))	
)

(def main (fn <unit> {
	(printer 10)
	(printer 10.10)
	(printer "10:hell")
	(printer true)
	(printer false)
	()
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
	resTyp, err := TypeCheck(builtin, resNode)
	if err != nil {
		t.Fatal(err)
	}
	// fmt.Println(builtin.Inspect())
	resVal, err := Eval(builtin, resNode)
	if err != nil {
		t.Fatal(err)
	}
	fmt.Printf("result: %s of %s\n", InspectValue(resVal), InspectType(resTyp))
}

func TestParser(t *testing.T) {
	fmt.Println(Parser.String())
}
