package main

import (
	"bytes"
	"fmt"
	"testing"

	"github.com/alecthomas/participle/v2"
)

const program = `
(def mapper (fn [lst:list<int> f:[int]<int>] <list<int>> {
	(var newLst = list<int>{})
	(range [itm : lst] {
		(def res (f itm))
		(set newLst = (lappend newLst res))
		()
	})
	newLst
}))

(def main (fn <unit> {
	(println (ltos (mapper 
		list<int>{1 2 3}
		(fn [itm:int] <int> (* itm 2))
	)))
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
	fmt.Println(builtin.Inspect())
	resVal, err := Eval(builtin, resNode)
	if err != nil {
		t.Fatal(err)
	}
	fmt.Printf("result: %s of %s\n", InspectValue(resVal), InspectType(resTyp))
}

func TestParser(t *testing.T) {
	fmt.Println(Parser.String())
}
