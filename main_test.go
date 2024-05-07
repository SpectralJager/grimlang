package main

import (
	"bytes"
	"fmt"
	"testing"

	"github.com/alecthomas/participle/v2"
)

const program = `{
	(def a 12)
	(def b (* a a))
	(+ a b)
}`

func TestInterpreter(t *testing.T) {
	var errBuf bytes.Buffer
	resNode, err := Parser.ParseString("test.grim", program, participle.Trace(&errBuf))
	if err != nil {
		fmt.Println(errBuf.String())
		t.Fatal(err)
	}
	mainEnv := NewEnviroment("main_global", nil)
	resTyp, err := TypeChecke(mainEnv, resNode)
	if err != nil {
		t.Fatal(err)
	}
	resVal, err := Eval(mainEnv, resNode)
	if err != nil {
		t.Fatal(err)
	}
	fmt.Printf("result: %s of %s\n", InspectValue(resVal), InspectType(resTyp))
	fmt.Println(mainEnv.Inspect())
}
