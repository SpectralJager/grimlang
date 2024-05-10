package main

import (
	"bytes"
	"fmt"
	"testing"

	"github.com/alecthomas/participle/v2"
)

const program = `
(def main (fn <unit> {
	(def lst list<int>{1 2})
	(println (ltos lst))
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

func Test(t *testing.T) {
	testCases := []struct {
		compt  Type
		repl   Type
		result Type
	}{
		{
			compt: &ListType{
				Subtype: &CompType{},
			},
			repl: &IntegerType{},
			result: &ListType{
				Subtype: &IntegerType{},
			},
		},
		{
			compt: &ListType{
				Subtype: &CompType{},
			},
			repl: &ListType{
				Subtype: &IntegerType{},
			},
			result: &ListType{
				Subtype: &IntegerType{},
			},
		},
	}
	for _, tC := range testCases {
		t.Run(InspectType(tC.result), func(t *testing.T) {
			res, _ := CompTypeReplace(tC.compt, tC.repl)
			if !CompareTypes(tC.result, res) {
				t.Fatal("got: ", InspectType(res))
			}
		})
	}
}
