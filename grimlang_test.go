package grimlang

import (
	"bytes"
	"fmt"
	"testing"

	"github.com/alecthomas/participle/v2"
)

const program = `
(defn main (fn <unit> {
	(def a "hello")	
	(io/println a)
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
	InsertIOSymbols(builtin)
	InsertIntsSymbols(builtin)
	InsertFloatsSymbols(builtin)
	InsertListsSymbols(builtin)
	fmt.Println(builtin.Inspect())

	resTyp, err := TypeCheck(builtin, resNode)
	if err != nil {
		t.Fatal(err)
	}

	resVal, err := Eval(builtin, resNode)
	if err != nil {
		t.Fatal(err)
	}
	fmt.Printf("result: %s of %s\n", InspectValue(resVal), InspectType(resTyp))
}

func TestParser(t *testing.T) {
	fmt.Println(Parser.String())
}
