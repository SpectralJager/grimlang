package main

import (
	"bytes"
	"fmt"
	"testing"

	"github.com/alecthomas/participle/v2"
)

// TODO: add user function
// TODO: add global env
// TODO: add entry main function call

const program = `{
	(var result = {
		(break (itof 12))
	})
}`

func TestEval(t *testing.T) {
	var errBuff bytes.Buffer
	prog, err := Parser.ParseString("", program, participle.Trace(&errBuff))
	if err != nil {
		fmt.Println(errBuff.String())
		t.Fatal(err)
	}

	state := NewState(
		NewCoreEnviroment(),
	)
	_, err = Semantic(state, prog)
	if err != nil {
		t.Fatal(err)
	}
	_, err = Eval(state, prog)
	if err != nil {
		t.Fatal(err)
	}
	fmt.Println(state.Env.Inspect(2))
}
