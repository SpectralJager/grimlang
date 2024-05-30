package main

import (
	"bytes"
	"fmt"
	"testing"

	"github.com/alecthomas/participle/v2"
)

const program = `
(defn printPoint2d (fn [point:point2d] <unit>
	(io/println (++
		"x:"
		(floats/toString point/x)
		" y:"
		(floats/toString point/y)
	))
))

(defn main (fn <unit> {
	(def point point2d{
		x:12.2
		y:0.0
	})
	(printPoint2d point)
	(def vec vector{
		p0:point{
			x:0.0
			y:0.0
		}
		p1:point{
			x:0.0
			y:0.0
		}
	})
	(printPoint2d vec/p0)
	(printPoint2d vec/p1)
	(io/println (floats/toString vec/p0/x))
	()
}))
(defr vector (record
	p0: point2d	
	p1: point2d	
))

(defr point2d (record 
	x:float	
	y:float
))

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
