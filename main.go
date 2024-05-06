package main

import (
	"github.com/alecthomas/participle/v2"
	"github.com/alecthomas/participle/v2/lexer"
)

/*
Language structure

1. Parsing:
	Input -> string
	Output -> AST

2. Semantic:
	Input -> AST
	Output -> annotated AST

3. Compile:
	Input -> annotated AST
	Output ->
		| Golang code
		| Bytecode

4. Runtime:
	Input -> Bytecode
	Output -> result
*/

// Parser
var (
	Lexer = lexer.MustStateful(lexer.Rules{
		"Root": []lexer.Rule{},
	})
	Parser = participle.MustBuild[any](
		participle.Lexer(Lexer),
		participle.UseLookahead(1),
	)
)

// AST
type (
	Node interface {
		Position() lexer.Position
	}
)
