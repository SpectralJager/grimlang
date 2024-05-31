package grimlang

import "github.com/alecthomas/participle/v2/lexer"

var Lexer = lexer.MustStateful(lexer.Rules{
	"Root": []lexer.Rule{
		{Name: "wspace", Pattern: `[ \r\t\n]+`},

		{Name: "Defn", Pattern: `defn`},
		{Name: "Defc", Pattern: `defc`},
		{Name: "Defr", Pattern: `defr`},
		{Name: "Def", Pattern: `def`},
		{Name: "Set", Pattern: `var`},
		{Name: "Var", Pattern: `set`},
		{Name: "Cond", Pattern: `cond`},
		{Name: "Case", Pattern: `case`},
		{Name: "While", Pattern: `while`},
		{Name: "For", Pattern: `for`},
		{Name: "Range", Pattern: `range`},
		{Name: "Fn", Pattern: `fn`},
		{Name: "Record", Pattern: `record`},

		{Name: "StringStart", Pattern: `"`, Action: lexer.Push("String")},
		{Name: "Boolean", Pattern: `(true|false)`},
		{Name: "Float", Pattern: `-?[0-9]+\.[0-9]*`},
		{Name: "Integer", Pattern: `-?[0-9]+`},
		{Name: "Unit", Pattern: `\(\)`},
		{Name: "Symbol", Pattern: `\??[a-zA-Z]+[a-zA-Z0-9_]*`},

		{Name: "<=", Pattern: `<=`},
		{Name: ">=", Pattern: `>=`},
		{Name: "<>", Pattern: `<>`},
		{Name: "++", Pattern: `\+\+`},

		{Name: "+", Pattern: `\+`},
		{Name: "-", Pattern: `-`},
		{Name: "*", Pattern: `\*`},
		{Name: "/", Pattern: `\/`},
		{Name: "<", Pattern: `<`},
		{Name: ">", Pattern: `>`},
		{Name: "=", Pattern: `=`},
		{Name: "|", Pattern: `\|`},
		{Name: "&", Pattern: `&`},
		{Name: "::", Pattern: `:`},
		{Name: ":", Pattern: `:`},
		{Name: "(", Pattern: `\(`},
		{Name: ")", Pattern: `\)`},
		{Name: "[", Pattern: `\[`},
		{Name: "]", Pattern: `\]`},
		{Name: "{", Pattern: `{`},
		{Name: "}", Pattern: `}`},
	},
	"String": {
		{Name: "StringEnd", Pattern: `"`, Action: lexer.Pop()},
		{Name: "String", Pattern: `(\\"|[^"])*`},
	},
})
