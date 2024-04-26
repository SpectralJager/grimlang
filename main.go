package main

import (
	"fmt"
	"maps"
	"strings"

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

// Parsing
var (
	Lexer = lexer.MustStateful(lexer.Rules{
		"Root": []lexer.Rule{
			{Name: "wspace", Pattern: `[ \t\n]+`},

			{Name: "Variable", Pattern: `var`},
			{Name: "Set", Pattern: `set`},
			{Name: "Break", Pattern: `break`},

			{Name: "String", Pattern: `"(\\"|[^"])*"`},
			{Name: "Float", Pattern: `[-+]?[0-9]+\.[0-9]*`},
			{Name: "Integer", Pattern: `[-+]?[0-9]+`},
			{Name: "Boolean", Pattern: `(true|false)`},
			{Name: "Symbol", Pattern: `[a-zA-Z]+[a-zA-Z0-9_]*`},

			{Name: "+", Pattern: `\+`},
			{Name: "-", Pattern: `\-`},
			{Name: "*", Pattern: `\*`},
			{Name: "/", Pattern: `\/`},

			{Name: "(", Pattern: `\(`},
			{Name: ")", Pattern: `\)`},
			{Name: "{", Pattern: `{`},
			{Name: "}", Pattern: `}`},

			{Name: "=", Pattern: `=`},
		},
	})
	Parser = participle.MustBuild[BlockNode](
		participle.Lexer(Lexer),
		participle.UseLookahead(1),
		participle.Union[BlockNodeUnion](
			&VariableNode{},
			&SetNode{},
			&BreakNode{},
			&OperaitonNode{},
			&CallNode{},
		),
		participle.Union[ExpressionNodeUnion](
			&BlockNode{},
			&OperaitonNode{},
			&CallNode{},
			&SymbolNode{},
			&IntegerNode{},
			&FloatNode{},
		),
	)
)

// AST
type (
	NodeKind uint
	Node     interface {
		Position() lexer.Position
		Kind() NodeKind
	}
	ExpressionNodeUnion interface{ Node }
	BlockNodeUnion      interface{ Node }
	OperaitonNode       struct {
		Pos           lexer.Position
		Operation     string                `parser:"'(' @('+'|'-'|'*'|'/')"`
		Arguments     []ExpressionNodeUnion `parser:"@@+ ')'"`
		OperationType Type
	}
	CallNode struct {
		Pos           lexer.Position
		Call          *SymbolNode           `parser:"'(' @@"`
		Arguments     []ExpressionNodeUnion `parser:"@@+ ')'"`
		OperationType Type
	}
	IntegerNode struct {
		Pos   lexer.Position
		Value int64 `parser:"@Integer"`
	}
	FloatNode struct {
		Pos   lexer.Position
		Value float64 `parser:"@Float"`
	}
	SymbolNode struct {
		Pos   lexer.Position
		Value string `parser:"@Symbol"`
	}
	VariableNode struct {
		Pos        lexer.Position
		Identifier *SymbolNode         `parser:"'(' 'var' @@"`
		Content    ExpressionNodeUnion `parser:"'=' @@ ')'"`
	}
	BlockNode struct {
		Pos     lexer.Position
		Content []BlockNodeUnion `parser:"'{' @@+ '}'"`
		Env     Enviroment
	}
	SetNode struct {
		Pos        lexer.Position
		Identifier *SymbolNode         `parser:"'(' 'set' @@"`
		Content    ExpressionNodeUnion `parser:"'=' @@ ')'"`
	}
	BreakNode struct {
		Pos     lexer.Position
		Content ExpressionNodeUnion `parser:"'(' 'break' @@? ')'"`
	}
)

const (
	IllegalNK NodeKind = iota
	IntegerNK
	FloatNK
	SymbolNK
	OperationNK
	CallNK
	VariableNK
	SetNK
	BlockNK
	BreakNK
)

func (node *IntegerNode) Position() lexer.Position {
	return node.Pos
}
func (node *FloatNode) Position() lexer.Position {
	return node.Pos
}
func (node *OperaitonNode) Position() lexer.Position {
	return node.Pos
}
func (node *CallNode) Position() lexer.Position {
	return node.Pos
}
func (node *SymbolNode) Position() lexer.Position {
	return node.Pos
}
func (node *VariableNode) Position() lexer.Position {
	return node.Pos
}
func (node *BlockNode) Position() lexer.Position {
	return node.Pos
}
func (node *SetNode) Position() lexer.Position {
	return node.Pos
}
func (node *BreakNode) Position() lexer.Position {
	return node.Pos
}

func (*IntegerNode) Kind() NodeKind {
	return IntegerNK
}
func (*FloatNode) Kind() NodeKind {
	return FloatNK
}
func (*OperaitonNode) Kind() NodeKind {
	return OperationNK
}
func (*CallNode) Kind() NodeKind {
	return CallNK
}
func (*SymbolNode) Kind() NodeKind {
	return SymbolNK
}
func (*VariableNode) Kind() NodeKind {
	return VariableNK
}
func (*BlockNode) Kind() NodeKind {
	return BlockNK
}
func (*SetNode) Kind() NodeKind {
	return SetNK
}
func (*BreakNode) Kind() NodeKind {
	return BreakNK
}

// Semantic
func Semantic(state *State, node Node) (Type, error) {
	switch node := node.(type) {
	case *IntegerNode:
		return &IntegerType{}, nil
	case *FloatNode:
		return &FloatType{}, nil
	case *SymbolNode:
		symb := state.Env.LookupAll(node.Value)
		if symb == nil {
			return nil, fmt.Errorf("symbol '%s' not found", node.Value)
		}
		tp := symb.Type()
		if tp == nil {
			return nil, fmt.Errorf("symbol have't type")
		}
		return tp, nil
	case *VariableNode:
		expType, err := Semantic(state, node.Content)
		if err != nil {
			return nil, err
		}
		if expType == nil {
			return nil, fmt.Errorf("can't initialize variable without knowning type")
		}
		state.Env.Insert(
			&VariableSymbol{
				Ident: node.Identifier,
				Typ:   expType,
			},
		)
		return nil, nil
	case *SetNode:
		expType, err := Semantic(state, node.Content)
		if err != nil {
			return nil, err
		}
		if expType == nil {
			return nil, fmt.Errorf("can't set new value to variable without knowning type")
		}
		symbType, err := Semantic(state, node.Identifier)
		if err != nil {
			return nil, err
		}
		if !CompareTypes(symbType, expType) {
			return nil, fmt.Errorf("can't set new value to '%s': types mismatched", node.Identifier.Value)
		}
		return nil, nil
	case *BlockNode:
		blockState := NewState(
			NewEnviroment("block", state.Env),
		)
		defer func() {
			node.Env = *blockState.Env
		}()
		for index := range node.Content {
			nodeItem := node.Content[index]
			tp, err := Semantic(blockState, nodeItem)
			if err != nil {
				return nil, err
			}
			if blockState.IsBreak {
				return tp, nil
			}
		}
		return nil, nil
	case *BreakNode:
		state.IsBreak = true
		if node.Content == nil {
			return nil, nil
		}
		expType, err := Semantic(state, node.Content)
		if err != nil {
			return nil, err
		}
		return expType, nil
	case *CallNode:
		callType, err := Semantic(state, node.Call)
		if err != nil {
			return nil, err
		}
		fnType, ok := callType.(*FuncType)
		if !ok {
			return nil, fmt.Errorf("can't call not function symbol, got %s", callType.Inspect())
		}
		if len(fnType.Inputs) != len(node.Arguments) {
			return nil, fmt.Errorf("can't call func, expect %d inputs, got %d", len(fnType.Inputs), len(node.Arguments))
		}
		for index := range node.Arguments {
			inputItem := node.Arguments[index]
			inputType, err := Semantic(state, inputItem)
			if err != nil {
				return nil, err
			}
			if inputType == nil {
				return nil, fmt.Errorf("can't get input type #%d", index)
			}
			expectedInputType := fnType.Inputs[index]
			if !CompareTypes(expectedInputType, inputType) {
				return nil, fmt.Errorf("can't call func, #%d input type mismatched", index)
			}
		}
		return fnType.Output, nil
	case *OperaitonNode:
		if len(node.Arguments) < 2 {
			return nil, fmt.Errorf("semantic error: operation '%s' expects minimum 2 inputs, got %d", node.Operation, len(node.Arguments))
		}
		var expType Type
		switch node.Operation {
		case "+", "-", "*", "/":
			for index := range node.Arguments {
				arg := node.Arguments[index]
				argType, err := Semantic(state, arg)
				if err != nil {
					return nil, err
				}
				if !IsNumberType(argType) {
					return nil, fmt.Errorf("semantic error: '%s' allows only number arguments, got '%T'", node.Operation, argType)
				}
				if index == 0 {
					expType = argType
					continue
				}
				if !CompareTypes(expType, argType) {
					return nil, fmt.Errorf("semantic error: '%s' argument should be of the same type", node.Operation)
				}
			}
		default:
			return nil, fmt.Errorf("can't make semantic: unexpected operation '%s'", node.Operation)
		}
		node.OperationType = expType
		return expType, nil
	default:
		return nil, fmt.Errorf("can't make semantic for '%T', unexpected node", node)
	}
}

type (
	TypeKind uint
	Type     interface {
		Kind() TypeKind
		Inspect() string
	}
	IntegerType struct{}
	FloatType   struct{}
	FuncType    struct {
		Inputs []Type
		Output Type
	}
)

const (
	IllegalTK TypeKind = iota
	IntegerTK
	FloatTK
	FuncTK
)

func (*IntegerType) Kind() TypeKind {
	return IntegerTK
}
func (*FloatType) Kind() TypeKind {
	return FloatTK
}
func (*FuncType) Kind() TypeKind {
	return FuncTK
}

func (*IntegerType) Inspect() string {
	return "int"
}
func (*FloatType) Inspect() string {
	return "float"
}
func (tp *FuncType) Inspect() string {
	inputs := []string{}
	for index := range tp.Inputs {
		inputItem := tp.Inputs[index]
		inputs = append(inputs, inputItem.Inspect())
	}
	out := "void"
	if tp.Output != nil {
		out = tp.Output.Inspect()
	}
	return fmt.Sprintf("fn[%s]<%s>", strings.Join(inputs, " "), out)
}

func CompareTypes(first, second Type) bool {
	return first.Inspect() == second.Inspect()
}

func IsNumberType(dt Type) bool {
	return dt.Kind() == IntegerTK || dt.Kind() == FloatTK
}

// State
type State struct {
	Env     *Enviroment
	IsBreak bool
}

func NewState(env *Enviroment) *State {
	return &State{
		Env: env,
	}
}

// Symbols
type Enviroment struct {
	Scope   string
	Outer   *Enviroment
	Symbols map[string]Symbol
}

func NewCoreEnviroment() *Enviroment {
	return &Enviroment{
		Scope: "core",
		Symbols: map[string]Symbol{
			"itof": &BuiltinSymbol{
				Ident: "itof",
				Typ: &FuncType{
					Inputs: []Type{
						&IntegerType{},
					},
					Output: &FloatType{},
				},
				Callee: func(v ...Value) (Value, error) {
					if len(v) != 1 {
						return nil, fmt.Errorf("can't call 'itof', expect 1 argument, got %d", len(v))
					}
					value := v[0].(IntegerValue)
					return FloatValue{
						Value: float64(value.Value),
					}, nil
				},
			},
		},
	}
}

func NewEnviroment(scope string, outer *Enviroment) *Enviroment {
	return &Enviroment{
		Scope:   scope,
		Outer:   outer,
		Symbols: map[string]Symbol{},
	}
}

func (env *Enviroment) Insert(sm Symbol) error {
	_, ok := env.Symbols[sm.Name()]
	if ok {
		return fmt.Errorf("can't insert new symbol '%s', already exists in scope '%s'", sm.Name(), env.Scope)
	}
	env.Symbols[sm.Name()] = sm
	return nil
}

func (env *Enviroment) Lookup(name string) Symbol {
	symb, ok := env.Symbols[name]
	if !ok {
		return nil
	}
	return symb
}

func (env *Enviroment) LookupAll(name string) Symbol {
	if symb := env.Lookup(name); symb != nil {
		return symb
	}
	return env.Outer.LookupAll(name)
}

func (env *Enviroment) Inspect(indent int) string {
	symbols := []string{}
	for key := range env.Symbols {
		val := env.Symbols[key]
		symbols = append(symbols, val.Inspect())
	}
	space := strings.Repeat(" ", indent)
	return fmt.Sprintf(
		"=== Env:%s\n%s%s\n",
		env.Scope,
		space,
		strings.Join(symbols, "\n"+space),
	)
}

type (
	SymbolKind uint
	Symbol     interface {
		Kind() SymbolKind
		Name() string
		Value() Value
		Type() Type
		Inspect() string
	}
	VariableSymbol struct {
		Ident *SymbolNode
		Typ   Type
		Val   Value
	}
	BuiltinSymbol struct {
		Ident  string
		Typ    Type
		Callee func(...Value) (Value, error)
	}
)

const (
	IlligalSK SymbolKind = iota
	VariableSK
	FunctionSK
)

func (*VariableSymbol) Kind() SymbolKind {
	return VariableSK
}
func (sm *VariableSymbol) Name() string {
	return sm.Ident.Value
}
func (sm *VariableSymbol) Value() Value {
	return sm.Val
}
func (sm *VariableSymbol) Type() Type {
	return sm.Typ
}
func (sm *VariableSymbol) Inspect() string {
	if sm.Val != nil {
		return fmt.Sprintf("%s -> var %s", sm.Ident.Value, sm.Val.Inspect())

	} else if sm.Typ != nil {
		return fmt.Sprintf("%s -> var %s", sm.Ident.Value, sm.Typ.Inspect())
	}
	return fmt.Sprintf("%s -> var", sm.Ident.Value)
}

func (*BuiltinSymbol) Kind() SymbolKind {
	return FunctionSK
}
func (sm *BuiltinSymbol) Name() string {
	return sm.Ident
}
func (sm *BuiltinSymbol) Value() Value {
	return nil
}
func (sm *BuiltinSymbol) Type() Type {
	return sm.Typ
}
func (sm *BuiltinSymbol) Inspect() string {
	return fmt.Sprintf("%s -> builtin %s", sm.Ident, sm.Typ.Inspect())
}

// Evaluation
func Eval(state *State, node Node) (Value, error) {
	switch node := node.(type) {
	case *IntegerNode:
		return IntegerValue{
			Value: node.Value,
		}, nil
	case *FloatNode:
		return FloatValue{
			Value: node.Value,
		}, nil
	case *SymbolNode:
		symb := state.Env.LookupAll(node.Value)
		if symb == nil {
			return nil, fmt.Errorf("symbol '%s' not found", node.Value)
		}
		val := symb.Value()
		if val == nil {
			return nil, fmt.Errorf("symbol have't value")
		}
		return val, nil
	case *VariableNode:
		val, err := Eval(state, node.Content)
		if err != nil {
			return nil, err
		}
		symb := state.Env.LookupAll(node.Identifier.Value)
		if symb == nil {
			return nil, fmt.Errorf("symbol '%s' not found", node.Identifier.Value)
		}
		v, ok := symb.(*VariableSymbol)
		if !ok {
			return nil, fmt.Errorf("can't assign value to '%s': expect variable symbol", symb.Name())
		}
		v.Val = val
		return nil, nil
	case *SetNode:
		val, err := Eval(state, node.Content)
		if err != nil {
			return nil, err
		}
		symb := state.Env.LookupAll(node.Identifier.Value)
		if symb == nil {
			return nil, fmt.Errorf("symbol '%s' not found", node.Identifier.Value)
		}
		v, ok := symb.(*VariableSymbol)
		if !ok {
			return nil, fmt.Errorf("can't set value to '%s': expect variable symbol", symb.Name())
		}
		v.Val = val
		return nil, nil
	case *BlockNode:
		blockState := NewState(
			NewEnviroment("block", state.Env),
		)
		defer func() {
			fmt.Println(blockState.Env.Inspect(2))
		}()
		maps.Copy(blockState.Env.Symbols, node.Env.Symbols)
		for index := range node.Content {
			nodeItem := node.Content[index]
			val, err := Eval(blockState, nodeItem)
			if err != nil {
				return nil, err
			}
			if blockState.IsBreak {
				return val, nil
			}
		}
		return nil, nil
	case *BreakNode:
		state.IsBreak = true
		if node.Content == nil {
			return nil, nil
		}
		val, err := Eval(state, node.Content)
		if err != nil {
			return nil, err
		}
		return val, nil
	case *CallNode:
		values := []Value{}
		for index := range node.Arguments {
			input := node.Arguments[index]
			value, err := Eval(state, input)
			if err != nil {
				return nil, err
			}
			values = append(values, value)
		}
		symb := state.Env.LookupAll(node.Call.Value)
		if symb == nil {
			return nil, fmt.Errorf("symbol '%s' not found", node.Call.Value)
		}
		switch symb := symb.(type) {
		case *BuiltinSymbol:
			res, err := symb.Callee(values...)
			if err != nil {
				return nil, err
			}
			return res, nil
		default:
			return nil, fmt.Errorf("can't set value to '%s': expect variable symbol", symb.Name())
		}
	case *OperaitonNode:
		values := []Value{}
		for index := range node.Arguments {
			arg := node.Arguments[index]
			val, err := Eval(state, arg)
			if err != nil {
				return nil, err
			}
			values = append(values, val)
		}
		switch node.Operation {
		case "+":
			switch opType := node.OperationType.(type) {
			case *IntegerType:
				res := IntegerValue{}
				for index := range values {
					value := values[index].(IntegerValue)
					res.Value += value.Value
				}
				return res, nil
			case *FloatType:
				res := FloatValue{}
				for index := range values {
					value := values[index].(FloatValue)
					res.Value += value.Value
				}
				return res, nil
			default:
				return nil, fmt.Errorf("can't evaluate '+': unexpected operation type '%T'", opType)
			}
		case "-":
			switch opType := node.OperationType.(type) {
			case *IntegerType:
				res := values[0].(IntegerValue)
				for index := range values[1:] {
					value := values[index+1].(IntegerValue)
					res.Value -= value.Value
				}
				return res, nil
			case *FloatType:
				res := values[0].(FloatValue)
				for index := range values {
					value := values[index+1].(FloatValue)
					res.Value -= value.Value
				}
				return res, nil
			default:
				return nil, fmt.Errorf("can't evaluate '-': unexpected operation type '%T'", opType)
			}
		case "*":
			switch opType := node.OperationType.(type) {
			case *IntegerType:
				res := values[0].(IntegerValue)
				for index := range values[1:] {
					value := values[index+1].(IntegerValue)
					res.Value *= value.Value
				}
				return res, nil
			case *FloatType:
				res := values[0].(FloatValue)
				for index := range values[1:] {
					value := values[index+1].(FloatValue)
					res.Value *= value.Value
				}
				return res, nil
			default:
				return nil, fmt.Errorf("can't evaluate '*': unexpected operation type '%T'", opType)
			}
		case "/":
			switch opType := node.OperationType.(type) {
			case *IntegerType:
				res := values[0].(IntegerValue)
				for index := range values[1:] {
					value := values[index+1].(IntegerValue)
					res.Value /= value.Value
				}
				return res, nil
			case *FloatType:
				res := values[0].(FloatValue)
				for index := range values[1:] {
					value := values[index+1].(FloatValue)
					res.Value /= value.Value
				}
				return res, nil
			default:
				return nil, fmt.Errorf("can't evaluate '/': unexpected operation type '%T'", opType)
			}
		default:
			return nil, fmt.Errorf("can't evaluate: unexpected operation '%s'", node.Operation)
		}
	default:
		return nil, fmt.Errorf("can't evaluate '%T', unexpected node", node)
	}
}

type (
	ValueKind uint
	Value     interface {
		Kind() ValueKind
		Type() Type
		Inspect() string
	}
	IntegerValue struct {
		Value int64
	}
	FloatValue struct {
		Value float64
	}
)

const (
	IllegalVK ValueKind = iota
	IntegerVK
	FloatVK
)

func (IntegerValue) Kind() ValueKind {
	return IntegerVK
}
func (FloatValue) Kind() ValueKind {
	return FloatVK
}

func (IntegerValue) Type() Type {
	return &IntegerType{}
}
func (FloatValue) Type() Type {
	return &FloatType{}
}

func (vl IntegerValue) Inspect() string {
	return fmt.Sprintf("%d", vl.Value)
}
func (vl FloatValue) Inspect() string {
	return fmt.Sprintf("%f", vl.Value)
}
