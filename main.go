package main

import (
	"fmt"
	"os"
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
		| Bytecode

4. Runtime:
	Input -> Bytecode
	Output -> result
*/

// Parser
var (
	Lexer = lexer.MustStateful(lexer.Rules{
		"Root": []lexer.Rule{
			{Name: "wspace", Pattern: `[ \r\t\n]+`},

			{Name: "Def", Pattern: `def`},
			{Name: "Set", Pattern: `var`},
			{Name: "Var", Pattern: `set`},
			{Name: "Cond", Pattern: `cond`},
			{Name: "Case", Pattern: `case`},

			{Name: "IntT", Pattern: `int`},
			{Name: "BoolT", Pattern: `bool`},
			{Name: "StringT", Pattern: `string`},
			{Name: "FloatT", Pattern: `float`},
			{Name: "UnitT", Pattern: `unit`},
			{Name: "CompT", Pattern: `comp`},
			{Name: "ListT", Pattern: `list`},

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
	Parser = participle.MustBuild[ModuleNode](
		participle.Lexer(Lexer),
		participle.UseLookahead(4),
		participle.Union[ModuleUnionNode](
			&ConstNode{},
			&FnNode{},
		),
		participle.Union[ConstantUnionNode](
			&IntegerNode{},
			&FloatNode{},
			&BoolNode{},
			&StringNode{},
		),
		participle.Union[BlockUnionNode](
			&OperationNode{},
			&UnitNode{},
			&SymbolNode{},
			&CondNode{},
			&DefNode{},
			&VarNode{},
			&SetNode{},
			&CallNode{},
		),
		participle.Union[DatatypeUnionNode](
			&PrimitiveNode{},
		),
		participle.Union[ExpressionUnionNode](
			&OperationNode{},
			&ListNode{},
			&SymbolNode{},
			&IntegerNode{},
			&FloatNode{},
			&BoolNode{},
			&StringNode{},
			&BlockNode{},
			&CondNode{},
			&CaseNode{},
			&LambdaNode{},
			&CallNode{},
		),
	)
)

// AST
type (
	Node interface {
		Position() lexer.Position
	}
	ExpressionUnionNode interface{ Node }
	BlockUnionNode      interface{ Node }
	ModuleUnionNode     interface{ Node }
	DatatypeUnionNode   interface{ Node }
	ConstantUnionNode   interface{ Node }
	_node               struct {
		Pos lexer.Position
	}
	BlockNode struct {
		_node
		Body       []BlockUnionNode `parser:"'{' @@+ '}'"`
		ReturnType Type
		Env        Enviroment
	}
	ModuleNode struct {
		_node
		Body []ModuleUnionNode `parser:"@@+"`
		Env  Enviroment
	}
	OperationNode struct {
		_node
		Operation     string                `parser:"'(' @('+'|'-'|'*'|'/'|'<'|'='|'<>'|'>'|'<='|'>='|'|'|'&'|'++')"`
		Operands      []ExpressionUnionNode `parser:"@@ @@+ ')'"`
		OperationType Type
		OperandsType  Type
	}
	CondNode struct {
		_node
		Cases     []*CaseNode         `parser:"'(' 'cond' @@+"`
		Else      ExpressionUnionNode `parser:"@@ ')'"`
		CasesType Type
	}
	CaseNode struct {
		_node
		Condition   ExpressionUnionNode `parser:"'(' 'case' @@"`
		Content     ExpressionUnionNode `parser:"@@ ')'"`
		ContentType Type
	}
	FnNode struct {
		_node
		Identifier *SymbolNode         `parser:"'(' 'def' @@"`
		Inputs     []*InputNode        `parser:"'(' 'fn' ('[' @@+ ']')?"`
		Output     *PrimitiveNode      `parser:"'<' @@ '>' "`
		Content    ExpressionUnionNode `parser:"@@ ')' ')'"`
		Type       *FunctionType
		Env        Enviroment
	}
	ConstNode struct {
		_node
		Identifier   *SymbolNode       `parser:"'(' 'def' @@"`
		Constant     ConstantUnionNode `parser:"@@ ')'"`
		ConstantType Type
	}
	CallNode struct {
		_node
		Callable   *SymbolNode           `parser:"'(' @@"`
		Inputs     []ExpressionUnionNode `parser:" @@* ')'"`
		ReturnType Type
	}
	DefNode struct {
		_node
		Identifier  *SymbolNode         `parser:"'(' 'def' @@"`
		Content     ExpressionUnionNode `parser:"@@ ')'"`
		ContentType Type
	}
	SetNode struct {
		_node
		Identifier  *SymbolNode         `parser:"'(' 'set' @@"`
		Content     ExpressionUnionNode `parser:"'=' @@ ')'"`
		ContentType Type
	}
	VarNode struct {
		_node
		Identifier  *SymbolNode         `parser:"'(' 'var' @@"`
		Content     ExpressionUnionNode `parser:"'=' @@ ')'"`
		ContentType Type
	}
	LambdaNode struct {
		_node
		Inputs  []*InputNode        `parser:"'(' 'fn' ('[' @@+ ']')? "`
		Output  DatatypeUnionNode   `parser:"'<' @@ '>'"`
		Content ExpressionUnionNode `parser:"@@ ')'"`
		Type    *FunctionType
		Env     Enviroment
	}
	InputNode struct {
		_node
		Identifier *SymbolNode       `parser:"@@"`
		Datatype   DatatypeUnionNode `parser:"':' @@"`
		Type       Type
	}
	PrimitiveNode struct {
		_node
		Datatype string `parser:"@('int'|'float'|'string'|'bool'|'unit')"`
	}
	SymbolNode struct {
		_node
		Value string `parser:"@Symbol"`
	}
	IntegerNode struct {
		_node
		Value int `parser:"@Integer"`
	}
	FloatNode struct {
		_node
		Value float64 `parser:"@Float"`
	}
	BoolNode struct {
		_node
		Value string `parser:"@Boolean"`
	}
	StringNode struct {
		_node
		Value string `parser:"StringStart @String StringEnd"`
	}
	UnitNode struct {
		_node
		Value string `parser:"'()'"`
	}
	ListNode struct {
		_node
		SubType DatatypeUnionNode     `parser:"'list' '<' @@ '>'"`
		Items   []ExpressionUnionNode `parser:"'{' @@* '}'"`
	}
)

func (node *_node) Position() lexer.Position {
	return node.Pos
}

func InspectNode(node Node) string {
	switch node := node.(type) {
	case *ModuleNode:
		return "module_node"
	case *UnitNode:
		return "unit_node"
	case *IntegerNode:
		return "integer_node"
	case *FloatNode:
		return "float_node"
	case *BoolNode:
		return "bool_node"
	case *StringNode:
		return "string_node"
	case *SymbolNode:
		return "symbol_node"
	case *DefNode:
		return "def_node"
	case *VarNode:
		return "var_node"
	case *SetNode:
		return "set_node"
	case *BlockNode:
		return "block_node"
	case *CallNode:
		return "call_node"
	case *PrimitiveNode:
		return "primitive_node"
	case *LambdaNode:
		return "lambda_node"
	case *InputNode:
		return "input_node"
	case *CaseNode:
		return "case_node"
	case *CondNode:
		return "cond_node"
	case *ConstNode:
		return "const_node"
	case *FnNode:
		return "fn_node"
	case *ListNode:
		return "list_node"
	case *OperationNode:
		return fmt.Sprintf("operation_%s", node.Operation)
	default:
		return "invalid_node"
	}
}

// Enviroment

type EnviromentKind int

const (
	LocalEK EnviromentKind = iota
	BuiltinEK
	GlobalEK
)

type Enviroment struct {
	Name         string
	Symbols      map[string]*Symbol
	Parent       *Enviroment
	Kind         EnviromentKind
	IsCollecting bool
}

func NewEnviroment(name string, parent *Enviroment) *Enviroment {
	return &Enviroment{
		Name:    name,
		Parent:  parent,
		Symbols: map[string]*Symbol{},
	}
}

func (env *Enviroment) CloneSymbols(other *Enviroment) error {
	for _, symb := range other.Symbols {
		newSymb := *symb
		env.Insert(&newSymb)
	}
	return nil
}

func (env *Enviroment) ClosestEnv(kind EnviromentKind) *Enviroment {
	if env.Kind == kind {
		return env
	}
	if env.Parent != nil {
		return env.Parent.ClosestEnv(kind)
	}
	return nil
}

func (env *Enviroment) Insert(sm *Symbol) error {
	_, ok := env.Symbols[sm.Ident.Value]
	if ok {
		return fmt.Errorf("symbol '%s' already exists in enviroment %s", sm.Ident.Value, env.Name)
	}
	env.Symbols[sm.Ident.Value] = sm
	return nil
}

func (env *Enviroment) Lookup(ident string) (*Symbol, error) {
	sm, ok := env.Symbols[ident]
	if !ok {
		return nil, fmt.Errorf("symbol '%s' not found in enviroment %s", ident, env.Name)
	}
	return sm, nil
}

func (env *Enviroment) LookupAll(ident string) (*Symbol, error) {
	sm, err := env.Lookup(ident)
	if err == nil {
		return sm, nil
	}
	if env.Parent == nil {
		return nil, err
	}
	return env.Parent.LookupAll(ident)
}

func (env *Enviroment) Inspect() string {
	symbols := []string{}
	for _, sm := range env.Symbols {
		symbols = append(symbols, InspectSymbol(sm))
	}
	return fmt.Sprintf("=== Env:%s |>\n\t%s\n", env.Name, strings.Join(symbols, "\n\t"))
}

// Symbol

type (
	Symbol struct {
		Ident     *SymbolNode
		Type      Type
		Value     Value
		IsMutable bool
		IsBuiltin bool
	}
)

func InspectSymbol(sm *Symbol) string {
	switch {
	case sm.IsMutable:
		return fmt.Sprintf("%s -> mut %s", sm.Ident.Value, InspectType(sm.Type))
	case sm.IsBuiltin:
		return fmt.Sprintf("%s -> builtin %s", sm.Ident.Value, InspectType(sm.Type))
	default:
		return fmt.Sprintf("%s -> %s", sm.Ident.Value, InspectType(sm.Type))
	}
}

// Value

type (
	Value interface {
		value()
	}
	_value       struct{}
	UnitValue    struct{ _value }
	IntegerValue struct {
		_value
		Value int
	}
	FloatValue struct {
		_value
		Value float64
	}
	BoolValue struct {
		_value
		Value bool
	}
	StringValue struct {
		_value
		Value string
	}
	BuiltinFuncValue struct {
		_value
		Fn func(...Value) (Value, error)
	}
	FuncValue struct {
		_value
		Inputs  []*InputNode
		Content ExpressionUnionNode
		Env     Enviroment
	}
	ListValue struct {
		_value
		Items []Value
	}
)

func (_value) value() {}

func InspectValue(value Value) string {
	switch value := value.(type) {
	case UnitValue:
		return "()"
	case IntegerValue:
		return fmt.Sprintf("%d", value.Value)
	case FloatValue:
		return fmt.Sprintf("%f", value.Value)
	case BoolValue:
		return fmt.Sprintf("%v", value.Value)
	case StringValue:
		return fmt.Sprintf("\"%s\"", value.Value)
	case ListValue:
		items := []string{}
		for _, item := range value.Items {
			items = append(items, InspectValue(item))
		}
		return fmt.Sprintf("{%s}", strings.Join(items, " "))
	case BuiltinFuncValue:
		return "builtin_fn"
	case FuncValue:
		return "fn"
	default:
		return "invalid_value"
	}
}

func ValueToList(value Value) (ListValue, error) {
	val, ok := value.(ListValue)
	if !ok {
		return ListValue{}, fmt.Errorf("can't convert value to list")
	}
	return val, nil
}

func ValueToUnit(value Value) (UnitValue, error) {
	val, ok := value.(UnitValue)
	if !ok {
		return UnitValue{}, fmt.Errorf("can't convert value to unit")
	}
	return val, nil
}

func ValueToString(value Value) (StringValue, error) {
	val, ok := value.(StringValue)
	if !ok {
		return StringValue{}, fmt.Errorf("can't convert value to string")
	}
	return val, nil
}

func ValuesToString(values ...Value) ([]StringValue, error) {
	res := []StringValue{}
	for _, val := range values {
		str, err := ValueToString(val)
		if err != nil {
			return nil, err
		}
		res = append(res, str)
	}
	return res, nil
}

func ValueToInteger(value Value) (IntegerValue, error) {
	val, ok := value.(IntegerValue)
	if !ok {
		return IntegerValue{}, fmt.Errorf("can't convert value to integer")
	}
	return val, nil
}

func ValuesToInteger(values ...Value) ([]IntegerValue, error) {
	res := []IntegerValue{}
	for _, val := range values {
		integer, err := ValueToInteger(val)
		if err != nil {
			return nil, err
		}
		res = append(res, integer)
	}
	return res, nil
}

func ValueToFloat(value Value) (FloatValue, error) {
	val, ok := value.(FloatValue)
	if !ok {
		return FloatValue{}, fmt.Errorf("can't convert value to float")
	}
	return val, nil
}

func ValuesToFloat(values ...Value) ([]FloatValue, error) {
	res := []FloatValue{}
	for _, val := range values {
		float, err := ValueToFloat(val)
		if err != nil {
			return nil, err
		}
		res = append(res, float)
	}
	return res, nil
}

func ValueToBool(value Value) (BoolValue, error) {
	val, ok := value.(BoolValue)
	if !ok {
		return BoolValue{}, fmt.Errorf("can't convert value to boolean")
	}
	return val, nil
}

func ValuesToBool(values ...Value) ([]BoolValue, error) {
	res := []BoolValue{}
	for _, val := range values {
		boolean, err := ValueToBool(val)
		if err != nil {
			return nil, err
		}
		res = append(res, boolean)
	}
	return res, nil
}

// Type

type (
	Type interface {
		tp()
	}
	_type        struct{}
	CompType     struct{ _type }
	UnitType     struct{ _type }
	IntegerType  struct{ _type }
	FloatType    struct{ _type }
	BoolType     struct{ _type }
	StringType   struct{ _type }
	FunctionType struct {
		_type
		Inputs []Type
		Output Type
	}
	ListType struct {
		_type
		Subtype Type
	}
)

func (*_type) tp() {}

var types = map[string]Type{
	"comp":   &CompType{},
	"unit":   &UnitType{},
	"int":    &IntegerType{},
	"float":  &FloatType{},
	"bool":   &BoolType{},
	"string": &StringType{},
}

func InspectType(typ Type) string {
	switch typ := typ.(type) {
	case *CompType:
		return "comp"
	case *UnitType:
		return "unit"
	case *IntegerType:
		return "int"
	case *FloatType:
		return "float"
	case *BoolType:
		return "bool"
	case *StringType:
		return "string"
	case *FunctionType:
		inputs := []string{}
		for _, input := range typ.Inputs {
			inputs = append(inputs, InspectType(input))
		}
		if typ.Output == nil {
			return "illigal"
		}
		output := InspectType(typ.Output)
		return fmt.Sprintf("fn[%s]<%s>", strings.Join(inputs, " "), output)
	case *ListType:
		return fmt.Sprintf("list<%s>", InspectType(typ.Subtype))
	default:
		return "illigal"
	}
}

func CompareTypes(a, b Type) bool {
	return InspectType(a) == InspectType(b)
}

func IsNumberType(a Type) bool {
	return IsIntegerType(a) || IsFloatType(a)
}

func IsComparableType(a Type) bool {
	return IsNumberType(a) || IsBooleanType(a) || IsStringType(a)
}

func IsBooleanType(a Type) bool {
	return CompareTypes(types["bool"], a)
}

func IsStringType(a Type) bool {
	return CompareTypes(types["string"], a)
}

func IsIntegerType(a Type) bool {
	return CompareTypes(types["int"], a)
}

func IsFloatType(a Type) bool {
	return CompareTypes(types["float"], a)
}

func IsUnitType(a Type) bool {
	return CompareTypes(types["unit"], a)
}

func IsFunctionType(a Type) bool {
	if _, ok := a.(*FunctionType); ok {
		return true
	}
	return false
}

func IsListType(a Type) bool {
	if _, ok := a.(*ListType); ok {
		return true
	}
	return false
}

func IsCompType(a Type) bool {
	if _, ok := a.(*CompType); ok {
		return true
	}
	return false
}

// Comp type checking

func CompTypeReplace(origin Type, replace Type) (Type, bool) {
	switch typ := origin.(type) {
	default:
		return typ, false
	case *CompType:
		return replace, true
	case *ListType:
		res, ok := CompTypeReplace(typ.Subtype, replace)
		if !ok {
			return typ, false
		}
		typ.Subtype = res
		return typ, true
	case *FunctionType:
		repl := CompTypeExptr(replace)
		for index := range typ.Inputs {
			input := typ.Inputs[index]
			res, ok := CompTypeReplace(input, repl)
			if !ok {
				continue
			}
			typ.Inputs[index] = res
		}
		res, ok := CompTypeReplace(typ.Output, repl)
		if ok {
			typ.Output = res
		}
		return typ, false
	}
}

func CompTypeExptr(replace Type) Type {
	switch replace := replace.(type) {
	case *ListType:
		return replace.Subtype
	default:
		return replace
	}

}

// Type checking

func TypeCheck(env *Enviroment, node Node) (Type, error) {
	switch node := node.(type) {
	case *ModuleNode:
		moduleEnv := NewEnviroment("module", env)
		moduleEnv.Kind = GlobalEK
		moduleEnv.IsCollecting = true
		for _, content := range node.Body {
			_, err := TypeCheck(moduleEnv, content)
			if err != nil {
				return nil, err
			}
		}
		moduleEnv.IsCollecting = false
		for _, content := range node.Body {
			_, err := TypeCheck(moduleEnv, content)
			if err != nil {
				return nil, err
			}
		}
		node.Env = *moduleEnv
		fmt.Println(moduleEnv.Inspect())
		return types["unit"], nil
	case *FnNode:
		if !env.IsCollecting {
			fnEnv := node.Env
			resTyp, err := TypeCheck(&fnEnv, node.Content)
			if err != nil {
				return nil, err
			}
			if !CompareTypes(resTyp, node.Type.Output) {
				return nil, fmt.Errorf("%s -> return types mismatched", node.Position())
			}
			return types["unit"], nil
		}
		fnEnv := NewEnviroment("lambda", env)
		inputs := []Type{}
		for _, input := range node.Inputs {
			inTyp, err := TypeCheck(fnEnv, input)
			if err != nil {
				return nil, err
			}
			inputs = append(inputs, inTyp)
		}
		output, err := TypeCheck(fnEnv, node.Output)
		if err != nil {
			return nil, err
		}
		node.Env = *fnEnv
		node.Type = &FunctionType{
			Inputs: inputs,
			Output: output,
		}
		err = env.Insert(
			&Symbol{
				Ident: node.Identifier,
				Type:  node.Type,
				Value: FuncValue{
					Inputs:  node.Inputs,
					Content: node.Content,
					Env:     node.Env,
				},
			},
		)
		if err != nil {
			return nil, fmt.Errorf("%s -> %w", node.Position(), err)
		}
		return types["unit"], nil
	case *ConstNode:
		if !env.IsCollecting {
			return types["unit"], nil
		}
		val, err := Eval(env, node.Constant)
		if err != nil {
			return nil, err
		}
		typ, err := TypeCheck(env, node.Constant)
		if err != nil {
			return nil, err
		}
		node.ConstantType = typ
		err = env.Insert(
			&Symbol{
				Ident: node.Identifier,
				Type:  node.ConstantType,
				Value: val,
			},
		)
		if err != nil {
			return nil, fmt.Errorf("%s -> %w", node.Position(), err)
		}
		return types["unit"], nil
	case *IntegerNode:
		return types["int"], nil
	case *FloatNode:
		return types["float"], nil
	case *BoolNode:
		return types["bool"], nil
	case *StringNode:
		return types["string"], nil
	case *UnitNode:
		return types["unit"], nil
	case *ListNode:
		subtype, err := TypeCheck(env, node.SubType)
		if err != nil {
			return nil, err
		}
		for _, item := range node.Items {
			itemTyp, err := TypeCheck(env, item)
			if err != nil {
				return nil, err
			}
			if !CompareTypes(subtype, itemTyp) {
				return nil, fmt.Errorf("%s -> list items should be same type", item.Position())
			}
		}
		return &ListType{
			Subtype: subtype,
		}, nil
	case *PrimitiveNode:
		switch node.Datatype {
		case "int":
			return types["int"], nil
		case "float":
			return types["float"], nil
		case "bool":
			return types["bool"], nil
		case "string":
			return types["string"], nil
		case "unit":
			return types["unit"], nil
		default:
			return nil, fmt.Errorf("%s -> unexpected primitive type '%s'", node.Position(), node.Datatype)
		}
	case *SymbolNode:
		sm, err := env.LookupAll(node.Value)
		if err != nil {
			return nil, fmt.Errorf("%s -> %w", node.Position(), err)
		}
		return sm.Type, nil
	case *InputNode:
		typ, err := TypeCheck(env, node.Datatype)
		if err != nil {
			return nil, err
		}
		if IsUnitType(typ) {
			return nil, fmt.Errorf("%s -> can't use unit type for input", node.Position())
		}
		err = env.Insert(
			&Symbol{
				Ident: node.Identifier,
				Type:  typ,
			},
		)
		if err != nil {
			return nil, fmt.Errorf("%s -> %w", node.Position(), err)
		}
		return typ, nil
	case *LambdaNode:
		fnEnv := NewEnviroment("lambda", env)
		inputs := []Type{}
		for _, input := range node.Inputs {
			inTyp, err := TypeCheck(fnEnv, input)
			if err != nil {
				return nil, err
			}
			inputs = append(inputs, inTyp)
		}
		output, err := TypeCheck(fnEnv, node.Output)
		if err != nil {
			return nil, err
		}
		resTyp, err := TypeCheck(fnEnv, node.Content)
		if err != nil {
			return nil, err
		}
		if !CompareTypes(resTyp, output) {
			return nil, fmt.Errorf("%s -> return types mismatched", node.Position())
		}
		node.Env = *fnEnv
		node.Type = &FunctionType{
			Inputs: inputs,
			Output: output,
		}
		return node.Type, nil
	case *SetNode:
		contType, err := TypeCheck(env, node.Content)
		if err != nil {
			return nil, err
		}
		if IsUnitType(contType) {
			return nil, fmt.Errorf("%s -> can't assign unit type to symbol '%s'", node.Position(), node.Identifier.Value)
		}
		node.ContentType = contType
		sm, err := env.LookupAll(node.Identifier.Value)
		if err != nil {
			return nil, err
		}
		if !sm.IsMutable {
			return nil, fmt.Errorf("%s -> can't change value of constant symbol '%s'", node.Position(), node.Identifier.Value)
		}
		if !CompareTypes(sm.Type, contType) {
			return nil, fmt.Errorf("%s -> can't set new value of different type to '%s' of '%s'", node.Position(), node.Identifier.Value, InspectType(sm.Type))
		}
		return types["unit"], nil
	case *VarNode:
		contType, err := TypeCheck(env, node.Content)
		if err != nil {
			return nil, err
		}
		if IsUnitType(contType) {
			return nil, fmt.Errorf("%s -> can't assign unit type to symbol '%s'", node.Position(), node.Identifier.Value)
		}
		node.ContentType = contType
		err = env.Insert(
			&Symbol{
				Ident:     node.Identifier,
				Type:      node.ContentType,
				IsMutable: true,
			},
		)
		if err != nil {
			return nil, fmt.Errorf("%s -> %w", node.Position(), err)
		}
		return types["unit"], nil
	case *DefNode:
		contType, err := TypeCheck(env, node.Content)
		if err != nil {
			return nil, err
		}
		if IsUnitType(contType) {
			return nil, fmt.Errorf("%s -> can't assign unit type to symbol '%s'", node.Position(), node.Identifier.Value)
		}
		node.ContentType = contType
		err = env.Insert(
			&Symbol{
				Ident: node.Identifier,
				Type:  node.ContentType,
			},
		)
		if err != nil {
			return nil, fmt.Errorf("%s -> %w", node.Position(), err)
		}
		return types["unit"], nil
	case *BlockNode:
		blockEnv := NewEnviroment("block", env)
		for index, content := range node.Body {
			typ, err := TypeCheck(blockEnv, content)
			if err != nil {
				return nil, err
			}
			if index == len(node.Body)-1 {
				node.ReturnType = typ
			}
		}
		node.Env = *blockEnv
		fmt.Println(blockEnv.Inspect())
		return node.ReturnType, nil
	case *CallNode:
		typ, err := TypeCheck(env, node.Callable)
		if err != nil {
			return nil, err
		}
		fnTyp, ok := typ.(*FunctionType)
		if !ok {
			return nil, fmt.Errorf("%s -> callable should be function, got '%s'", node.Position(), InspectType(fnTyp))
		}
		if fnTyp.Output == nil {
			return nil, fmt.Errorf("%s -> function should contain return type", node.Position())
		}
		if len(node.Inputs) != len(fnTyp.Inputs) {
			return nil, fmt.Errorf("%s -> number of inputs mismatched", node.Position())
		}
		if len(fnTyp.Inputs) != 0 {
			compTyp, err := TypeCheck(env, node.Inputs[0])
			if err != nil {
				return nil, err
			}
			fmt.Println("before: ", InspectType(fnTyp))
			res, _ := CompTypeReplace(fnTyp, compTyp)
			fmt.Println("after: ", InspectType(res))
			fnTyp = res.(*FunctionType)
		}
		for index, input := range node.Inputs {
			inTyp, err := TypeCheck(env, input)
			if err != nil {
				return nil, err
			}
			if !CompareTypes(fnTyp.Inputs[index], inTyp) {
				return nil, fmt.Errorf("%s -> #%d input type mismatched", input.Position(), index)
			}
		}
		node.ReturnType = fnTyp.Output
		return node.ReturnType, nil
	case *CaseNode:
		condType, err := TypeCheck(env, node.Condition)
		if err != nil {
			return nil, err
		}
		if !IsBooleanType(condType) {
			return nil, fmt.Errorf("%s -> condition type should be bool, got %s", node.Position(), InspectType(condType))
		}
		contType, err := TypeCheck(env, node.Content)
		if err != nil {
			return nil, err
		}
		node.ContentType = contType
		return contType, nil
	case *CondNode:
		csType := Type(types["unit"])
		for i, cs := range node.Cases {
			typ, err := TypeCheck(env, cs)
			if err != nil {
				return nil, err
			}
			if i == 0 {
				csType = typ
				continue
			}
			if !CompareTypes(csType, typ) {
				return nil, fmt.Errorf("%s -> content type of cases should be same, expect %s -- got %s", cs.Position(), InspectType(csType), InspectType(typ))
			}
		}
		elseType, err := TypeCheck(env, node.Else)
		if err != nil {
			return nil, err
		}
		if !CompareTypes(csType, elseType) {
			return nil, fmt.Errorf("%s -> content type of cases should be same, expect %s -- got %s", node.Else.Position(), InspectType(csType), InspectType(elseType))
		}
		node.CasesType = csType
		return csType, nil
	case *OperationNode:
		opTyp, err := TypeCheck(env, node.Operands[0])
		if err != nil {
			return nil, err
		}
		node.OperandsType = opTyp
		switch node.Operation {
		case "+", "-", "*", "/":
			if !IsNumberType(opTyp) {
				return nil, fmt.Errorf("%s -> can't use not number type in '%s' operation", node.Position(), node.Operation)
			}
			node.OperationType = opTyp
			return TypeCheckOperatorOperands(env, node)
		case "=", "<>", "<", ">", "<=", ">=":
			if !IsComparableType(opTyp) {
				return nil, fmt.Errorf("%s -> can't use not comparable type in '%s' operation", node.Position(), node.Operation)
			}
			node.OperationType = types["bool"]
			return TypeCheckOperatorOperands(env, node)
		case "|", "&":
			if !IsBooleanType(opTyp) {
				return nil, fmt.Errorf("%s -> can't use not boolean type in '%s' operation", node.Position(), node.Operation)
			}
			node.OperationType = opTyp
			return TypeCheckOperatorOperands(env, node)
		case "++":
			if !IsStringType(opTyp) {
				return nil, fmt.Errorf("%s -> can't use not string type in '%s' operation", node.Position(), node.Operation)
			}
			node.OperationType = opTyp
			return TypeCheckOperatorOperands(env, node)
		default:
			return nil, fmt.Errorf("%s -> unexpected operation '%s'", node.Position(), node.Operation)
		}
	default:
		return nil, fmt.Errorf("%s -> can't check unexpected node '%s'", node.Position(), InspectNode(node))
	}
}

func TypeCheckOperatorOperands(env *Enviroment, node *OperationNode) (Type, error) {
	for index, operand := range node.Operands {
		typ, err := TypeCheck(env, operand)
		if err != nil {
			return nil, err
		}
		if !CompareTypes(node.OperandsType, typ) {
			return nil, fmt.Errorf(
				"%s -> operands should be same type '%s', #%d got '%s'",
				operand.Position(),
				InspectType(node.OperationType),
				index,
				InspectType(typ),
			)
		}
	}
	return node.OperationType, nil
}

// Evaluator

func Eval(env *Enviroment, node Node) (Value, error) {
	switch node := node.(type) {
	case *ModuleNode:
		moduleEnv := node.Env
		moduleEnv.Kind = GlobalEK
		_, err := Eval(&moduleEnv, &CallNode{
			Callable: &SymbolNode{
				Value: "main",
			},
		})
		if err != nil {
			return nil, err
		}
		return UnitValue{}, nil
	case *UnitNode:
		return UnitValue{}, nil
	case *IntegerNode:
		return IntegerValue{
			Value: node.Value,
		}, nil
	case *FloatNode:
		return FloatValue{
			Value: node.Value,
		}, nil
	case *BoolNode:
		if node.Value == "true" {
			return BoolValue{
				Value: true,
			}, nil
		}
		return BoolValue{
			Value: false,
		}, nil
	case *StringNode:
		return StringValue{
			Value: node.Value,
		}, nil
	case *ListNode:
		items := []Value{}
		for _, item := range node.Items {
			itemVal, err := Eval(env, item)
			if err != nil {
				return nil, err
			}
			items = append(items, itemVal)
		}
		return ListValue{
			Items: items,
		}, nil
	case *SymbolNode:
		sm, err := env.LookupAll(node.Value)
		if err != nil {
			return nil, fmt.Errorf("%s -> %w", node.Position(), err)
		}
		return sm.Value, nil
	case *SetNode:
		contVal, err := Eval(env, node.Content)
		if err != nil {
			return nil, err
		}
		sm, err := env.LookupAll(node.Identifier.Value)
		if err != nil {
			return nil, err
		}
		sm.Value = contVal
		return UnitValue{}, nil
	case *VarNode:
		val, err := Eval(env, node.Content)
		if err != nil {
			return nil, err
		}
		sm, err := env.Lookup(node.Identifier.Value)
		if err != nil {
			return nil, fmt.Errorf("%s -> %w", node.Position(), err)
		}
		sm.Value = val
		return UnitValue{}, nil
	case *DefNode:
		val, err := Eval(env, node.Content)
		if err != nil {
			return nil, err
		}
		sm, err := env.Lookup(node.Identifier.Value)
		if err != nil {
			return nil, fmt.Errorf("%s -> %w", node.Position(), err)
		}
		sm.Value = val
		return UnitValue{}, nil
	case *BlockNode:
		blockEnv := NewEnviroment("block", env)
		blockEnv.CloneSymbols(&node.Env)
		retVal := Value(UnitValue{})
		for index, content := range node.Body {
			val, err := Eval(blockEnv, content)
			if err != nil {
				return nil, err
			}
			if index == len(node.Body)-1 {
				retVal = val
			}
		}
		return retVal, nil
	case *LambdaNode:
		return FuncValue{
			Inputs:  node.Inputs,
			Content: node.Content,
			Env:     node.Env,
		}, nil
	case *CallNode:
		callVal, err := Eval(env, node.Callable)
		if err != nil {
			return nil, err
		}
		inputs := []Value{}
		for _, input := range node.Inputs {
			val, err := Eval(env, input)
			if err != nil {
				return nil, err
			}
			inputs = append(inputs, val)
		}
		switch val := callVal.(type) {
		case BuiltinFuncValue:
			res, err := val.Fn(inputs...)
			if err != nil {
				return nil, fmt.Errorf("%s -> %w", node.Position(), err)
			}
			return res, nil
		case FuncValue:
			fnEnv := NewEnviroment("fn", env.ClosestEnv(GlobalEK))
			fnEnv.CloneSymbols(&val.Env)
			for index, input := range val.Inputs {
				sm, err := fnEnv.Lookup(input.Identifier.Value)
				if err != nil {
					return nil, fmt.Errorf("%s -> %w", node.Position(), err)
				}
				sm.Value = inputs[index]
			}
			res, err := Eval(fnEnv, val.Content)
			if err != nil {
				return nil, err
			}
			return res, nil
		default:
			return nil, fmt.Errorf("%s -> can't call not callable node", node.Position())
		}
	case *CaseNode:
		condVal, err := Eval(env, node.Condition)
		if err != nil {
			return nil, err
		}
		condBool, err := ValueToBool(condVal)
		if err != nil {
			return nil, fmt.Errorf("%s -> %w", node.Position(), err)
		}
		if !condBool.Value {
			return nil, nil
		}
		return Eval(env, node.Content)
	case *CondNode:
		for _, cs := range node.Cases {
			res, err := Eval(env, cs)
			if err != nil {
				return nil, err
			}
			if res != nil {
				return res, nil
			}
		}
		return Eval(env, node.Else)
	case *OperationNode:
		operands := []Value{}
		for _, operand := range node.Operands {
			val, err := Eval(env, operand)
			if err != nil {
				return nil, err
			}
			operands = append(operands, val)
		}
		switch typ := node.OperandsType.(type) {
		case *IntegerType:
			values, err := ValuesToInteger(operands...)
			if err != nil {
				return nil, fmt.Errorf("%s -> %w", node.Position(), err)
			}
			switch node.Operation {
			case "+":
				return AddInt(values...), nil
			case "-":
				return SubInt(values...), nil
			case "*":
				return MulInt(values...), nil
			case "/":
				return DivInt(values...), nil
			case "<":
				return LessInt(values...), nil
			case ">":
				return GreaterInt(values...), nil
			case "=":
				return EqualInt(values...), nil
			case "<>":
				return NeqInt(values...), nil
			case "<=":
				return LeqInt(values...), nil
			case ">=":
				return GeqInt(values...), nil
			default:
				return nil, fmt.Errorf("%s -> unexpected operation '%s' for 'int' type", node.Position(), node.Operation)
			}
		case *FloatType:
			values, err := ValuesToFloat(operands...)
			if err != nil {
				return nil, fmt.Errorf("%s -> %w", node.Position(), err)
			}
			switch node.Operation {
			case "+":
				return AddFloat(values...), nil
			case "-":
				return SubFloat(values...), nil
			case "*":
				return MulFloat(values...), nil
			case "/":
				return DivFloat(values...), nil
			case "<":
				return LessFloat(values...), nil
			case ">":
				return GreaterFloat(values...), nil
			case "=":
				return EqualFloat(values...), nil
			case "<>":
				return NeqFloat(values...), nil
			case "<=":
				return LeqFloat(values...), nil
			case ">=":
				return GeqFloat(values...), nil
			default:
				return nil, fmt.Errorf("%s -> unexpected operation '%s' for 'float' type", node.Position(), node.Operation)
			}
		case *BoolType:
			values, err := ValuesToBool(operands...)
			if err != nil {
				return nil, fmt.Errorf("%s -> %w", node.Position(), err)
			}
			switch node.Operation {
			case "=":
				return EqualBool(values...), nil
			case "<>":
				return NeqBool(values...), nil
			case "|":
				return OrBool(values...), nil
			case "&":
				return AndBool(values...), nil
			default:
				return nil, fmt.Errorf("%s -> unexpected operation '%s' for 'bool' type", node.Position(), node.Operation)
			}
		case *StringType:
			values, err := ValuesToString(operands...)
			if err != nil {
				return nil, fmt.Errorf("%s -> %w", node.Position(), err)
			}
			switch node.Operation {
			case "=":
				return EqualString(values...), nil
			case "<>":
				return NeqString(values...), nil
			case "++":
				return ConcatString(values...), nil
			default:
				return nil, fmt.Errorf("%s -> unexpected operation '%s' for 'string' type", node.Position(), node.Operation)
			}
		default:
			return nil, fmt.Errorf("%s -> unexpected '%s' operation type '%s'",
				node.Position(),
				node.Operation,
				InspectType(typ),
			)
		}
	default:
		return nil, fmt.Errorf("%s -> can't evaluate unexpected node '%s'", node.Position(), InspectNode(node))
	}
}

// Runtime
// Int Functions

func AddInt(operands ...IntegerValue) IntegerValue {
	res := IntegerValue{}
	for _, operand := range operands {
		res.Value += operand.Value
	}
	return res
}

func SubInt(operands ...IntegerValue) IntegerValue {
	res := IntegerValue{}
	for index, operand := range operands {
		if index == 0 {
			res = operand
			continue
		}
		res.Value -= operand.Value
	}
	return res
}

func MulInt(operands ...IntegerValue) IntegerValue {
	res := IntegerValue{}
	for index, operand := range operands {
		if index == 0 {
			res = operand
			continue
		}
		res.Value *= operand.Value
	}
	return res
}

func DivInt(operands ...IntegerValue) IntegerValue {
	res := IntegerValue{}
	for index, operand := range operands {
		if index == 0 {
			res = operand
			continue
		}
		res.Value /= operand.Value
	}
	return res
}

func LessInt(operands ...IntegerValue) BoolValue {
	comp := IntegerValue{}
	for index, operand := range operands {
		if index == 0 {
			comp = operand
			continue
		}
		if comp.Value >= operand.Value {
			return BoolValue{Value: false}
		}
	}
	return BoolValue{Value: true}
}

func GreaterInt(operands ...IntegerValue) BoolValue {
	comp := IntegerValue{}
	for index, operand := range operands {
		if index == 0 {
			comp = operand
			continue
		}
		if comp.Value <= operand.Value {
			return BoolValue{Value: false}
		}
	}
	return BoolValue{Value: true}
}

func EqualInt(operands ...IntegerValue) BoolValue {
	comp := IntegerValue{}
	for index, operand := range operands {
		if index == 0 {
			comp = operand
			continue
		}
		if comp.Value != operand.Value {
			return BoolValue{Value: false}
		}
	}
	return BoolValue{Value: true}
}

func NeqInt(operands ...IntegerValue) BoolValue {
	comp := IntegerValue{}
	for index, operand := range operands {
		if index == 0 {
			comp = operand
			continue
		}
		if comp.Value == operand.Value {
			return BoolValue{Value: false}
		}
	}
	return BoolValue{Value: true}
}

func LeqInt(operands ...IntegerValue) BoolValue {
	comp := IntegerValue{}
	for index, operand := range operands {
		if index == 0 {
			comp = operand
			continue
		}
		if comp.Value > operand.Value {
			return BoolValue{Value: false}
		}
	}
	return BoolValue{Value: true}
}

func GeqInt(operands ...IntegerValue) BoolValue {
	comp := IntegerValue{}
	for index, operand := range operands {
		if index == 0 {
			comp = operand
			continue
		}
		if comp.Value < operand.Value {
			return BoolValue{Value: false}
		}
	}
	return BoolValue{Value: true}
}

// Float functions

func AddFloat(operands ...FloatValue) FloatValue {
	res := FloatValue{}
	for _, operand := range operands {
		res.Value += operand.Value
	}
	return res
}

func SubFloat(operands ...FloatValue) FloatValue {
	res := FloatValue{}
	for index, operand := range operands {
		if index == 0 {
			res = operand
			continue
		}
		res.Value -= operand.Value
	}
	return res
}

func MulFloat(operands ...FloatValue) FloatValue {
	res := FloatValue{}
	for index, operand := range operands {
		if index == 0 {
			res = operand
			continue
		}
		res.Value *= operand.Value
	}
	return res
}

func DivFloat(operands ...FloatValue) FloatValue {
	res := FloatValue{}
	for index, operand := range operands {
		if index == 0 {
			res = operand
			continue
		}
		res.Value /= operand.Value
	}
	return res
}

func LessFloat(operands ...FloatValue) BoolValue {
	comp := FloatValue{}
	for index, operand := range operands {
		if index == 0 {
			comp = operand
			continue
		}
		if comp.Value >= operand.Value {
			return BoolValue{Value: false}
		}
	}
	return BoolValue{Value: true}
}

func GreaterFloat(operands ...FloatValue) BoolValue {
	comp := FloatValue{}
	for index, operand := range operands {
		if index == 0 {
			comp = operand
			continue
		}
		if comp.Value <= operand.Value {
			return BoolValue{Value: false}
		}
	}
	return BoolValue{Value: true}
}

func EqualFloat(operands ...FloatValue) BoolValue {
	comp := FloatValue{}
	for index, operand := range operands {
		if index == 0 {
			comp = operand
			continue
		}
		if comp.Value != operand.Value {
			return BoolValue{Value: false}
		}
	}
	return BoolValue{Value: true}
}

func NeqFloat(operands ...FloatValue) BoolValue {
	comp := FloatValue{}
	for index, operand := range operands {
		if index == 0 {
			comp = operand
			continue
		}
		if comp.Value == operand.Value {
			return BoolValue{Value: false}
		}
	}
	return BoolValue{Value: true}
}

func LeqFloat(operands ...FloatValue) BoolValue {
	comp := FloatValue{}
	for index, operand := range operands {
		if index == 0 {
			comp = operand
			continue
		}
		if comp.Value > operand.Value {
			return BoolValue{Value: false}
		}
	}
	return BoolValue{Value: true}
}

func GeqFloat(operands ...FloatValue) BoolValue {
	comp := FloatValue{}
	for index, operand := range operands {
		if index == 0 {
			comp = operand
			continue
		}
		if comp.Value < operand.Value {
			return BoolValue{Value: false}
		}
	}
	return BoolValue{Value: true}
}

// Boolean funcs

func EqualBool(operands ...BoolValue) BoolValue {
	comp := BoolValue{}
	for index, operand := range operands {
		if index == 0 {
			comp = operand
			continue
		}
		if comp.Value != operand.Value {
			return BoolValue{Value: false}
		}
	}
	return BoolValue{Value: true}
}

func NeqBool(operands ...BoolValue) BoolValue {
	comp := BoolValue{}
	for index, operand := range operands {
		if index == 0 {
			comp = operand
			continue
		}
		if comp.Value == operand.Value {
			return BoolValue{Value: false}
		}
	}
	return BoolValue{Value: true}
}

func OrBool(operands ...BoolValue) BoolValue {
	for _, operand := range operands {
		if operand.Value {
			return BoolValue{Value: true}
		}
	}
	return BoolValue{Value: false}
}

func AndBool(operands ...BoolValue) BoolValue {
	for _, operand := range operands {
		if !operand.Value {
			return BoolValue{Value: false}
		}
	}
	return BoolValue{Value: true}
}

// String funcs

func EqualString(operands ...StringValue) BoolValue {
	comp := StringValue{}
	for index, operand := range operands {
		if index == 0 {
			comp = operand
			continue
		}
		if comp.Value != operand.Value {
			return BoolValue{Value: false}
		}
	}
	return BoolValue{Value: true}
}

func NeqString(operands ...StringValue) BoolValue {
	comp := StringValue{}
	for index, operand := range operands {
		if index == 0 {
			comp = operand
			continue
		}
		if comp.Value == operand.Value {
			return BoolValue{Value: false}
		}
	}
	return BoolValue{Value: true}
}

func ConcatString(operands ...StringValue) StringValue {
	var builder strings.Builder
	for _, operand := range operands {
		builder.WriteString(operand.Value)
	}
	return StringValue{Value: builder.String()}
}

func InsertBuiltinSymbols(env *Enviroment) {
	env.Insert(&Symbol{
		Ident: &SymbolNode{Value: "println"},
		Type: &FunctionType{
			Inputs: []Type{&StringType{}},
			Output: &UnitType{},
		},
		Value: BuiltinFuncValue{
			Fn: Println,
		},
		IsBuiltin: true,
	})
	env.Insert(&Symbol{
		Ident: &SymbolNode{Value: "print"},
		Type: &FunctionType{
			Inputs: []Type{&StringType{}},
			Output: &UnitType{},
		},
		Value: BuiltinFuncValue{
			Fn: Print,
		},
		IsBuiltin: true,
	})
	env.Insert(&Symbol{
		Ident: &SymbolNode{Value: "itof"},
		Type: &FunctionType{
			Inputs: []Type{&IntegerType{}},
			Output: &FloatType{},
		},
		Value: BuiltinFuncValue{
			Fn: Itof,
		},
		IsBuiltin: true,
	})
	env.Insert(&Symbol{
		Ident: &SymbolNode{Value: "itos"},
		Type: &FunctionType{
			Inputs: []Type{&IntegerType{}},
			Output: &StringType{},
		},
		Value: BuiltinFuncValue{
			Fn: Itos,
		},
		IsBuiltin: true,
	})
	env.Insert(&Symbol{
		Ident: &SymbolNode{Value: "ftoi"},
		Type: &FunctionType{
			Inputs: []Type{&FloatType{}},
			Output: &IntegerType{},
		},
		Value: BuiltinFuncValue{
			Fn: Ftoi,
		},
		IsBuiltin: true,
	})
	env.Insert(&Symbol{
		Ident: &SymbolNode{Value: "ftos"},
		Type: &FunctionType{
			Inputs: []Type{&FloatType{}},
			Output: &StringType{},
		},
		Value: BuiltinFuncValue{
			Fn: Ftos,
		},
		IsBuiltin: true,
	})
	env.Insert(&Symbol{
		Ident: &SymbolNode{Value: "ltos"},
		Type: &FunctionType{
			Inputs: []Type{&ListType{Subtype: &CompType{}}},
			Output: &StringType{},
		},
		Value: BuiltinFuncValue{
			Fn: Ltos,
		},
		IsBuiltin: true,
	})
	env.Insert(&Symbol{
		Ident: &SymbolNode{Value: "llen"},
		Type: &FunctionType{
			Inputs: []Type{&ListType{Subtype: &CompType{}}},
			Output: &IntegerType{},
		},
		Value: BuiltinFuncValue{
			Fn: Llen,
		},
		IsBuiltin: true,
	})
	env.Insert(&Symbol{
		Ident: &SymbolNode{Value: "lappend"},
		Type: &FunctionType{
			Inputs: []Type{&ListType{Subtype: &CompType{}}, &CompType{}},
			Output: &ListType{Subtype: &CompType{}},
		},
		Value: BuiltinFuncValue{
			Fn: Ltos,
		},
		IsBuiltin: true,
	})
	env.Insert(&Symbol{
		Ident: &SymbolNode{Value: "readFile"},
		Type: &FunctionType{
			Inputs: []Type{&StringType{}},
			Output: &StringType{},
		},
		Value: BuiltinFuncValue{
			Fn: ReadFile,
		},
		IsBuiltin: true,
	})
}

func Println(inputs ...Value) (Value, error) {
	input, err := ValueToString(inputs[0])
	if err != nil {
		return nil, err
	}
	fmt.Println(input.Value)
	return UnitValue{}, nil
}

func Print(inputs ...Value) (Value, error) {
	input, err := ValueToString(inputs[0])
	if err != nil {
		return nil, err
	}
	fmt.Print(input.Value)
	return UnitValue{}, nil
}

func Itof(inputs ...Value) (Value, error) {
	input, err := ValueToInteger(inputs[0])
	if err != nil {
		return nil, err
	}
	return FloatValue{Value: float64(input.Value)}, nil
}

func Itos(inputs ...Value) (Value, error) {
	input, err := ValueToInteger(inputs[0])
	if err != nil {
		return nil, err
	}
	return StringValue{Value: fmt.Sprint(input.Value)}, nil
}

func Ftoi(inputs ...Value) (Value, error) {
	input, err := ValueToFloat(inputs[0])
	if err != nil {
		return nil, err
	}
	return IntegerValue{Value: int(input.Value)}, nil
}

func Ftos(inputs ...Value) (Value, error) {
	input, err := ValueToFloat(inputs[0])
	if err != nil {
		return nil, err
	}
	return StringValue{Value: fmt.Sprint(input.Value)}, nil
}

func Ltos(inputs ...Value) (Value, error) {
	input, err := ValueToList(inputs[0])
	if err != nil {
		return nil, err
	}
	return StringValue{Value: InspectValue(input)}, nil
}

func Llen(inputs ...Value) (Value, error) {
	input, err := ValueToList(inputs[0])
	if err != nil {
		return nil, err
	}
	return IntegerValue{Value: len(input.Items)}, nil
}

func Lappend(inputs ...Value) (Value, error) {
	lst, err := ValueToList(inputs[0])
	if err != nil {
		return nil, err
	}
	val := inputs[1]
	lst.Items = append(lst.Items, val)
	return lst, nil
}

func ReadFile(inputs ...Value) (Value, error) {
	filename, err := ValueToString(inputs[0])
	if err != nil {
		return nil, err
	}
	data, err := os.ReadFile(filename.Value)
	if err != nil {
		return nil, err
	}
	return StringValue{Value: string(data)}, nil
}
