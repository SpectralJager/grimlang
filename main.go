package main

import (
	"fmt"
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
			{Name: "Symbol", Pattern: `\??[a-zA-Z]+[a-zA-Z0-9_]*`},

			{Name: "StringStart", Pattern: `"`, Action: lexer.Push("String")},
			{Name: "Boolean", Pattern: `(true|false)`},
			{Name: "Float", Pattern: `-?[0-9]+\.[0-9]*`},
			{Name: "Integer", Pattern: `-?[0-9]+`},
			{Name: "Unit", Pattern: `\(\)`},

			{Name: "++", Pattern: `\+\+`},
			{Name: "+", Pattern: `\+`},
			{Name: "-", Pattern: `-`},
			{Name: "*", Pattern: `\*`},
			{Name: "/", Pattern: `\/`},
			{Name: "<>", Pattern: `<>`},
			{Name: "<", Pattern: `<`},
			{Name: ">", Pattern: `>`},
			{Name: "=", Pattern: `=`},
			{Name: "<=", Pattern: `<=`},
			{Name: ">=", Pattern: `>=`},
			{Name: "|", Pattern: `\|`},
			{Name: "&", Pattern: `&`},

			{Name: "(", Pattern: `\(`},
			{Name: ")", Pattern: `\)`},
			{Name: "{", Pattern: `{`},
			{Name: "}", Pattern: `}`},
		},
		"String": {
			{Name: "StringEnd", Pattern: `"`, Action: lexer.Pop()},
			{Name: "String", Pattern: `(\\"|[^"])*`},
		},
	})
	Parser = participle.MustBuild[BlockNode](
		participle.Lexer(Lexer),
		participle.UseLookahead(1),
		participle.Union[BlockUnionNode](
			&UnitNode{},
			&OperationNode{},
			&DefNode{},
		),
		participle.Union[ExpressionUnionNode](
			&BlockNode{},
			&OperationNode{},
			&SymbolNode{},
			&IntegerNode{},
			&FloatNode{},
			&BoolNode{},
			&StringNode{},
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
	_node               struct {
		Pos lexer.Position
	}
	BlockNode struct {
		_node
		Body       []BlockUnionNode `parser:"'{' @@+ '}'"`
		ReturnType Type
		Env        Enviroment
	}
	OperationNode struct {
		_node
		Operation     string                `parser:"'(' @('+'|'-'|'*'|'/'|'<'|'='|'<>'|'>'|'<='|'>='|'|'|'&'|'++')"`
		Operands      []ExpressionUnionNode `parser:"@@ @@+ ')'"`
		OperationType Type
		OperandsType  Type
	}
	DefNode struct {
		_node
		Identifier  *SymbolNode         `parser:"'(' 'def' @@"`
		Content     ExpressionUnionNode `parser:"@@ ')'"`
		ContentType Type
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
)

func (node *_node) Position() lexer.Position {
	return node.Pos
}

func InspectNode(node Node) string {
	switch node := node.(type) {
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
	case *BlockNode:
		return "block_node"
	case *OperationNode:
		return fmt.Sprintf("operation_%s", node.Operation)
	default:
		return "invalid_node"
	}
}

// Enviroment

type Enviroment struct {
	Name    string
	Symbols map[string]*Symbol
	Parent  *Enviroment
}

func NewEnviroment(name string, parent *Enviroment) *Enviroment {
	return &Enviroment{
		Name:    name,
		Parent:  parent,
		Symbols: map[string]*Symbol{},
	}
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
		Ident *SymbolNode
		Type  Type
		Value Value
	}
)

func InspectSymbol(sm *Symbol) string {
	return fmt.Sprintf("%s -> %s", sm.Ident.Value, InspectType(sm.Type))
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
	default:
		return "invalid_value"
	}
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
	_type       struct{}
	UnitType    struct{ _type }
	IntegerType struct{ _type }
	FloatType   struct{ _type }
	BoolType    struct{ _type }
	StringType  struct{ _type }
)

func (*_type) tp() {}

var types = map[string]Type{
	"unit":   &UnitType{},
	"int":    &IntegerType{},
	"float":  &FloatType{},
	"bool":   &BoolType{},
	"string": &StringType{},
}

func InspectType(typ Type) string {
	switch typ.(type) {
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

func isUnitType(a Type) bool {
	return CompareTypes(types["unit"], a)
}

// Type checking

func TypeChecke(env *Enviroment, node Node) (Type, error) {
	switch node := node.(type) {
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
	case *SymbolNode:
		sm, err := env.LookupAll(node.Value)
		if err != nil {
			return nil, fmt.Errorf("%s -> %w", node.Position(), err)
		}
		return sm.Type, nil
	case *DefNode:
		contType, err := TypeChecke(env, node.Content)
		if err != nil {
			return nil, err
		}
		if isUnitType(contType) {
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
			typ, err := TypeChecke(blockEnv, content)
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
	case *OperationNode:
		opTyp, err := TypeChecke(env, node.Operands[0])
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
		typ, err := TypeChecke(env, operand)
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
	case *SymbolNode:
		sm, err := env.LookupAll(node.Value)
		if err != nil {
			return nil, fmt.Errorf("%s -> %w", node.Position(), err)
		}
		return sm.Value, nil
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
		blockEnv := node.Env
		retVal := Value(UnitValue{})
		for index, content := range node.Body {
			val, err := Eval(&blockEnv, content)
			if err != nil {
				return nil, err
			}
			if index == len(node.Body)-1 {
				retVal = val
			}
		}
		return retVal, nil
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
