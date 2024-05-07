package main

import (
	"fmt"

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
		"Root": []lexer.Rule{
			{Name: "wspace", Pattern: `[ \r\t\n]+`},

			{Name: "Boolean", Pattern: `(true|false)`},
			{Name: "Float", Pattern: `-?[0-9]+\.[0-9]*`},
			{Name: "Integer", Pattern: `-?[0-9]+`},

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
		},
	})
	Parser = participle.MustBuild[OperationNode](
		participle.Lexer(Lexer),
		participle.UseLookahead(1),
		participle.Union[ExpressionNode](
			&OperationNode{},
			&IntegerNode{},
			&FloatNode{},
			&BoolNode{},
		),
	)
)

// AST
type (
	Node interface {
		Position() lexer.Position
	}
	ExpressionNode interface{ Node }
	_node          struct {
		Pos lexer.Position
	}
	OperationNode struct {
		_node
		Operation     string           `parser:"'(' @('+'|'-'|'*'|'/'|'<'|'='|'<>'|'>'|'<='|'>='|'|'|'&')"`
		Operands      []ExpressionNode `parser:"@@ @@+ ')'"`
		OperationType Type
		OperandsType  Type
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
)

func (node *_node) Position() lexer.Position {
	return node.Pos
}

func InspectNode(node Node) string {
	switch node := node.(type) {
	case *IntegerNode:
		return "integer_node"
	case *FloatNode:
		return "float_node"
	case *BoolNode:
		return "bool_node"
	case *OperationNode:
		return fmt.Sprintf("operation_%s", node.Operation)
	default:
		return "invalid_node"
	}
}

// Value

type (
	Value interface {
		value()
	}
	_value       struct{}
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
)

func (_value) value() {}

func InspectValue(value Value) string {
	switch value := value.(type) {
	case IntegerValue:
		return fmt.Sprintf("%d", value.Value)
	case FloatValue:
		return fmt.Sprintf("%f", value.Value)
	case BoolValue:
		return fmt.Sprintf("%v", value.Value)
	default:
		return "invalid_value"
	}
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
		return FloatValue{}, fmt.Errorf("can't convert value to integer")
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
	IntegerType struct{ _type }
	FloatType   struct{ _type }
	BoolType    struct{ _type }
)

func (*_type) tp() {}

func InspectType(typ Type) string {
	switch typ.(type) {
	case *IntegerType:
		return "int"
	case *FloatType:
		return "float"
	case *BoolType:
		return "bool"
	default:
		return "illigal"
	}
}

func CompareTypes(a, b Type) bool {
	return InspectType(a) == InspectType(b)
}

func IsNumberType(a Type) bool {
	return CompareTypes(&IntegerType{}, a) || CompareTypes(&FloatType{}, a)
}

func IsComparableType(a Type) bool {
	return IsNumberType(a) || IsBooleanType(a)
}

func IsBooleanType(a Type) bool {
	return CompareTypes(&BoolType{}, a)
}

// Type checking

func TypeChecker(node Node) (Type, error) {
	switch node := node.(type) {
	case *IntegerNode:
		return &IntegerType{}, nil
	case *FloatNode:
		return &FloatType{}, nil
	case *BoolNode:
		return &BoolType{}, nil
	case *OperationNode:
		opTyp, err := TypeChecker(node.Operands[0])
		if err != nil {
			return nil, err
		}
		switch node.Operation {
		case "+", "-", "*", "/":
			if !IsNumberType(opTyp) {
				return nil, fmt.Errorf("%s -> can't use not number type in '%s' operation", node.Position(), node.Operation)
			}
			node.OperationType = opTyp
			node.OperandsType = opTyp
			return TypeCheckOperatorOperands(node)
		case "=", "<>", "<", ">", "<=", ">=":
			if !IsComparableType(opTyp) {
				return nil, fmt.Errorf("%s -> can't use not comparable type in '%s' operation", node.Position(), node.Operation)
			}
			node.OperationType = &BoolType{}
			node.OperandsType = opTyp
			return TypeCheckOperatorOperands(node)
		case "|", "&":
			if !IsBooleanType(opTyp) {
				return nil, fmt.Errorf("%s -> can't use not boolean type in '%s' operation", node.Position(), node.Operation)
			}
			node.OperationType = &BoolType{}
			node.OperandsType = opTyp
			return TypeCheckOperatorOperands(node)
		default:
			return nil, fmt.Errorf("%s -> unexpected operation '%s'", node.Position(), node.Operation)
		}
	default:
		return nil, fmt.Errorf("%s -> can't evaluate unexpected node '%s'", node.Position(), InspectNode(node))
	}
}

func TypeCheckOperatorOperands(node *OperationNode) (Type, error) {
	for index, operand := range node.Operands {
		typ, err := TypeChecker(operand)
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

func Eval(node Node) (Value, error) {
	switch node := node.(type) {
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
	case *OperationNode:
		operands := []Value{}
		for _, operand := range node.Operands {
			val, err := Eval(operand)
			if err != nil {
				return nil, err
			}
			operands = append(operands, val)
		}
		switch typ := node.OperandsType.(type) {
		case *IntegerType:
			values, err := ValuesToInteger(operands...)
			if err != nil {
				return nil, err
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
				return nil, err
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
				return nil, err
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
