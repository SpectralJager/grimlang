package grimlang

import (
	"context"
	"fmt"

	"github.com/alecthomas/participle/v2/lexer"
)

type (
	Node interface {
		Position() lexer.Position
		SetAttribute(string, any) error
		GetAttribute(string) (any, error)

		Inspect() string
		Semantic(context.Context) (Type, error)
		Eval(context.Context) (Value, error)
	}
	ExpressionUnionNode interface{ Node }
	BlockUnionNode      interface{ Node }
	ModuleUnionNode     interface{ Node }
	DatatypeUnionNode   interface{ Node }
	ConstantUnionNode   interface{ Node }
	IterableUnionNode   interface{ Node }
	CallableUnionNode   interface{ Node }
	SymbolUnionNode     interface{ Node }
)

type _node struct {
	Pos        lexer.Position
	Attributes map[string]any
}

func (node *_node) Position() lexer.Position {
	return node.Pos
}

func (node *_node) SetAttribute(key string, value any) {
	node.Attributes[key] = value
}

func (node *_node) GetAttribute(key string) any {
	value, ok := node.Attributes[key]
	if !ok {
		return nil
	}
	return value
}

type BlockNode struct {
	_node
	Body []BlockUnionNode `parser:"'{' @@+ '}'"`
}

func (node *BlockNode) Semantic(ctx context.Context) (Type, error) {
	env := ctx.Value("env").(*Enviroment)

	blockEnv := NewEnviroment("block", env)
	node.SetAttribute("env", blockEnv)

	var retType Type
	for _, content := range node.Body {
		ctx := context.WithValue(ctx, "env", blockEnv)
		typ, err := content.Semantic(ctx)
		if err != nil {
			return nil, err
		}
		retType = typ
	}
	node.SetAttribute("retType", retType)

	return retType, nil
}

func (node *BlockNode) Eval(ctx context.Context) (Value, error) {
	env := ctx.Value("env").(*Enviroment)

	blockEnv := NewEnviroment("block", env)
	blockEnv.CloneSymbols(node.GetAttribute("env").(*Enviroment))

	retVal := Value(UnitValue{})
	for index, content := range node.Body {
		ctx := context.WithValue(ctx, "env", blockEnv)
		val, err := content.Eval(ctx)
		if err != nil {
			return nil, err
		}
		if index == len(node.Body)-1 {
			retVal = val
		}
	}

	return retVal, nil
}

type ModuleNode struct {
	_node
	Body []ModuleUnionNode `parser:"@@+"`
}

func (node *ModuleNode) Semantic(ctx context.Context) (Type, error) {
	env := ctx.Value("env").(*Enviroment)

	moduleEnv := NewEnviroment("module", env)
	moduleEnv.Kind = GlobalEK
	moduleEnv.IsCollecting = true

	for _, content := range node.Body {
		ctx := context.WithValue(ctx, "env", moduleEnv)
		_, err := content.Semantic(ctx)
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
	node.SetAttribute("env", moduleEnv)
	return types["unit"], nil
}

func (node *ModuleNode) Eval(ctx context.Context) (Value, error) {

	moduleEnv := node.GetAttribute("env").(*Enviroment)
	moduleEnv.Kind = GlobalEK

	mainCall := &CallNode{
		Callable: &SymbolNode{
			Value: "main",
		},
	}

	ctx = context.WithValue(ctx, "env", moduleEnv)
	_, err := mainCall.Eval(ctx)
	if err != nil {
		return nil, err
	}
	return UnitValue{}, nil
}

type OperationNode struct {
	_node
	Operation string                `parser:"'(' @('+'|'-'|'*'|'/'|'<'|'='|'<>'|'>'|'<='|'>='|'|'|'&'|'++')"`
	Operands  []ExpressionUnionNode `parser:"@@ @@+ ')'"`
}

func (node *OperationNode) Semantic(ctx context.Context) (Type, error) {
	opTyp, err := node.Operands[0].Semantic(ctx)
	if err != nil {
		return nil, err
	}

	switch node.Operation {
	case "+", "-", "*", "/":
		if !IsNumberType(opTyp) {
			return nil, fmt.Errorf("%s -> can't use not number type in '%s' operation", node.Position(), node.Operation)
		}
		node.SetAttribute("operationType", opTyp)
		return node.SemanticOperatorOperans(ctx)
	case "=", "<>", "<", ">", "<=", ">=":
		if !IsComparableType(opTyp) {
			return nil, fmt.Errorf("%s -> can't use not comparable type in '%s' operation", node.Position(), node.Operation)
		}
		node.SetAttribute("operationType", types["bool"])
		return node.SemanticOperatorOperans(ctx)
	case "|", "&":
		if !IsBooleanType(opTyp) {
			return nil, fmt.Errorf("%s -> can't use not boolean type in '%s' operation", node.Position(), node.Operation)
		}
		node.SetAttribute("operationType", opTyp)
		return node.SemanticOperatorOperans(ctx)
	case "++":
		if !IsStringType(opTyp) {
			return nil, fmt.Errorf("%s -> can't use not string type in '%s' operation", node.Position(), node.Operation)
		}
		node.SetAttribute("operationType", opTyp)
		return node.SemanticOperatorOperans(ctx)
	default:
		return nil, fmt.Errorf("%s -> unexpected operation '%s'", node.Position(), node.Operation)
	}

}

func (node *OperationNode) SemanticOperatorOperans(ctx context.Context) (Type, error) {
	operationType := node.GetAttribute("operationType").(Type)
	for index, operand := range node.Operands {
		typ, err := operand.Semantic(ctx)
		if err != nil {
			return nil, err
		}
		if !CompareTypes(operationType, typ) {
			return nil, fmt.Errorf(
				"%s -> operands should be same type '%s', #%d got '%s'",
				operand.Position(),
				InspectType(operationType),
				index,
				InspectType(typ),
			)
		}
	}
	return operationType, nil
}

func (node *OperationNode) Eval(ctx context.Context) (Value, error) {
	operands := []Value{}
	for _, operand := range node.Operands {
		val, err := operand.Eval(ctx)
		if err != nil {
			return nil, err
		}
		operands = append(operands, val)
	}
	operationType := node.GetAttribute("operationType").(Type)
	switch typ := operationType.(type) {
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
}

type WhileNode struct {
	_node
	Condition ExpressionUnionNode `parser:"'(' 'while' @@"`
	Content   ExpressionUnionNode `parser:"@@ ')'"`
	Env       Enviroment
	Typ       Type
}
type RangeNode struct {
	_node
	IndexIdent *SymbolNode         `parser:"'(' 'range' '[' @@"`
	Iterable   IterableUnionNode   `parser:"':' @@ ']'"`
	Content    ExpressionUnionNode `parser:"@@ ')'"`
	Env        Enviroment
	Typ        Type
}
type ForNode struct {
	_node
	IndexIdent *SymbolNode         `parser:"'(' 'for' '[' @@"`
	Start      ExpressionUnionNode `parser:"'=' @@"`
	End        ExpressionUnionNode `parser:"':' @@"`
	Step       ExpressionUnionNode `parser:"':' @@ ']'"`
	Content    ExpressionUnionNode `parser:"@@ ')'"`
	Env        Enviroment
	Typ        Type
}
type CondNode struct {
	_node
	Cases     []*CaseNode         `parser:"'(' 'cond' @@+"`
	Else      ExpressionUnionNode `parser:"@@ ')'"`
	CasesType Type
}
type CaseNode struct {
	_node
	Condition   ExpressionUnionNode `parser:"'(' 'case' @@"`
	Content     ExpressionUnionNode `parser:"@@ ')'"`
	ContentType Type
}
type FnNode struct {
	_node
	Identifier *SymbolNode   `parser:"'(' 'defn' @@"`
	Fns        []*LambdaNode `parser:"@@+ ')'"`
	Types      []*FunctionType
}
type ConstNode struct {
	_node
	Identifier   *SymbolNode       `parser:"'(' 'defc' @@"`
	Constant     ConstantUnionNode `parser:"@@ ')'"`
	ConstantType Type
}
type RecordNode struct {
	_node
	Identifier *SymbolNode     `parser:"'(' 'defr' @@"`
	Record     *RecordTypeNode `parser:"@@ ')'"`
	Type       Type
}
type CallNode struct {
	_node
	Callable   CallableUnionNode     `parser:"'(' @@"`
	Inputs     []ExpressionUnionNode `parser:" @@* ')'"`
	ReturnType Type
	PolyIndex  int
}
type DefNode struct {
	_node
	Identifier  *SymbolNode         `parser:"'(' 'def' @@"`
	Content     ExpressionUnionNode `parser:"@@ ')'"`
	ContentType Type
}
type RecordTypeNode struct {
	_node
	Fields []*InputNode `parser:"'(' 'record' @@+ ')'"`
	Env    Enviroment
}
type SetNode struct {
	_node
	Identifier  *SymbolNode         `parser:"'(' 'set' @@"`
	Content     ExpressionUnionNode `parser:"'=' @@ ')'"`
	ContentType Type
}
type VarNode struct {
	_node
	Identifier  *SymbolNode         `parser:"'(' 'var' @@"`
	Content     ExpressionUnionNode `parser:"'=' @@ ')'"`
	ContentType Type
}
type LambdaNode struct {
	_node
	Inputs  []*InputNode        `parser:"'(' 'fn' ('[' @@+ ']')? "`
	Output  DatatypeUnionNode   `parser:"'<' @@ '>'"`
	Content ExpressionUnionNode `parser:"@@ ')'"`
	Type    *FunctionType
	Env     Enviroment
}
type InputNode struct {
	_node
	Identifier *SymbolNode       `parser:"@@"`
	Datatype   DatatypeUnionNode `parser:"':' @@"`
	Type       Type
}
type PrimitiveNode struct {
	_node
	Datatype string `parser:"@('int'|'float'|'string'|'bool'|'unit')"`
}
type CompositeNode struct {
	_node
	Datatype string            `parser:"@('list')"`
	SubType  DatatypeUnionNode `parser:"'<' @@ '>'"`
}
type FnTypeNode struct {
	_node
	Inputs []DatatypeUnionNode `parser:"'[' @@* ']'"`
	Output DatatypeUnionNode   `parser:"'<' @@ '>'"`
}
type SymbolNode struct {
	_node
	Value string `parser:"@Symbol"`
}
type SymbolSeqNode struct {
	_node
	Primary *SymbolNode     `parser:"@@"`
	Next    SymbolUnionNode `parser:"'/' @@"`
}
type IntegerNode struct {
	_node
	Value int `parser:"@Integer"`
}
type FloatNode struct {
	_node
	Value float64 `parser:"@Float"`
}
type BoolNode struct {
	_node
	Value string `parser:"@Boolean"`
}
type StringNode struct {
	_node
	Value string `parser:"StringStart @String StringEnd"`
}
type UnitNode struct {
	_node
	Value string `parser:"'()'"`
}
type ListNode struct {
	_node
	SubType DatatypeUnionNode     `parser:"'list' '<' @@ '>'"`
	Items   []ExpressionUnionNode `parser:"'{' @@* '}'"`
}
type RecordConstructorNode struct {
	_node
	RecordName SymbolUnionNode `parser:"@@ '{'"`
	Fields     []struct {
		_node
		Ident *SymbolNode         `parser:"@@"`
		Value ExpressionUnionNode `parser:"':' @@"`
	} `parser:"@@+'}'"`
	Env Enviroment
}
