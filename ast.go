package grimlang

import (
	"fmt"

	"github.com/alecthomas/participle/v2/lexer"
)

type (
	Node interface {
		Position() lexer.Position
	}
	ExpressionUnionNode interface{ Node }
	BlockUnionNode      interface{ Node }
	ModuleUnionNode     interface{ Node }
	DatatypeUnionNode   interface{ Node }
	ConstantUnionNode   interface{ Node }
	IterableUnionNode   interface{ Node }
	CallableUnionNode   interface{ Node }
	SymbolUnionNode     interface{ Node }
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
	WhileNode struct {
		_node
		Condition ExpressionUnionNode `parser:"'(' 'while' @@"`
		Content   ExpressionUnionNode `parser:"@@ ')'"`
		Env       Enviroment
		Typ       Type
	}
	RangeNode struct {
		_node
		IndexIdent *SymbolNode         `parser:"'(' 'range' '[' @@"`
		Iterable   IterableUnionNode   `parser:"':' @@ ']'"`
		Content    ExpressionUnionNode `parser:"@@ ')'"`
		Env        Enviroment
		Typ        Type
	}
	ForNode struct {
		_node
		IndexIdent *SymbolNode         `parser:"'(' 'for' '[' @@"`
		Start      ExpressionUnionNode `parser:"'=' @@"`
		End        ExpressionUnionNode `parser:"':' @@"`
		Step       ExpressionUnionNode `parser:"':' @@ ']'"`
		Content    ExpressionUnionNode `parser:"@@ ')'"`
		Env        Enviroment
		Typ        Type
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
		Identifier *SymbolNode   `parser:"'(' 'defn' @@"`
		Fns        []*LambdaNode `parser:"@@+ ')'"`
		Types      []*FunctionType
	}
	ConstNode struct {
		_node
		Identifier   *SymbolNode       `parser:"'(' 'defc' @@"`
		Constant     ConstantUnionNode `parser:"@@ ')'"`
		ConstantType Type
	}
	RecordNode struct {
		_node
		Identifier *SymbolNode     `parser:"'(' 'defr' @@"`
		Record     *RecordTypeNode `parser:"@@ ')'"`
		Type       Type
	}
	CallNode struct {
		_node
		Callable   CallableUnionNode     `parser:"'(' @@"`
		Inputs     []ExpressionUnionNode `parser:" @@* ')'"`
		ReturnType Type
		PolyIndex  int
	}
	DefNode struct {
		_node
		Identifier  *SymbolNode         `parser:"'(' 'def' @@"`
		Content     ExpressionUnionNode `parser:"@@ ')'"`
		ContentType Type
	}
	RecordTypeNode struct {
		_node
		Fields []*InputNode `parser:"'(' 'record' @@+ ')'"`
		Env    Enviroment
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
	CompositeNode struct {
		_node
		Datatype string            `parser:"@('list')"`
		SubType  DatatypeUnionNode `parser:"'<' @@ '>'"`
	}
	FnTypeNode struct {
		_node
		Inputs []DatatypeUnionNode `parser:"'[' @@* ']'"`
		Output DatatypeUnionNode   `parser:"'<' @@ '>'"`
	}
	SymbolNode struct {
		_node
		Value string `parser:"@Symbol"`
	}
	SymbolSeqNode struct {
		_node
		Primary *SymbolNode     `parser:"@@"`
		Next    SymbolUnionNode `parser:"'/' @@"`
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
	RecordConstructorNode struct {
		_node
		RecordName SymbolUnionNode `parser:"@@ '{'"`
		Fields     []struct {
			_node
			Ident *SymbolNode         `parser:"@@"`
			Value ExpressionUnionNode `parser:"':' @@"`
		} `parser:"@@+'}'"`
		Env Enviroment
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
	case *SymbolSeqNode:
		return "symbol_seq_node"
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
	case *WhileNode:
		return "while_node"
	case *RangeNode:
		return "range_node"
	case *ForNode:
		return "for_node"
	case *RecordNode:
		return "record_node"
	case *RecordTypeNode:
		return "record_type_node"
	case *RecordConstructorNode:
		return "record_constructor_node"
	default:
		return "invalid_node"
	}
}
