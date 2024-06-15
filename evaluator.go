package grimlang

import "fmt"

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
	case *RecordConstructorNode:
		recEnv := NewEnviroment("rec", env)
		recEnv.CloneSymbols(&node.Env)
		for _, field := range node.Fields {
			sm, err := recEnv.Lookup(field.Ident.Value)
			if err != nil {
				return nil, fmt.Errorf("%s -> %w", field.Position(), err)
			}
			val, err := Eval(recEnv, field.Value)
			if err != nil {
				return nil, err
			}
			sm.Value = val
		}
		return RecordValue{
			Fields: recEnv,
		}, nil
	case *SymbolNode:
		sm, err := env.LookupAll(node.Value)
		if err != nil {
			return nil, fmt.Errorf("%s -> %w", node.Position(), err)
		}
		return sm.Value, nil
	case *SymbolSeqNode:
		sm, err := env.LookupAll(node.Primary.Value)
		if err != nil {
			return nil, fmt.Errorf("%s -> %w", node.Position(), err)
		}
		if IsRecord(sm.Type) {
			recVal, _ := ValueToRecord(sm.Value)
			return Eval(recVal.Fields, node.Next)
		}
		return Eval(sm.Env, node.Next)
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
		switch val := callVal.(type) {
		case BuiltinFuncValue:
			return EvalFunc(env, val, node)
		case FuncValue:
			return EvalFunc(env, val, node)
		case PolyFuncValue:
			return EvalFunc(env, val.Fns[node.PolyIndex], node)
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
	case *WhileNode:
		var resList ListValue
		var genList bool
		if IsListType(node.Typ) {
			genList = true
		}
		for {
			condVal, err := Eval(env, node.Condition)
			if err != nil {
				return nil, err
			}
			cond, err := ValueToBool(condVal)
			if err != nil {
				return nil, err
			}
			if !cond.Value {
				break
			}
			res, err := Eval(env, node.Content)
			if err != nil {
				return nil, err
			}
			if genList {
				resList.Items = append(resList.Items, res)
			}
		}
		if genList {
			return resList, nil
		}
		return UnitValue{}, nil
	case *ForNode:
		startVal, err := Eval(env, node.Start)
		if err != nil {
			return nil, err
		}
		endVal, err := Eval(env, node.End)
		if err != nil {
			return nil, err
		}
		stepVal, err := Eval(env, node.Step)
		if err != nil {
			return nil, err
		}
		forEnv := NewEnviroment("for", env)
		forEnv.CloneSymbols(&node.Env)
		var resList ListValue
		var genList bool
		if IsListType(node.Typ) {
			genList = true
		}
		index, err := forEnv.Lookup(node.IndexIdent.Value)
		if err != nil {
			return nil, err
		}
		index.Value = startVal
	breakList:
		for {
			switch index.Type.(type) {
			default:
				return nil, fmt.Errorf("unexpected index type")
			case *IntegerType:
				indexInt, _ := ValueToInteger(index.Value)
				endVal, _ := ValueToInteger(endVal)
				cond := LessInt(indexInt, endVal)
				condBool, err := ValueToBool(cond)
				if err != nil {
					return nil, err
				}
				if !condBool.Value {
					break breakList
				}
			case *FloatType:
				indexInt, _ := ValueToFloat(index.Value)
				endVal, _ := ValueToFloat(endVal)
				cond := LessFloat(indexInt, endVal)
				condBool, err := ValueToBool(cond)
				if err != nil {
					return nil, err
				}
				if !condBool.Value {
					break breakList
				}
			}
			res, err := Eval(forEnv, node.Content)
			if err != nil {
				return nil, err
			}
			if genList {
				resList.Items = append(resList.Items, res)
			}
			switch index.Type.(type) {
			default:
				return nil, fmt.Errorf("unexpected index type")
			case *IntegerType:
				indexInt, _ := ValueToInteger(index.Value)
				stepInt, _ := ValueToInteger(stepVal)
				res := AddInt(indexInt, stepInt)
				index.Value = res
			case *FloatType:
				indexInt, _ := ValueToFloat(index.Value)
				stepInt, _ := ValueToFloat(stepVal)
				res := AddFloat(indexInt, stepInt)
				index.Value = res
			}
		}
		if genList {
			return resList, nil
		}
		return UnitValue{}, nil
	case *RangeNode:
		rangeEnv := NewEnviroment("range", env)
		rangeEnv.CloneSymbols(&node.Env)
		var resList ListValue
		var genList bool
		if IsListType(node.Typ) {
			genList = true
		}
		index, err := rangeEnv.Lookup(node.IndexIdent.Value)
		if err != nil {
			return nil, err
		}
		val, err := Eval(env, node.Iterable)
		if err != nil {
			return nil, err
		}
		iterable, err := ValueToList(val)
		if err != nil {
			return nil, err
		}
		for _, itm := range iterable.Items {
			index.Value = itm
			res, err := Eval(rangeEnv, node.Content)
			if err != nil {
				return nil, err
			}
			if genList {
				resList.Items = append(resList.Items, res)
			}
		}
		if genList {
			return resList, nil
		}
		return UnitValue{}, nil
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

func EvalFunc(env *Enviroment, val Value, node *CallNode) (Value, error) {
	inputs := []Value{}
	for _, input := range node.Inputs {
		val, err := Eval(env, input)
		if err != nil {
			return nil, err
		}
		inputs = append(inputs, val)
	}
	switch val := val.(type) {
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

}
