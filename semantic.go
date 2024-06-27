package grimlang

import "fmt"

func TypeCheck(env *Enviroment, node Node) (Type, error) {
	switch node := node.(type) {
	case *ModuleNode:
	case *FnNode:
		if !env.IsCollecting {
			for _, fn := range node.Fns {
				_, err := TypeCheck(&fn.Env, fn.Content)
				if err != nil {
					return nil, err
				}
			}
			return types["unit"], nil
		}
		fnsValue := []FuncValue{}
		for _, fn := range node.Fns {
			inputs := []Type{}
			fnEnv := NewEnviroment("fn", env)
			for _, input := range fn.Inputs {
				inputTyp, err := TypeCheck(fnEnv, input)
				if err != nil {
					return nil, err
				}
				inputs = append(inputs, inputTyp)
			}
			output, err := TypeCheck(fnEnv, fn.Output)
			if err != nil {
				return nil, err
			}
			fn.Env = *fnEnv
			fn.Type = &FunctionType{
				Inputs: inputs,
				Output: output,
			}
			node.Types = append(node.Types, fn.Type)
			fnsValue = append(fnsValue, FuncValue{
				Inputs:  fn.Inputs,
				Env:     *fnEnv,
				Content: fn.Content,
			})
		}
		err := env.Insert(
			&Symbol{
				Ident: node.Identifier,
				Type:  &PolyFuncType{Fns: node.Types},
				Value: PolyFuncValue{
					Fns: fnsValue,
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
	case *RecordNode:
		if !env.IsCollecting {
			typ, err := TypeCheck(env, node.Record)
			if err != nil {
				return nil, err
			}
			sm, err := env.Lookup(node.Identifier.Value)
			if err != nil {
				return nil, fmt.Errorf("%s -> %w", node.Position(), err)
			}
			smTyp := sm.Type.(*RecordType)
			recTyp := typ.(*RecordType)
			smTyp.Fields = recTyp.Fields
			smTyp.Enviroment = recTyp.Enviroment
			return types["unit"], nil
		}
		err := env.Insert(
			&Symbol{
				Ident: node.Identifier,
				Type:  &RecordType{},
				Value: UnitValue{},
			},
		)
		if err != nil {
			return nil, fmt.Errorf("%s -> %w", node.Position(), err)
		}
		return types["unit"], nil
	case *RecordTypeNode:
		recEnv := NewEnviroment("rec", env)
		fields := []Type{}
		for _, field := range node.Fields {
			fieldType, err := TypeCheck(recEnv, field)
			if err != nil {
				return nil, err
			}
			fields = append(fields, fieldType)
		}
		return &RecordType{
			Fields:     fields,
			Enviroment: *recEnv,
		}, nil
	case *RecordConstructorNode:
		typ, err := TypeCheck(env, node.RecordName)
		if err != nil {
			return nil, err
		}
		if !IsRecord(typ) {
			return nil, fmt.Errorf("%s -> expect record type", node.Position())
		}
		recTyp := typ.(*RecordType)
		recEnv := NewEnviroment("rec", env)
		recEnv.CloneSymbols(&recTyp.Enviroment)
		if len(node.Fields) != len(recTyp.Enviroment.Symbols) {
			return nil, fmt.Errorf("%s -> record constructor expect initialization of all fields", node.Position())
		}
		for _, field := range node.Fields {
			sm, err := recEnv.Lookup(field.Ident.Value)
			if err != nil {
				return nil, fmt.Errorf("%s -> %w", field.Position(), err)
			}
			valTyp, err := TypeCheck(recEnv, field.Value)
			if err != nil {
				return nil, err
			}
			if !CompareTypes(sm.Type, valTyp) {
				return nil, fmt.Errorf("%s -> field expect %s, got %s", field.Position(), InspectType(sm.Type), InspectType(valTyp))
			}
		}
		node.Env = *recEnv
		return typ, nil
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
	case *CompositeNode:
		switch node.Datatype {
		case "list":
			subTyp, err := TypeCheck(env, node.SubType)
			if err != nil {
				return nil, err
			}
			return &ListType{Subtype: subTyp}, nil
		default:
			return nil, fmt.Errorf("%s -> unexpected composite type '%s'", node.Position(), node.Datatype)
		}
	case *FnTypeNode:
		inputs := []Type{}
		for _, in := range node.Inputs {
			inTyp, err := TypeCheck(env, in)
			if err != nil {
				return nil, err
			}
			inputs = append(inputs, inTyp)
		}
		output, err := TypeCheck(env, node.Output)
		if err != nil {
			return nil, err
		}
		return &FunctionType{
			Inputs: inputs,
			Output: output,
		}, nil
	case *SymbolNode:
		sm, err := env.LookupAll(node.Value)
		if err != nil {
			return nil, fmt.Errorf("%s -> %w", node.Position(), err)
		}
		return sm.Type, nil
	case *SymbolSeqNode:
		sm, err := env.LookupAll(node.Primary.Value)
		if err != nil {
			return nil, fmt.Errorf("%s -> %w", node.Position(), err)
		}
		if IsRecord(sm.Type) {
			recTyp := sm.Type.(*RecordType)
			recEnv := NewEnviroment(sm.Ident.Value, env)
			recEnv.CloneSymbols(&recTyp.Enviroment)
			return TypeCheck(recEnv, node.Next)
		}
		if sm.Env != nil {
			return TypeCheck(sm.Env, node.Next)
		}
		return nil, fmt.Errorf("%s -> can't get symbol from %s", node.Position(), InspectSymbol(sm))
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
	case *CallNode:
		typ, err := TypeCheck(env, node.Callable)
		if err != nil {
			return nil, err
		}
		switch fnTyp := typ.(type) {
		case *FunctionType:
			return CallFuncCheck(env, fnTyp, node)
		case *PolyFuncType:
			inputs := []Type{}
			for _, input := range node.Inputs {
				typ, err := TypeCheck(env, input)
				if err != nil {
					return nil, err
				}
				inputs = append(inputs, typ)
			}
			var typ *FunctionType
			for index, fnTyp := range fnTyp.Fns {
				if len(inputs) != len(fnTyp.Inputs) {
					continue
				}
				finded := true
				for i, input := range fnTyp.Inputs {
					if !CompareTypes(input, inputs[i]) {
						finded = false
						break
					}
				}
				if finded {
					typ = fnTyp
					node.PolyIndex = index
					break
				}
			}
			if typ == nil {
				return nil, fmt.Errorf("%s -> can't match function with such inputs", node.Position())
			}
			return CallFuncCheck(env, typ, node)
		default:
			return nil, fmt.Errorf("%s -> callable should be function, got '%s'", node.Position(), InspectType(typ))
		}
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
	case *WhileNode:
		condType, err := TypeCheck(env, node.Condition)
		if err != nil {
			return nil, err
		}
		if !IsBooleanType(condType) {
			return nil, fmt.Errorf("%s -> while condition should be bool type", node.Position())
		}
		contTyp, err := TypeCheck(env, node.Content)
		if err != nil {
			return nil, err
		}
		if IsUnitType(contTyp) {
			return contTyp, nil
		}
		node.Typ = &ListType{Subtype: contTyp}
		return node.Typ, nil
	case *ForNode:
		startTyp, err := TypeCheck(env, node.Start)
		if err != nil {
			return nil, err
		}
		endTyp, err := TypeCheck(env, node.End)
		if err != nil {
			return nil, err
		}
		stepTyp, err := TypeCheck(env, node.End)
		if err != nil {
			return nil, err
		}
		if !IsNumberType(startTyp) || !IsNumberType(endTyp) || !IsNumberType(stepTyp) {
			return nil, fmt.Errorf("%s -> start, end and step should be numbers", node.Position())
		}
		if !CompareTypes(startTyp, endTyp, stepTyp) {
			return nil, fmt.Errorf("%s -> start, end and step should be same type", node.Position())
		}
		forEnv := NewEnviroment("for", env)
		forEnv.Insert(
			&Symbol{Ident: node.IndexIdent, Type: startTyp},
		)
		contTyp, err := TypeCheck(forEnv, node.Content)
		if err != nil {
			return nil, err
		}
		node.Env = *forEnv
		// fmt.Println(forEnv.Inspect())
		if IsUnitType(contTyp) {
			return contTyp, nil
		}
		node.Typ = &ListType{Subtype: contTyp}
		return node.Typ, nil
	case *RangeNode:
		iterableTyp, err := TypeCheck(env, node.Iterable)
		if err != nil {
			return nil, err
		}
		if !IsIterableType(iterableTyp) {
			return nil, fmt.Errorf("%s -> should be iterable type", node.Iterable.Position())
		}
		var itemTyp Type
		switch iterableTyp := iterableTyp.(type) {
		case *ListType:
			itemTyp = iterableTyp.Subtype
		default:
			return nil, fmt.Errorf("%s -> unexpected iterable type", node.Iterable.Position())
		}
		rangeEnv := NewEnviroment("range", env)
		rangeEnv.Insert(
			&Symbol{Ident: node.IndexIdent, Type: itemTyp},
		)
		contTyp, err := TypeCheck(rangeEnv, node.Content)
		if err != nil {
			return nil, err
		}
		node.Env = *rangeEnv
		// fmt.Println(rangeEnv.Inspect())
		if IsUnitType(contTyp) {
			return contTyp, nil
		}
		node.Typ = &ListType{Subtype: contTyp}
		return node.Typ, nil
	case *OperationNode:
	default:
		return nil, fmt.Errorf("%s -> can't check unexpected node '%s'", node.Position(), InspectNode(node))
	}
}

func TypeCheckOperatorOperands(env *Enviroment, node *OperationNode) (Type, error) {
}

func CallFuncCheck(env *Enviroment, fnTyp *FunctionType, node *CallNode) (Type, error) {
	if fnTyp.Output == nil {
		return nil, fmt.Errorf("%s -> function should contain return type", node.Position())
	}
	node.ReturnType = fnTyp.Output
	if len(node.Inputs) != len(fnTyp.Inputs) {
		return nil, fmt.Errorf("%s -> number of inputs mismatched", node.Position())
	}
	var comp Type
	for index, input := range node.Inputs {
		inTyp, err := TypeCheck(env, input)
		if err != nil {
			return nil, err
		}
		if ContainsComp(fnTyp.Inputs[index]) {
			res, ok := CompTypeCheck(fnTyp.Inputs[index], inTyp)
			if !ok {
				return nil, fmt.Errorf("%s -> #%d input can't check comp", input.Position(), index)
			}
			if comp == nil {
				comp = res
			} else {
				if !CompareTypes(comp, res) {
					return nil, fmt.Errorf("%s -> #%d input comp type mismatched", input.Position(), index)
				}
			}
		} else if !CompareTypes(fnTyp.Inputs[index], inTyp) {
			return nil, fmt.Errorf("%s -> #%d input type mismatched", input.Position(), index)
		}
	}
	var ok bool
	if ContainsComp(fnTyp.Output) {
		if comp == nil {
			return nil, fmt.Errorf("%s -> can't replace comp output without comp input", node.Position())
		}
		node.ReturnType, ok = ReplaceComp(fnTyp.Output, comp)
		if !ok {
			return nil, fmt.Errorf("%s -> can't replace comp output", node.Position())
		}
	}
	return node.ReturnType, nil
}
