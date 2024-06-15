package grimlang

import (
	"fmt"
	"os"
)

func InsertBuiltinSymbols(env *Enviroment) {}

func InsertIOSymbols(parent *Enviroment) {
	env := NewEnviroment("io", parent)
	parent.Insert(
		&Symbol{
			Ident:    &SymbolNode{Value: "io"},
			IsImport: true,
			Value:    UnitValue{},
			Env:      env,
		},
	)
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

func InsertFloatsSymbols(parent *Enviroment) {
	env := NewEnviroment("floats", parent)
	parent.Insert(
		&Symbol{
			Ident:    &SymbolNode{Value: "floats"},
			IsImport: true,
			Value:    UnitValue{},
			Env:      env,
		},
	)
	env.Insert(&Symbol{
		Ident: &SymbolNode{Value: "toInt"},
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
		Ident: &SymbolNode{Value: "toString"},
		Type: &FunctionType{
			Inputs: []Type{&FloatType{}},
			Output: &StringType{},
		},
		Value: BuiltinFuncValue{
			Fn: Ftos,
		},
		IsBuiltin: true,
	})
}

func InsertIntsSymbols(parent *Enviroment) {
	env := NewEnviroment("ints", parent)
	parent.Insert(
		&Symbol{
			Ident:    &SymbolNode{Value: "ints"},
			IsImport: true,
			Value:    UnitValue{},
			Env:      env,
		},
	)
	env.Insert(&Symbol{
		Ident: &SymbolNode{Value: "toFloat"},
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
		Ident: &SymbolNode{Value: "toString"},
		Type: &FunctionType{
			Inputs: []Type{&IntegerType{}},
			Output: &StringType{},
		},
		Value: BuiltinFuncValue{
			Fn: Itos,
		},
		IsBuiltin: true,
	})
}

func InsertListsSymbols(parent *Enviroment) {
	env := NewEnviroment("lists", parent)
	parent.Insert(
		&Symbol{
			Ident:    &SymbolNode{Value: "lists"},
			IsImport: true,
			Value:    UnitValue{},
			Env:      env,
		},
	)
	env.Insert(&Symbol{
		Ident: &SymbolNode{Value: "toString"},
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
		Ident: &SymbolNode{Value: "len"},
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
		Ident: &SymbolNode{Value: "append"},
		Type: &FunctionType{
			Inputs: []Type{&ListType{Subtype: &CompType{}}, &CompType{}},
			Output: &ListType{Subtype: &CompType{}},
		},
		Value: BuiltinFuncValue{
			Fn: Lappend,
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
