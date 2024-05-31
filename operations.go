package grimlang

import "strings"

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
