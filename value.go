package grimlang

import (
	"fmt"
	"strings"
)

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
	PolyFuncValue struct {
		_value
		Fns []FuncValue
	}
	ListValue struct {
		_value
		Items []Value
	}
	RecordValue struct {
		_value
		Fields *Enviroment
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
	case RecordValue:
		fields := []string{}
		for _, field := range value.Fields.Symbols {
			fields = append(fields, InspectSymbol(field))
		}
		return fmt.Sprintf("#{%s}", strings.Join(fields, " "))
	case BuiltinFuncValue:
		return "builtin_fn"
	case FuncValue:
		return "fn"
	case PolyFuncValue:
		return "fns"
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

func ValueToRecord(value Value) (RecordValue, error) {
	val, ok := value.(RecordValue)
	if !ok {
		return RecordValue{}, fmt.Errorf("can't convert value to record")
	}
	return val, nil
}

func ValueToList(value Value) (ListValue, error) {
	val, ok := value.(ListValue)
	if !ok {
		return ListValue{}, fmt.Errorf("can't convert value to list")
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
