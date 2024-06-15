package grimlang

import (
	"fmt"
	"strings"
)

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
	PolyFuncType struct {
		_type
		Fns []*FunctionType
	}
	ListType struct {
		_type
		Subtype Type
	}
	RecordType struct {
		_type
		Fields     []Type
		Enviroment Enviroment
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
	case *PolyFuncType:
		fns := []string{}
		for _, fn := range typ.Fns {
			fns = append(fns, InspectType(fn))
		}
		if len(fns) == 1 {
			return fns[0]
		}
		return fmt.Sprintf("{%s}", strings.Join(fns, " "))
	case *RecordType:
		fields := []string{}
		for _, field := range typ.Fields {
			fields = append(fields, InspectType(field))
		}
		return fmt.Sprintf("#{%s}", strings.Join(fields, " "))
	case *ListType:
		return fmt.Sprintf("list<%s>", InspectType(typ.Subtype))
	default:
		return "illigal"
	}
}

func CompareTypes(types ...Type) bool {
	if len(types) < 2 {
		return false
	}
	cmp := InspectType(types[0])
	for _, tp := range types[1:] {
		if cmp != InspectType(tp) {
			return false
		}
	}
	return true
}

func IsNumberType(a Type) bool {
	return IsIntegerType(a) || IsFloatType(a)
}

func IsIterableType(a Type) bool {
	return IsListType(a)
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

func IsRecord(a Type) bool {
	if _, ok := a.(*RecordType); ok {
		return true
	}
	return false
}

// Comp type operations

func ContainsComp(a Type) bool {
	switch a := a.(type) {
	default:
		return false
	case *CompType:
		return true
	case *ListType:
		return ContainsComp(a.Subtype)
	case *FunctionType:
		for _, input := range a.Inputs {
			if ContainsComp(input) {
				return true
			}
		}
		return false
	}
}

func CompTypeCheck(src Type, trg Type) (Type, bool) {
	switch src := src.(type) {
	default:
		return src, false
	case *CompType:
		return trg, true
	case *ListType:
		if IsListType(trg) {
			return CompTypeCheck(src.Subtype, trg.(*ListType).Subtype)
		}
		return src, false
	}
}

func ReplaceComp(src Type, trg Type) (Type, bool) {
	switch src := src.(type) {
	default:
		return src, false
	case *CompType:
		return trg, true
	case *ListType:
		lst := &ListType{}
		if _, ok := ReplaceComp(src.Subtype, trg); ok {
			lst.Subtype = trg
			return lst, true
		}
		return src, false
	}
}
