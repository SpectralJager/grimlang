package grimlang

import "fmt"

type (
	Symbol struct {
		Ident     *SymbolNode
		Type      Type
		Value     Value
		IsMutable bool
		IsBuiltin bool
		IsImport  bool
		Env       *Enviroment
	}
)

func InspectSymbol(sm *Symbol) string {
	switch {
	case sm.IsMutable:
		return fmt.Sprintf("%s -> mut %s", sm.Ident.Value, InspectType(sm.Type))
	case sm.IsBuiltin:
		return fmt.Sprintf("%s -> builtin %s", sm.Ident.Value, InspectType(sm.Type))
	case sm.IsImport:
		return fmt.Sprintf("%s -> import", sm.Ident.Value)
	default:
		return fmt.Sprintf("%s -> %s", sm.Ident.Value, InspectType(sm.Type))
	}
}
