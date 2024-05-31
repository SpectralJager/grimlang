package grimlang

import (
	"fmt"
	"strings"
)

type EnviromentKind int

const (
	LocalEK EnviromentKind = iota
	BuiltinEK
	GlobalEK
)

type Enviroment struct {
	Name         string
	Symbols      map[string]*Symbol
	Parent       *Enviroment
	Kind         EnviromentKind
	IsCollecting bool
}

func NewEnviroment(name string, parent *Enviroment) *Enviroment {
	return &Enviroment{
		Name:    name,
		Parent:  parent,
		Symbols: map[string]*Symbol{},
	}
}

func (env *Enviroment) CloneSymbols(other *Enviroment) error {
	for _, symb := range other.Symbols {
		newSymb := *symb
		env.Insert(&newSymb)
	}
	return nil
}

func (env *Enviroment) ClosestEnv(kind EnviromentKind) *Enviroment {
	if env.Kind == kind {
		return env
	}
	if env.Parent != nil {
		return env.Parent.ClosestEnv(kind)
	}
	return nil
}

func (env *Enviroment) Insert(sm *Symbol) error {
	_, ok := env.Symbols[sm.Ident.Value]
	if ok {
		return fmt.Errorf("symbol '%s' already exists in enviroment %s", sm.Ident.Value, env.Name)
	}
	env.Symbols[sm.Ident.Value] = sm
	return nil
}

func (env *Enviroment) Lookup(ident string) (*Symbol, error) {
	sm, ok := env.Symbols[ident]
	if !ok {
		return nil, fmt.Errorf("symbol '%s' not found in enviroment %s", ident, env.Name)
	}
	return sm, nil
}

func (env *Enviroment) LookupAll(ident string) (*Symbol, error) {
	sm, err := env.Lookup(ident)
	if err == nil {
		return sm, nil
	}
	if env.Parent == nil {
		return nil, err
	}
	return env.Parent.LookupAll(ident)
}

func (env *Enviroment) Inspect() string {
	symbols := []string{}
	for _, sm := range env.Symbols {
		symbols = append(symbols, InspectSymbol(sm))
	}
	return fmt.Sprintf("=== Env:%s |>\n\t%s\n", env.Name, strings.Join(symbols, "\n\t"))
}
