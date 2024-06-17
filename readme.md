# Grimlang
Simple, imperative, general purpose programming language with strong type system and lisp-like syntax, written in Golang.

# Examples
Hello world program:
```Clojure
(defn main (fn <unit>) (io/println "Hello, world!"))
```
# Type system
## Primitive types
Int (64 bit):
```
12
54
100
923847293487
```

Float (64 bit):
```Clojure
0.0
12.3212
534979.123142
```

String (unicode):
```Clojure
"Hello, world!"
```

Bool:
```Clojure
true
false
```

## Complex types
List
```Clojure
list<int>{1 2 3 4}
list<string>{"h" "e" "l" "l" "o"}
list<list<int>>{
	list<int>{1 2}
	list<int>{3 4}
	list<int>{5 6}
}
```

## User types
Record
```Clojure
(defr point
	x:float
	y:float
	z:float
)
```

# Language constructions
## Constants
Immutable identifiers with binded values. Values can be simple constant literal or expression. Can shadow identifiers from parent scope. Returns unit type.

Create new constant:
```Lisp
(def pi 3.14)
(def area (* pi (* 10 10)))
```

## Variable
Muatable identifiers with binded value. Values can be simple constant literal or expression. Can shadow identifiers from parent scope. Returns unit type.

Create new variable:
```Lisp
(var lst = list<int>{1 2 3 4})
```

Update variable with same value type:
```Lisp
(set lst = (list/append lst 5))
```

## Block
Create new local scope. Allows execute sequence of instructions. Block return value of last instruction.

Evaluate some expressions and return result:
```Lisp
{
	(def pi 3.14)
	(def r 10.0)
	(def area (* pi (* r r)))
	area
}
```

## Operation
Expression, that can take from 2 to 1024 arguments of the same type then return the result of operation, usually of the same type with arguments.

Arithmetic expressions:
```Clojure
(+ 1 2 3 4 5)
(* 10 10 10 10)
(- 20.0 18.3)
```

Concatenation:
```Clojure
(++ "hello," " " "Alex!")
```

Logic expressions:
```Clojure
(< 2 n 4)
(= 10 12)
(!= 10 12)
```

## Call expression
Calling a function with a specific list of arguments then returns the result value.
```Clojure
(io/println "hello, world")
```

Calling lambda function:
```Clojure
((fn [msg:string] <unit> (io/println msg)) "hello, world")
```

## Condition control flow
Allows create conditional execution of code. All cases should return a value of the same type. Unlimited count of cases. Last argument of condition is else (default).

```Clojure
(def n 10)
(cond
	(case (< 2 n 4) (io/println "2 < n < 4"))
	(case (<= n 2) (io/println "n <= 2"))
	(io/println "4 <= n")
)
```

## Loops
Allows repeat execution of instructions. If return non unit type, can be used as list generator.

While loop:
```Clojure
(var n = 0)
(while (< n 10) (set n = (+ n 1)))
```

Range loop allows iterate over list:
```Clojure
(def lst list<int>{1 2 3 4})
(range [item : lst] (io/println (ints/toString item)))
```

For loop iterates from minimum to maximum value with some step:
```Clojure
(for [i = 0 : 10 : 2] (io/println (ints/toString i)))
```

Loops as list generators:
```Clojure
(def forLst (for [i = 0 : 10 : 2] i))
(var n = 0)
(def whileLst (while (< n 10) {
	(def listItem n)
	(set n = (+ n 1))
	listItem
}))
(def rangeLst (range [item : forLst] (* item item)))
```

## Function defenition
Global function definition. Allows create multiple function instances with the same name and different inputs.

```Clojure
(defn printer
	(fn <unit> (io/println "Hello, World"))
	(fn [msg:string] <unit> (io/println msg))
	(fn [msg:int] <unit> (printer (ints/toString msg)))
)

(defn main (fn <unit> {
	(printer 10)
	(printer "Hello, Alex")
	()
}))
```

## Global constant
Global constant definition. As values can be used only primitive litterals.
```Clojure
(defc pi 3.14)
```

## Record
Global record type declaration.
```Clojure
(defr point
	x:float
	y:float
	z:float
)
```


# Resources
## Participle
A parser library for Go
```
go get github.com/alecthomas/participle/v2@latest
`
