Interpreter - prolog
=======================

An interpreter is a computer program that directly executes, i.e. performs, instructions written in a programming or scripting language, without previously compiling them into a machine language program. An interpreter generally uses one of the following strategies for program execution:

*parse the source code and perform its behavior directly
*translate source code into some efficient intermediate representation and immediately execute this
*explicitly execute stored precompiled code made by a compiler which is part of the interpreter system

###DCG - Definite Clause Grammar [Right Recursive]

```
P  ::= K.
K  ::= begin D; C end
D  ::= const I = N D' | var I D' %| I := N D'
D' ::= ε | ; D D'
C  ::= I := E C' | if B then C else C endif C' | while B do C endwhile C' | K C'
C' ::= ε | ; C C'
B  ::= true | false | E = E | not B
E  ::= I := E E' |I E' | N E'
E' ::= ε | + E E' | - E E' | * E E' | / E E'
I  ::= x | y | z | u | v
N  ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
```

##Install

consult('/Users/abc/documets/interpreter.pl').
main_1(2, 3, A).

This basic interpreter considers two variables x and y in the input code and generates the parse tree.

A valid code block may look like this,

```
[begin, var, x,;, var, y,;, var ,z,;, z,:=,1,;, w,:=,x,;, while ,not ,w, =, 0 ,do, z, :=,z,*,y,;, w,:=,w,-,1, endwhile, end,.]
```


##Project Contributors

* Dinesh Appavoo ([@DineshAppavoo](https://twitter.com/DineshAppavoo))
