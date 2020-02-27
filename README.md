# InterpreterProlog

A Prolog Interpreter programmed in Haskell.
By Erik Steffahn and Bjarne Wittlieb

Compile with

```
ghc -Wall Main.hs -odir bin -hidir bin
```

Make sure to have a bin folder in the main directory if you want to locate all compiled files there.

## Repl methods
When running the interactive environment, the following methods are supported:

```:h``` for showing a help window with this list of commands.

```:l <filename>``` to load a Prolog filename into the current system to operate on.

```:s <strategy>``` to switch the strategy which is used to search the dfs tree for solutions.
Available are ```dfs```, ```bfs``` and ```idfs```.

```:q``` to exit the current environment.

```<rule>``` to prove the rule via search of the sld tree.

## Supported Prolog predicates

There are a couple of characteristic prolog commands that are supported. Note that infix predicates and the cut operator are NOT supported!

### Higher order predicates
```call(T1, T2, ..., TN)``` Proves the Term T1, T2 to TN are appended to the predicate in T1. The predicate is true when the goal can be proven.

```\+(T)``` Tries to prove the term T and returns true when it could not be proven.

```findall(Template, Term, Container)``` Tries to prove the term Term. Container is the list of all values that the template can be substituted with.

### Arithmecy
```is(A, X)``` Evaluates the arithmetic Term A and unfies with Variable (or constant) X.

```=.=(A1, A2)``` Evaluates the arithmetic Terms A1, A2 and is true when they are equal.

```=\=(A1, A2)``` Evaluates the arithmetic Terms A1, A2 and is true when they are not equal. Equivalent to ```\+(=.=(A1, A2))```.

```<(A1, A2)```, ```>(A1, A2)```, ```=<(A1, A2)```, ```>=(A1, A2)``` are defined in similar ways.

### Arithmetic terms
Arithmetic terms are terms that suffice the following rules:

```c``` is an arithmetic term, where c is an Integer.

```+(A1, A2)``` is an arithmetic term, where A1 and A2 are both arithmetic terms.

```-(A1, A2)``` is an arithmetic term, where A1 and A2 are both arithmetic terms.

```*(A1, A2)``` is an arithmetic term, where A1 and A2 are both arithmetic terms.

```div(A1, A2)``` is an arithmetic term, where A1 and A2 are both arithmetic terms.

```mod(A1, A2)``` is an arithmetic term, where A1 and A2 are both arithmetic terms.
