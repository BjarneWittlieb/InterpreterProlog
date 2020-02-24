# InterpreterProlog

A Prolog Interpreter programmed in Haskell.
By Erik Steffahn and Bjarne Wittlieb

Compile with

```
ghc -Wall Main.hs -odir bin -hidir bin
```

Make sure to have a bin folder in the main directory if you want to locate all compiled files there.

## Bash methods
When running the interactive Environment, the following methods are supported:

```:h``` for showing a Help window with this list of commands.

```:l <filename>``` to load a Prolog filename into the current system to operate on.

```:s <strategey>``` to switch the strategy which is used to search the dfs tree for solutions.
Available are ```dfs```, ```bfs``` and ```idfs```.

```:q``` to exit the current environment.

```<rule>``` to proof the rule via search of the sld tree.

## Supported Prolog predicates

There are a couple of charactaristic prolog commands that are supported.

### Higher order predicates
```call(T1, T2, ..., TN)``` Prooves the Terms within in the predicate T1 to TN. The predicate is true when the foal could be prooven.

```\+(T)``` Tries to proof the term T and returns true when it could not be proofed.

```findall(X, Term, Container)``` Tries to proof the term Term. Container is the list of all Values that the variable X can be substituted with.

### Arithmecy
```is(A, X)``` Evaluates the arethmetic Term A and unfies with Variable (or constant) X.

```=.=(A1, A2)``` Evaluates the arethmetic Terms A1, A2 and is true when they are equal.

```=\=(A1, A2)``` Evaluates the arethmetic Terms A1, A2 and is true when they are not equal. Equivalent to ```\+(=.=(A1, A2))```.

```<(A1, A2)```, ```>(A1, A2)```, ```=<(A1, A2)```, ```>=(A1, A2)``` are defined in similar ways.

### Arethmetic terms
Arethmetic terms are terms that suffice the following rules:

```c``` is an arethmetic term, where c is an Integer.

```+(A1, A2)``` is an arethmetic term, where A1 and A2 are both arethmetic terms.

```-(A1, A2)``` is an arethmetic term, where A1 and A2 are both arethmetic terms.

```*(A1, A2)``` is an arethmetic term, where A1 and A2 are both arethmetic terms.

```div(A1, A2)``` is an arethmetic term, where A1 and A2 are both arethmetic terms.

```mod(A1, A2)``` is an arethmetic term, where A1 and A2 are both arethmetic terms.
