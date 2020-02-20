# InterpreterProlog

A Prolog Interpreter programmed in Haskell.
By Erik Steffahn and Bjarne Wittlieb

Compile with

```
ghc -Wall Main.hs -odir bin -hidir bin
```

## Bash methods
When running the interactive Environment, the following methods are supported:

```:h``` for showing a Help window with this list of commands.

```:l <filename>``` to load a Prolog filename into the current system to operate on.

```:s <strategey>``` to switch the strategy which is used to search the dfs tree for solutions.
Available are ```dfs```, ```bfs``` and ```idfs```.

```:q``` to exit the current environment.

```<rule>``` to proof the rule via dfs search.

## Supported Prolog commands

The Engine does not support a full Prolog system.
Only one programm can be loaded at a time, there is not (finite) negation and also the ! (cut-) operator is not implemented.
