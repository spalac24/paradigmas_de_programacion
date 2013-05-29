How to use the oracle debugger (Prolog frontend):

- Generate the compiler for Curry into Prolog with strict evaluation:
  "make" in this directory (you might adapt the options at the beginning
  of Curry2StrictPl.curry)

- To debug a Curry program stored in MOD.curry:

  * The file MOD.steps must contain the list of innermost computation
    steps (usually produced in the preceding transformation step)

  * Execute "./debug MOD f" if MOD.f is the 0-ary main function
    (or simply "./debug MOD" if MOD.main is the main function):
    This generates a Prolog file MODs.pl containing the innermost
    execution code and starts the debugger


--------

How to use the oracle debugger with the Haskell frontend

assuming your program is called 'Example.curry' and the main function
is called 'main'

- if there is no binary called 'stricths', then use
    $ make stricths
  to compile it via kics.

- transform Example.curry to ExampleOracle.fcy:
    $ ./transform Example main

- generate oracle file 'Example.steps'
    $ ./oracle Example main

- generate strict program (omit --hs for Curry programs)
    $ ./stricths --hs Example

-  start ghci with strict program
    $ ghci ExampleStrict.hs

- debug main expression
    ExampleStrict> main



--------

Files:

Curry2StrictPl.curry: The compiler Curry -> Prolog

ReadFlat.curry: The front-end for the compiler

Prolog.curry: A representation for Prolog programs used by the compiler

cdbenv.pl: The run-time system of the debugger

ex1.curry / ex1.steps: A simple example

-------
more files:

Ex*.curry		example programs

StrictSteps.hs		run-time system of the Haskell based debugger

stricths.curry		source code for the second transformation 'stricths'

Oracle.curry		run-time system for oracle generation

Makefile		just what you expected

transform		batch file that does the first transformation

Transform.curry		the first transformation

oracle			batch file that generates an oracle 

Oracle.curry		run-time system for oracle generation


------
also lingering around:

Parser.curry
PrettyStrict.curry
SourceProgGUI.curry
Wrapper.curry
cdb
cdb.pl
etaexpand.curry
higherOrder.curry
suppressmsgs.pl
test.curry

