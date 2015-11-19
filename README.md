DuckTest: Static Duck Type Checking for Python
------------

DuckTest is a program that attempts to type-check Python source files by using
type inference on Python structural types.

It is named after the famous (or maybe infamous) 'duck test'. That is,
"if it looks, walks and quacks like a duck, then it must be a duck."
DuckTest uses type inference on aspects of python programs (given that
the programs are sanely typed) to look for inconsistencies in typing.
In short, DuckTest does exactly what its name says, it checks to see
if your source code passes the Duck Test.

For best results, DuckTest should be run on Python programs that intend
to adhere to *Type Erasure Semantics*. This means that the program
should not dynamically introspect the types at runtime and branch
based on them. This means no branching on the `__class__` attribute,
or any other type-level attribute. Failure to adhere to this rule
will result in some false positives.
