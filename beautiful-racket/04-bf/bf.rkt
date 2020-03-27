#lang br/quicklang

#|
 brainfuck's grammar is (maybe surprisingly) very simple:

 bf-program : (operator | loop)*
 operator   : ">" | "<" | "+" | "-" | "." | ","
 loop       : "[" (operator | loop)* "]"
|#
