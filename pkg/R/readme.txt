How to use the debugger
=======================
1. source the debugger
   source("idbg.R")
2. Set a breakpoint in the function to debug
   idbg.bp("foo")
3. Call the function
   foo(1,2,3)
4. The debugger will stop at the first line of foo
5. Use the following commands

h - help. print this message
n - next. Empty line is the same as 'n'
s - step into
o - step out
c - continue
q - quit
b - print breakpoints
b FALSE - clear all breakpoints
b <func_name> [FALSE] - set/unset a breakpoint in first line of function
b <line_number> [FALSE] - set/unset a breakpoint in current function
b <func_name> <line_number> [FALSE] - set/unset a breakpoint in function at at line_number
w - print the stack
u - go up the stack
d - go down the stack
l [nlines] - print nlines of source before and after current position
l [nlines_back] [nlines_forward] - print source around current position
x expr - execute expression. Any expression that doesn't match the above options will also be executed


Enjoy,
Ronen
