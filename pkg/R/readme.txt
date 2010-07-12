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

h - help. Print this message
n - next. Empty line is the same as 'n'
s - step. Step  into a function
o - out. Step out of a function
c - continue. Continue running
q - quit. Exit the debugger
b - print breakpoints 
b <func_name> [FALSE] - set/unset a breakpoint in the first line of function
b <line_number> [FALSE] - set/unset a breakpoint in current function
b <func_name> <line_number> [FALSE] - set/unset a breakpoint in function at line_number
w - where. Print the stack
u - up. Go up the stack
d - down. Go down the stack
l [nlines] - list. Print nlines of source before and after current position
l [nlines_back] [n_lines_forward] - list. Print source around current position
x expr - execute.  Evaluate expr. Any expression that doesn't match the above options will also be executed


Enjoy,
Ronen
