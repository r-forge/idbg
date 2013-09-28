\name{idbg-package}
\Rdversion{1.1}
\alias{idbg-package}
\alias{idbg}
\docType{package}
\title{idbg}
\description{
An interactive R debugger
}
\details{
\tabular{ll}{
Package: \tab idbg\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2012-01-16\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}

Main features supported:\cr
\itemize{
\item Debugger control - next, step into, step out commands
\item Set/unset/list breakpoints.
\item Breakpoints can be set at a specific line.
\item Conditional breakpoints support.
\item Code is displayed with marking of current command and line number.
\item Control statements (if, for, while etc.) are broken to their components stopping at each part (e.g. condition-then-else).
\item The Last line of a block (or function) is a debugger stopping point
\item Code listing within the debugger
\item Call stack traversal with up/down commands
}


There are two ways to start the debugger:\cr
\itemize{
\item idbg.run - will take a function call and start the debugger at the first line.
\item idbg.bp - place a breakpoint at a specific function (and optionally a line number). A call to the function will start the debugger.
}

The following debugger options are available:\cr
h - help. print this message\cr
n - next. Empty line is the same as 'n'\cr
s - step into\cr
o - step out\cr
c - continue\cr
q - quit\cr
b - print breakpoints\cr
b FALSE - clear all breakpoints\cr
b <func_name> [FALSE] - set/unset a breakpoint at the first line of function\cr
b <line_number> [FALSE] - set/unset a breakpoint at the current function\cr
b <func_name> <line_number> [FALSE] - set/unset a breakpoint in func_name at line_number\cr
w - print the stack\cr
u - go up the stack\cr
d - go down the stack\cr
l [func_name] [nlines] - print nlines of source before and after current position\cr
l [func_name] [nlines_back] [nlines_forward] - print source around current position\cr
f string [func_name] - find string in function source\cr
x expr - execute expression.\cr
expr - Any expression that doesn't match the above options will also be executed\cr



}
\author{
Ronen Kimchi

Maintainer: mitzpaz@gmail.com
}
\keyword{ package }
\seealso{
\code{\link{idbg.run}} \code{\link{idbg.bp}} \code{\link{idbg.source}}
}

\examples{

library(idbg)

bar <- function(a)
{
  bzz <- function(x)
  {
    cat("in bzz x =",x,"\n")
    return(x * 2)
  }
  
  if (a < 0)
    result <- bzz(-1)
  else
    result <- bzz(1)
  return(result)
}
foo <- function(x,y)
{
  tmp <- bar(x - y)
  return(tmp)
}  

idbg.run(foo(2,3))

## The debugger starts with displaying foo and the current location 
# function foo(x, y) 
#   0001 {
# =>0002     tmp <- bar(x - y)
#   0003     return(tmp)
#   0004 }

## Press 's' and enter at the debugger prompt to step into bar
# debug: s
# function bar(a) 
#   0001 {
# =>0002     bzz <- function(x) {
#   0003         cat("in bzz x =", x, "\n")
#   0004         return(x * 2)
#   0005     }
#   0006     if (a < 0) {
#   0007         result <- bzz(-1)
#   0008     }
#   0009     else {
#   0010         result <- bzz(1)
#   0011     }
#   0012     return(result)

## Examine the value of variable a
# debug: a
# [1] -1 
# 

## Place a breakpoint at row 6 and continue
# debug: b 6
# debug: c
# function bar(a) 
#   0001 {
#   0002     bzz <- function(x) {
#   0003         cat("in bzz x =", x, "\n")
#   0004         return(x * 2)
#   0005     }
# =>0006     if (a < 0) {
#   0007         result <- bzz(-1)
#   0008     }
#   0009     else {
#   0010         result <- bzz(1)
#   0011     }
#   0012     return(result)
#   0013 }

## Place a breakpoint in bzz and continue. This can only be done after bzz is defined
# debug: b bzz
# debug: c
# function bzz(x) 
#   0001 {
# =>0002     cat("in bzz x =", x, "\n")
#   0003     return(x * 2)
#   0004 }

## Press 'w' to print the call stack
# debug: w
#   1 foo  =>0002     tmp <- bar(x - y)
#   2 bar  =>0007         result <- bzz(-1)
# * 3 bzz  =>0002     cat("in bzz x =", x, "\n")

## Press 'u' to go up in the stack to bar
# debug: u

## Press 'l' to list the code
# debug: l
# function bar(a) 
#   0001 {
#   0002     bzz <- function(x) {
#   0003         cat("in bzz x =", x, "\n")
#   0004         return(x * 2)
#   0005     }
#   0006     if (a < 0) {
# =>0007         result <- bzz(-1)
#   0008     }
#   0009     else {
#   0010         result <- bzz(1)
#   0011     }
#   0012     return(result)
#   0013 }

## place a conditional breakpoint at row 12 of bar continue
# debug: b bar 12 result < 0

## print all breakpoints
# debug: b
#   function_name line  condition 
# 1           bar    6       TRUE 
# 2           bar   12 result < 0 
# 3           bzz    2       TRUE 
# 

## Continue running. We should stop at the conditional breakpoint
# debug: c
# in bzz x = -1 
#   0002     bzz <- function(x) {
#   0003         cat("in bzz x =", x, "\n")
#   0004         return(x * 2)
#   0005     }
#   0006     if (a < 0) {
#   0007         result <- bzz(-1)
#   0008     }
#   0009     else {
#   0010         result <- bzz(1)
#   0011     }
# =>0012     return(result)
#   0013 }
# debug: result
# [1] -2 
# 

## List the source code of foo
# debug: l foo
# function foo(x, y) 
#   0001 {
#   0002     tmp <- bar(x - y)
#   0003     return(tmp)
#   0004 }

## Press 'o' to step out of bar
# debug: o
# function foo(x, y) 
#   0001 {
#   0002     tmp <- bar(x - y)
# =>0003     return(tmp)
#   0004 }

## Print the values of x, y and x-y. Must use the 'x' command to avoid confusion with the x variable
# debug: x x
# [1] 2 
# 
# debug: x y
# [1] 3 
# 
# debug: x (x - y)
# [1] -1 
# 

## find the string cat in bar
# debug: f cat bar
# 0003         cat("in bzz x =", x, "\n") 

## Print the debugger help message
# debug: h
# h - help. print this message
# n - next. Empty line is the same as 'n'
# s - step into
# o - step out
# c - continue
# q - quit
# b - print breakpoints
# b FALSE - clear all breakpoints
# b <func_name> [FALSE] - set/unset a breakpoint at the first line of func_name
# b <line_number> [FALSE] - set/unset a breakpoint at the current function
# b <func_name> <line_number> [FALSE] - set/unset a breakpoint in func_name at line_number
# w - print the stack
# u - go up the stack
# d - go down the stack
# l [func_name] [nlines] - print nlines of source before and after current position
# l [func_name] [nlines_back] [nlines_forward] - print source around current position
# f string [func_name] - find string in function source
# x expr - execute expression.
# expr - Any expression that doesn't match the above options will also be executed

## Exit the debugger
# debug: q
# 

}
