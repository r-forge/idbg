\name{idbg-package}
\Rdversion{1.1}
\alias{idbg-package}
\alias{idbg}
\docType{package}
\title{idbg}
\description{
An interactive debugger for R
}
\details{
\tabular{ll}{
Package: \tab idbg\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-01-16\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}

use idbg.bp to set a breakpoint in the debugged function.\cr
Run your code and the debugger will stop at the debugged function\cr\cr

The following debugger options are available:\cr
h - help. print this message\cr
n - next. Empty line is the same as 'n'\cr
s - step into\cr
o - step out\cr
c - continue\cr
q - quit\cr
b - print breakpoints\cr
b FALSE - clear all breakpoints\cr
b <func_name> [FALSE] - set/unset a breakpoint in first line of function\cr
b <line_number> [FALSE] - set/unset a breakpoint in current function\cr
b <func_name> <line_number> [FALSE] - set/unset a breakpoint in function at at line_number\cr
w - print the stack\cr
u - go up the stack\cr
d - go down the stack\cr
l [nlines] - print nlines of source before and after current position\cr
l [nlines_back] [nlines_forward] - print source around current position\cr
x expr - execute expression. Any expression that doesn't match the above options will also be executed\cr
}
\author{
Ronen Kimchi

Maintainer: mitzpaz@gmail.com
}
\keyword{ package }
\seealso{
\code{\link{idbg.bp}}
}
