\name{idbg.bp}
\Rdversion{1.1}
\alias{idbg.bp}
\alias{idbg.interact}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{idbg.bp}
\description{Prepare a function for debugging and set a breakpoint}
\usage{
idbg.bp(func_name, line_number = NA, condition = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{func_name}{name of function}
  \item{line_number}{line number where to set the breakpoint}
  \item{condition}{ a condition to evaluate for conditional breakpoint}
}
\value{ Logical. TRUE if the breakpoint was set, FALSE if it wasn't}
\author{ Ronen Kimchi}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{idbg.run}} \code{\link{idbg.source}}
}
\examples{

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

## Place a breakpoint in bar
idbg.bp(bar)

## Call foo 
foo(8,9)

## The debugger should stop at bar
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
#   0013 }

## Press 'n' to go to the next statement
# debug: n
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

## Examine the value of variable a
# debug: a
# [1] -1 
# 

## Place conditional breakpoints in bzz. Only one of those should break
# debug: b bzz 2 x >= 0
# debug: b bzz 3 x < 0
# ## print all breakpoints
# debug: b
#   function_name line condition 
# 1           bar    2      TRUE 
# 2           bzz    2    x >= 0 
# 3           bzz    3     x < 0 
# 

## Continue running. We should stop only in line 3 of bzz
# debug: c
# in bzz x = -1 
# function bzz(x) 
#   0001 {
#   0002     cat("in bzz x =", x, "\n")
# =>0003     return(x * 2)
#   0004 }

## Print the value of x and check that x < 0
# debug: x x
# [1] -1 
# 

## Exit the debugger
# debug: q
# 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
