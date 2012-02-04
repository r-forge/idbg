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
  \item{func_name}{name for function. Must be of type character}
  \item{line_number}{line number where to set the breakpoint}
  \item{condition}{ a condition to evaluate for conditional breakpoint}
}
\value{ Logical. TRUE if the breakpoint was set, FALSE if it wasn't}
\author{ Ronen Kimchi}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{debug}} \code{\link{undebug}}
}
\examples{

bar <- function(a)
{
  if (a < 0)
    cat("a < 0\n")
  else
    cat("a >= 0\n")
  return(a)
}
foo <- function(x,y)
{
  tmp <- bar(x - y)
  return(tmp)
}  

\dontrun{idbg.bp("foo")}

# call foo to enter the debugger
# in the debugger type h to see the help
# use n or enter to step over
# use s to step into bar
\dontrun{foo(2,3)}


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
