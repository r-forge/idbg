

###############################################################################
#
# idbg.init - Initialize debugger data structure  
#
# parameters: force - force initialization even if debugger data already exists
#           
# the debugger data is stored in an environment called idbg.data under the environment
# of function idbg. Use function idbg() to get that environment
#
###############################################################################
idbg.init <- function(force=FALSE)
{
  if (!exists("idbg.data") || force) 
  {
    idbg.data <- new.env() 
    idbg.data[["break_frame"]] <- -1
    idbg.data[["call_stack"]] <- list()
    idbg.data[["debug_frame"]] <- -1
    idbg.data[["ifunc_names"]] <- c()
  }
  idbg <<- function() {
    return(idbg.data)
  }
  idbg.call_stack_top <<- function(frame_id, func_name, pos)
  {
    idbg.data$call_stack[[frame_id]] <- list(func_name, pos)
    length(idbg.data$call_stack) <- frame_id
  }
  idbg.add_ifunc <<- function(fname)
  {
    if (! (fname %in% idbg.data$ifunc_names))
      idbg.data$ifunc_names <- c(idbg.data$ifunc_names, fname)
  }
}
###############################################################################
idbg.bp <- function( func_name, line_number=NA, condition=TRUE)
{
  f <- ifunc(func_name)

  breakpoint.ifunc(f, line_number, condition)
}
###############################################################################
idbg.print_breakpoints <- function()
{
  
  breakpoints <- NULL
  for (fname in idbg()[["ifunc_names"]])
  {
    f <-  idbg.match.ifunc(fname)
    if (is.null(f))
      next
    fbp <- list_breakpoints.ifunc(f)
    if (length(fbp))
      breakpoints <- rbind(breakpoints, data.frame("function"=fname,line=fbp))
  }
  print(breakpoints)
}
###############################################################################
idbg.interact <- function(pos, func_name)
{
  debug_loop = TRUE

  frame_id <- sys.nframe()
  
  idbg.call_stack_top(frame_id-1, func_name, pos)
  assign("debug_frame", frame_id-1, envir=idbg())
  
  #get("call_stack", envir=idbg())[[frame_id-1]] <- list(func_name, pos)
  #length(idbg()$call_stack) <- frame_id-1


  func <- match.fun(func_name)
  breakpoints <- attr(func, "data")$breakpoints
  # eval the breakpoint at the parrent - for conditional breakpoint support
  if (! eval.parent(breakpoints[[pos]]))
  {
    
    if (! is.na(idbg()$break_frame) && frame_id-1 != idbg()$break_frame)
      return(NULL)
    assign("break_frame", -1, envir=idbg())
  }


  list_source.ifunc(func, pos)


  while (debug_loop)
  {
    line = readline(sprintf("debug %d: ",pos));
    if (line == "")
      line <- "n"

    words = strsplit(line, "\ +")[[1]];
    if (length(words) == 0)
      next
    if (words[1] == "")
      words = words[2:length(words)];

    cmd = words[1];
    words = words[-1];
    if (cmd == "n")
    {
      assign("break_frame", sys.nframe() -1, envir=idbg())
      debug_loop <- FALSE
    }
    else
    if (cmd == "s")
    {
      addr <- attr(func, "data")$key2addr[[pos]]
      e <- body(func)
      has_error <- FALSE
      for (h in addr)
        if (h <= length(e))
          e <- e[[h]]
        else
        {
          has_error <- TRUE
          break
        }

      if (! has_error)
        idbg.prepare_step(e)
      assign("break_frame", NA, envir=idbg())
      debug_loop <- FALSE
    }
    else
    if (cmd == "c")
    {
      debug_loop <- FALSE
    }
    else
    if (cmd == "o")
    {
      assign("break_frame", sys.nframe()-2, envir=idbg())
      debug_loop <- FALSE
    }
    else
    if (cmd == "l")
    {
      q <- idbg()$call_stack[[idbg()$debug_frame]]
      lfunc <- q[[1]]
      lpos <- q[[2]]
      if (length(words) == 0)
        list_source.ifunc(lfunc, lpos)
      else
      if (length(words) == 1)
        list_source.ifunc(lfunc, lpos, words[[1]], words[[1]])
      else
      if (length(words) >= 2)
        list_source.ifunc(lfunc, lpos, words[[1]], words[[2]])
    }
    else
    if (cmd == "b")
    {
      if (length(words) == 0)
        idbg.print_breakpoints()
      else
      {
        
        # if first argument is a number than break at current func otherwise it is a func name string
        bp_line <-  as.numeric(words[[1]])
        bp_cond_arg <- 2
        if (is.na(bp_line))
        {
          # this must be a name of a function, break on the 1st line 
          bp_func_name <- words[[1]]
          if (length(words) > 1)
          {  
            bp_line <-  as.numeric(words[[2]])
            if (! is.na(bp_line))
              bp_cond_arg <- 3
          }
        } 
        else
        {
          # this is a line number to break in current proc
          bp_func_name <- func_name
        }

        # conditional breakpoint support not working yet - to set a breakpoint condition is TRUE, to clear FALSE
        # condition <- parse(words[[2]])
        bp_condition <- TRUE
        if (length(words) >= bp_cond_arg && (words[[bp_cond_arg]] == "FALSE" || words[[bp_cond_arg]]=="F"))
          bp_condition <- FALSE
        
        idbg.bp(bp_func_name, bp_line, bp_condition) 
      }
    }
    else
    if (cmd == "w") 
    {
      # print the stack
      # todo - if the stack include non debugable functions (eg. apply) print data about them
      widx <- 0
      for (q in idbg()$call_stack)
      {
        widx <- widx + 1
        if (is.null(q))
          cat("???\n")
        else
        {
          if (widx == idbg()$debug_frame)
            wchar <- "*"
          else
            wchar <- " "
          cat(wchar, widx, q[[1]]," ")
          list_source.ifunc(q[[1]], q[[2]], 0, 0)
        }
      }
    }
    else
    if (cmd == "u") 
    {
      # up in the stack
      if (idbg()$debug_frame > 1)
        assign("debug_frame", idbg()$debug_frame-1, envir=idbg())
    }
    else
    if (cmd == "d") 
    {
      # down in the stack
      if (idbg()$debug_frame < frame_id)
        assign("debug_frame", idbg()$debug_frame+1, envir=idbg())
    }
    else
    if (cmd == "q")
    {
      invokeRestart(findRestart("abort"))
    }
    else
    if (cmd == "h")
    {
      cat("h - help. print this message\n")
      cat("n - next. Empty line is the same as 'n'\n")
      cat("s - step into\n")
      cat("o - step out\n")
      cat("c - continue\n")
      cat("q - quit\n")
      cat("b - print breakpoints - not implemented yet\n")
      cat("b <func_name> [FALSE] - set/unset a breakpoint in first line of function\n")
      cat("b <line_number> [FALSE] - set/unset a breakpoint in current function\n")
      cat("b <func_name> <line_number> [FALSE] - set/unset a breakpoint in function at at line_number\n")
      cat("w - print the stack\n")
      cat("u - go up the stack\n")
      cat("d - go down the stack\n")
      cat("l [nlines] - print nlines of source before and after current position\n")
      cat("l [nlines_back] [n_lines_forward] - print source around current position\n")
      cat("x expr - execute expression. Any expression that doesn't match the above options will also be executed\n")
    }
    else
    {
      if (cmd == "x")
      {
        expr <- ""
        for (w in words)
          expr = paste(expr, w)
      }
      else
        expr <- line

      e<- try(
        #print(eval.parent(parse(text=expr))),
        print(eval(parse(text=expr),envir=sys.frame(idbg()$debug_frame))),
        silent = TRUE
      )
      if (class(e) == "try-error")
        cat(geterrmessage())
      timestamp(expr,prefix="",suffix="",quiet =TRUE)
    }
  }
}
###############################################################################
idbg.match.ifunc <- function(fname)
{
  f <-match.fun(fname)
  if (is.ifunc(f))
    return(f)

  return(NULL)
}
###############################################################################
idbg.prepare_step <- function(expr)
{
  expr_len <- length(expr)

  
  e <- expr[[1]]
  ifunc(as.character(expr[[1]]))
  if (expr_len > 1)
  {
    for (i in 2:expr_len) 
    { 
      if (class(expr[[i]]) == "call")
        idbg.prepare_step(expr[[i]])
    }
  }
}
###############################################################################
idbg.instrument_if <- function(if_expr, func_name, ienv)
{
  l <- list(if_expr[[1]], if_expr[[2]])
  ienv$addr <- c(ienv$addr, NA)

  for (idx in 3:4)
  {
    if (length(if_expr) >= idx)
    {
      ienv$addr[[length(ienv$addr)]] <- idx
      ikey <- ienv$key

      if (class(if_expr[[idx]]) == "{")
        l <- c(l, idbg.instrument_expr_list(if_expr[[idx]], func_name, ienv))
      else
        l <- c(l, idbg.instrument_expr_list(as.call(c(as.name("{"), if_expr[[idx]])), func_name, ienv))
    }
  }
  ienv$addr <- ienv$addr[-length(ienv$addr)]
  return(as.call(l))
}
###############################################################################
idbg.instrument_for <- function(for_expr, func_name, ienv)
{
  key <- ienv$key
  l <- list(for_expr[[1]], for_expr[[2]], for_expr[[3]])
  ienv$addr <- c(ienv$addr, 4)

  if (class(for_expr[[4]]) == "{")
    l <- c(l, idbg.instrument_expr_list(for_expr[[4]], func_name, ienv))
  else
    l <- c(l, idbg.instrument_expr_list(as.call(c(as.name("{"), for_expr[[4]])), func_name, ienv))
  ienv$addr <- ienv$addr[-length(ienv$addr)]
  return(as.call(l))
}
###############################################################################
idbg.instrument_while <- function(while_expr, func_name, ienv)
{
  key <- ienv$key
  l <- list(while_expr[[1]], while_expr[[2]])
  ienv$addr <- c(ienv$addr, 3)

  if (class(while_expr[[3]]) == "{")
    l <- c(l, idbg.instrument_expr_list(while_expr[[3]], func_name, ienv))
  else
    l <- c(l, idbg.instrument_expr_list(as.call(c(as.name("{"), while_expr[[3]])), func_name, ienv))
  return(as.call(l))
}
###############################################################################
idbg.instrument_repeat <- function(repeat_expr, func_name, ienv)
{
  key <- ienv$key
  l <- list(repeat_expr[[1]])
  ienv$addr <- c(ienv$addr, 2)

  if (class(repeat_expr[[2]]) == "{")
    l <- c(l, idbg.instrument_expr_list(repeat_expr[[2]], func_name, ienv))
  else
    l <- c(l, idbg.instrument_expr_list(as.call(c(as.name("{"), repeat_expr[[2]])), func_name, ienv))
  return(as.call(l))
}
###############################################################################
idbg.instrument_expr_list <- function(expr, func_name, ienv)
{
  expr_len <- length(expr)
  l<-list()
  ikey <- ienv$key
  ienv$addr <- c(ienv$addr, NA)

  if (expr_len > 0 && class(expr[[1]]) == "name" && expr[[1]] != "{")
  {
    return(idbg.instrument_expr_list(as.call(c(as.name("{"), expr)), func_name, ienv))
  }
  
  for (i in seq_len(expr_len)) 
  { 
    ienv$addr[[length(ienv$addr)]] <- 2*(i-1)+1
    e <- expr[[i]]
    #ikey<- paste(key,i,sep=".")

    if (class(e) == "{")
      e<- idbg.instrument_expr_list(e, func_name, ienv)
    else
    if (class(e) == "if")
      e<- idbg.instrument_if(e, func_name, ienv)
    else
    if (class(e) == "for")
      e<- idbg.instrument_for(e, func_name, ienv)
    else
    if (class(e) == "while")
      e<- idbg.instrument_while(e, func_name, ienv)
    else    
    if (class(e) == "repeat" || (is.call(e) && e[[1]] == "repeat"))
      e<- idbg.instrument_repeat(e, func_name, ienv)
      
        
    ienv$key <- ienv$key + 1
    
    ikey <- ienv$key
    l <- c(l, e, substitute(idbg.interact(i,name), list(i=ikey,name=func_name))) 
    
    # increase by two in order to get the addess of the line following the bp and not the command address
    ienv$addr[[length(ienv$addr)]] <- ienv$addr[[length(ienv$addr)]] + 2
    ienv$key2addr[[ikey]] <- ienv$addr
    

  }

  ienv$addr <- ienv$addr[-length(ienv$addr)]

  return(as.call(l))
}
###############################################################################
idbg.gen_source <- function(instrumented_body)
{
  s <- format(instrumented_body)

  bp_pos <- regexpr("idbg.interact\\([0-9]+", format(s))
  #bp_len <- attr(bp_pos, "match.length") - 3
  is_bp <- bp_pos != -1
  nbp <- sum(is_bp)
  #bp_key <- as.numeric(substr(s[is_bp], bp_pos[is_bp] + 3, bp_pos[is_bp] + 3+ bp_len[is_bp]-1))
  key2line <- which(c(FALSE, is_bp)) - seq_len(nbp)
  s <- s[! is_bp]
  return(list(src=s, key2line=key2line))
}
###############################################################################
ifunc <- function(fname)
{
  if (! is.character(fname))
    return(NULL)

  if (fname %in% builtins())
    return(NULL)

  
  #func <- try (match.fun(fname))
  #if (length(class(func)) == 1 && class(func) == "try-error")
  #  return(NULL)

  # search for the func and the frame that it belongs to
  found <- FALSE
  for (frame_id in seq(sys.nframe(),0))
  {
    envir <- sys.frame(frame_id)
    if (exists(fname, envir = envir, mode = "function", inherits = FALSE))
    {
      func <- get(fname, envir = envir, mode = "function", inherits = FALSE)
      found <- TRUE
      break
    }
  }

  if (! found)
    return(NULL)

  if (is.primitive(func))
    return(func)
  
  if (is.ifunc(func))
    return(func)

  ienv <- new.env()
  ienv$key <- 0
  ienv$addr <- c()
  ienv$key2addr <- list()

  l <- idbg.instrument_expr_list(body(func),fname,ienv)
  ret <- func
  body(ret) <- as.call(l)  
  attr(ret,"orig") <- func
  q <-idbg.gen_source(l)
  data <- new.env()
  attr(ret,"data") <- data
  data[["src"]] <- q$src
  data[["key2line"]] <- q$key2line
  data[["key2addr"]] <- ienv$key2addr
  data[["breakpoints"]] <- rep(FALSE,length(q$key2line))


  class(ret) <- c("ifunc", class(ret))
  
  
  idbg.add_ifunc(fname)
  assign(fname, ret, envir=envir)
}
###############################################################################
is.ifunc <- function(x)
{
  return(inherits(x, "ifunc"))
}
###############################################################################
print.ifunc <- function(x)
{
  src <- attr(x, "data")$src
  cat(format(args(x))[[1]],"\n")
  for (line in src)
    cat(line,"\n")
}
###############################################################################
# to clear a breakpoint set expr to FALSE
breakpoint.ifunc <- function(f, line_number, expr=TRUE)
{
  if (! is.ifunc(f) )
    return(FALSE)

  key2line <- attr(f, "data")$key2line
  if (length(key2line) == 0)
    return(FALSE)
  
  if (is.na(line_number))
    d <- 1
  else
    d <- which(line_number - key2line==0)
  if (length(d) != 1)
    return(FALSE)
  attr(f,"data")$breakpoints[[d]] <- expr
  return(TRUE)
}
###############################################################################
list_breakpoints.ifunc <- function(f)
{
  if (! is.ifunc(f))
    return(NULL)
  keys <- which(attr(f,"data")$breakpoints != FALSE)
  lines <- (attr(f, "data")$key2line)[keys]
  return(lines)
}
###############################################################################
list_source.ifunc <- function(func, pos, back=10, forward=10)
{
  back <- suppressWarnings(as.integer(back))
  forward <- suppressWarnings(as.integer(forward))

  if (is.na(back) || back < 0)
    back <- 10
  if (is.na(forward) || forward < 0)
    forward <- 10

  if (is.character(func))
    func <- match.fun(func)

  if (is.ifunc(func))
  {  
    src <- attr(func, "data")$src
    key2line <- attr(func, "data")$key2line
    pos <- key2line[[pos]]
    start <- pos - back
    end <- pos + forward
  
    if (start < 1 )
      start <- 1
    if (end > length(src))
      end <- length(src)

    if (start == 1)
      cat(format(args(func))[[1]],"\n")

    for (i in start:end ) 
    { 
      line <- sprintf("%04d %s",i,src[[i]])
      if (i == pos)
        cat("=>")
      else
        cat("  ")
      cat(line,"\n")
    }
  }
}
###############################################################################
#idbg.gui <- function()
#{
#  tt2 <- tktoplevel()
#  nb <- tk2notebook(tt2, tabs = c("Src"))
#  tkpack(nb, fill = "both", expand = 1)
#  tb1 <- tk2notetab(nb, "Src")
#  xscr <- tkscrollbar(tb1, repeatinterval=5,orient="horizontal",
#                       command=function(...)tkxview(txt,...))
#  yscr <- tkscrollbar(tb1, repeatinterval=5,
#                       command=function(...)tkyview(txt,...))
#  txt <- tktext(tb1,bg="white",font="courier",
#    xscrollcommand=function(...)tkset(xscr,...),yscrollcommand=function(...)tkset(yscr,...))
#  tkgrid(txt,yscr)
#  tkgrid(xscr)
#  tkgrid.configure(yscr,sticky="ns")
#  tkgrid.configure(xscr,sticky="ew")
#  for (i in (1:100)) tkinsert(txt,"end",paste(i,"^ 2 =",i*i,"\n"))
#  tkconfigure(txt, state="disabled")
#  tkfocus(txt)
#
#
#  tk2notetab.select(nb, "Src")
#  tk2notetab.text(nb) # Text of the currently selected tab  
#
#
#  ntabs <- tkcget(nb, width=NULL)
#  print(ntabs)
#
#  return(tt2)
#}
################################################################################
#idbg.gui.view_src <- function(notebook, tab_name, src)
#{
#  # look for the tab_name
#  
#  
#    
#
#}
#foo <- function(a,b,c)
#{
#  d <- a + b
#  e <- a * (b + c)
#  if (d < 0) 
#    print(a)
#  else  
#  print(b)
#  q <- a <- b <- b
#  for (i in 1:10)
#  {
#    print(i)
#  }
#}
#bzz <- function(a)
#{
#  if ( a > 0)
#  {
#    a <- 0
#    bar()
#  }
#  else
#    foo(1,a+1,a+2)
#  
#  print(a)
#}
#
#
#bar <- function()
#{
#  c <- eval.parent(expression(a <-8));
#  print("========")
#  print(c);
#  print("########")
#}
#
#
#f0 <- function(fname)
#{
#  f1(fname)
#}
#
#f1 <- function(fname)
#{
#  f2 <- function()
#  {
#    cat("f2\n")
#  }
#
#  for (frame_id in seq(sys.nframe(),0))
#  {
#    envir <- sys.frame(frame_id)
#    if (exists(fname, envir = envir, mode = "function", inherits = FALSE))
#      qqq <- get(fname, envir = envir, mode = "function", inherits = FALSE)
#  }
#  qqq()
#}
#




idbg.init()