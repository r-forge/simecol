## package: simecol
## sEdit = simple Edit
## function for editing named vectors and simple lists

## Not all data types can be handled in the moment, e.g.:
##  long vectors/lists (with several pages)
##  character vectors correctly
##  no error handling if wrong data are entered

sEdit <- function(x, title="Please enter values:") {
    ## conversion functions
    chrToNum <- function(x) {
      row.names <- names(x)
      x <- type.convert(x, as.is=TRUE)
      names(x) <- row.names
      x
    }
    listToNum <- function(x) {
      if (is.character(x)) {
        type.convert(unlist(strsplit(x, ",")), as.is=TRUE)
      } else {
        unlist(x)
      }
    }
    listToStr <- function(x) {
      paste(x, collapse=", ")
    }
    ## create and handle dialog box
    editVec <- function(slot) {
      ## dialog box helper functions
      build <- function(slot) {
        ret <- character(length(slot))
        for (i in 1:length(slot)) {
            ret[i] <- tclvalue(row.names[i])
        }
        ret
      }
      reset <- function() {
        for (i in 1:length(slot)) {
            tclvalue(row.names[i]) <- slot[i]
        }
      }
      ## create dialog box
      tt <- tktoplevel()
      tkwm.title(tt,title)
      entries <- as.list(slot)
      row.names <- names(slot)
      if (is.null(row.names)) {
        row.names <- paste("var",1:length(slot),sep="")
      }
      for (i in 1:length(slot)) {
        entries[[i]] <- tkentry(tt, textvariable=row.names[i])       
        tkgrid(tklabel(tt,text=row.names[i]), entries[[i]])
      }
      reset.but  <- tkbutton(tt, text="Reset", command=reset)
      submit.but <- tkbutton(tt, text="OK",
                             command=function()tclvalue(done) <- 1)
      tkgrid(reset.but, submit.but)
      done <- tclVar(0)
      ## capture destroy event
      tkbind(tt, "<Destroy>", function()tclvalue(done) <- 2)
      ## initialize with oiginal slot values
      reset()
      tkwm.deiconify(tt)        # raise the tk window
      tkwait.variable(done)
      if(tclvalue(done)=="2") stop("dialog cancelled")
      tkdestroy(tt)
      ret <- build(slot)
      names(ret) <- names(slot) # restore original names
      ret
    }
    ## -------------- main ----------------
    tcltk <- require("tcltk", quietly=TRUE)
    if (is.vector(x) & !is.list(x) & (tcltk)) {
      ## slot is a vector
      ret  <- editVec(x)
      ret  <- chrToNum(ret)
    } else if (is.list(x) & (sum(sapply(x, is.list)) < 1) & (tcltk)) {
      ## slot is a list of vectors
      x <- sapply(x, listToStr)
      ret  <- editVec(x)
      ret  <- lapply(ret, listToNum)
    } else {
      ## default editor, e.g. data.frame or if tcltk is missing
      ret  <- edit(x) 
    }
    return(ret)
}

setGeneric("fixParms", function(x) standardGeneric("fixParms"))

setGeneric("fixInit", function(x) standardGeneric("fixInit"))

setGeneric("fixTimes", function(x) standardGeneric("fixTimes"))

setMethod("fixParms", "simObj",
  function(x) {
    sl   <- "parms"
    subx <- substitute(x)
    if (is.name(subx)) 
       subx <- deparse(subx)
    if (!is.character(subx) || length(subx) != 1) 
        stop("this function requires a name")
    if (!(sl %in% slotNames(x)))
        stop(paste("'", sl, "' does not exist in ", subx, sep=""))
    parent <- parent.frame()
    ret <- sEdit(slot(x, sl), sl)
    slot(x, sl) <- ret
    ## interactive function is assumed to work
    ## in global environment
    assign(subx, x, env=.GlobalEnv)
  }
)

setMethod("fixTimes", "simObj",
  function(x) {
    sl <- "times"
    subx <- substitute(x)
    if (is.name(subx)) 
       subx <- deparse(subx)
    if (!is.character(subx) || length(subx) != 1) 
        stop("this function requires a name")
    if (!(sl %in% slotNames(x)))
        stop(paste("'", sl, "' does not exist in ", subx, sep=""))
    parent <- parent.frame()
    if (sum(names(slot(x, sl)) == c("from", "to", "by"))==3) {
      ret <- sEdit(slot(x, sl), sl)
    }else {
      ret <- edit(slot(x, sl))
    }
    slot(x, sl) <- ret
    ## interactive function is assumed to work
    ## in global environment
    assign(subx, x, env=.GlobalEnv)

  }
)

setMethod("fixInit", "simObj",
  function(x) {
    sl <- "init"
    subx <- substitute(x)
    if (is.name(subx)) 
       subx <- deparse(subx)
    if (!is.character(subx) || length(subx) != 1) 
        stop("this function requires a name")
    if (!(sl %in% slotNames(x)))
        stop(paste("'", sl, "' does not exist in ", subx, sep=""))
    parent <- parent.frame()
    ret <- sEdit(slot(x, sl), sl)
    slot(x, sl) <- ret
    ## interactive function is assumed to work
    ## in global environment
    assign(subx, x, env=.GlobalEnv)

  }
)


    

