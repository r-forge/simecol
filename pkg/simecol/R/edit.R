## package: simecol
## editSlots = simple edit of data slots
## function for editing named vectors and simple lists

## Not all data types can be handled in the moment, e.g.:
##  long vectors/lists (with several pages)
##  character vectors correctly
##  no error handling if wrong data are entered

## these functions replace the deprecated "fixXXX" functions

setGeneric("editParms", function(x) standardGeneric("editParms"))

setGeneric("editInit", function(x) standardGeneric("editInit"))

setGeneric("editTimes", function(x) standardGeneric("editTimes"))

setMethod("editParms", "simObj",
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

            x
          }
)

setMethod("editTimes", "simObj",
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

            x
          }
)

setMethod("editInit", "simObj",
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

            x
          }
)
