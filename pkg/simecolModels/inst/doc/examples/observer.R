
setGeneric("observer", function(obj, ...) standardGeneric("observer"))
setGeneric("observer<-", function(obj, value) standardGeneric("observer<-"))


setMethod("observer", "indbasedModel",
    function(obj) {obj@observer}
)

setMethod("observer<-", "indbasedModel",
    function(obj, value) {
      obj@observer <- value
      invisible(obj)
    }
)
 
