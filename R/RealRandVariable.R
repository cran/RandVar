## generating function
RealRandVariable <- function(Map = list(function(x){1}), Domain = NULL, Range) {
    if(missing(Range)) Range <- Reals()
    if(!is(Range, "Reals"))
        stop("'Range' has to be of class 'Reals'")

    return(new("RealRandVariable", Map = Map, Domain = Domain, Range = Range))
}

## replace method
setReplaceMethod("Range", "RealRandVariable", 
    function(object, value){ 
        object@Range <- value 
        if(!is(value, "Reals"))
            stop("'Range' of 'value' is not the Real space")
        object
    })
