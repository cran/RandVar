pkgname <- "RandVar"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('RandVar')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("0RandVar-package")
### * 0RandVar-package

flush(stderr()); flush(stdout())

### Name: RandVar-package
### Title: Implementation of random variables
### Aliases: RandVar-package RandVar
### Keywords: package

### ** Examples

library(RandVar)
#vignette("RandVar")



cleanEx()
nameEx("EuclRandMatrix-class")
### * EuclRandMatrix-class

flush(stderr()); flush(stdout())

### Name: EuclRandMatrix-class
### Title: Euclidean random matrix
### Aliases: EuclRandMatrix-class
###   coerce,EuclRandMatrix,EuclRandVarList-method Dim
###   Dim,EuclRandMatrix-method Dim<- Dim<-,EuclRandMatrix-method
###   [,EuclRandMatrix-method ncol,EuclRandMatrix-method
###   nrow,EuclRandMatrix-method dimension,EuclRandMatrix-method
###   evalRandVar,EuclRandMatrix,numeric,missing-method
###   evalRandVar,EuclRandMatrix,matrix,missing-method
###   evalRandVar,EuclRandMatrix,numeric,Distribution-method
###   evalRandVar,EuclRandMatrix,matrix,Distribution-method
###   t,EuclRandMatrix-method show,EuclRandMatrix-method
###   %*%,matrix,EuclRandMatrix-method %*%,numeric,EuclRandMatrix-method
###   %*%,EuclRandMatrix,matrix-method %*%,EuclRandMatrix,numeric-method
###   %*%,EuclRandMatrix,EuclRandMatrix-method
###   Arith,numeric,EuclRandMatrix-method
###   Arith,EuclRandMatrix,numeric-method
###   Arith,EuclRandMatrix,EuclRandMatrix-method Math,EuclRandMatrix-method
###   E,UnivariateDistribution,EuclRandMatrix,missing-method
###   E,AbscontDistribution,EuclRandMatrix,missing-method
###   E,DiscreteDistribution,EuclRandMatrix,missing-method
###   E,MultivariateDistribution,EuclRandMatrix,missing-method
###   E,DiscreteMVDistribution,EuclRandMatrix,missing-method
###   E,UnivariateCondDistribution,EuclRandMatrix,numeric-method
###   E,AbscontCondDistribution,EuclRandMatrix,numeric-method
###   E,DiscreteCondDistribution,EuclRandMatrix,numeric-method
### Keywords: classes arith math

### ** Examples

L1 <- list(function(x){x}, function(x){x^2}, function(x){x^3}, function(x){x^4}, 
           function(x){x^5}, function(x){x^6})
L2 <- list(function(x){exp(x)}, function(x){abs(x)}, 
           function(x){sin(x)}, function(x){floor(x)})

R1 <- new("EuclRandMatrix", Map = L1, Dim = as.integer(c(3,2)), 
                            Domain = Reals(), Range = Reals())
dimension(R1)
R1[1:2, 2]
R1[1:2, 1:2]
Map(R1[1,2])
Map(t(R1)[2,1])

R2 <- EuclRandMatrix(Map = L2, ncol = 2, Domain = Reals(), dimension = 1)
dimension(R2)
(DL <- imageDistr(R2, Norm()))
plot(DL)

Map(gamma(R2)) # "Math" group

## "Arith" group
Map(2/R1)
Map(R2 * R2)



cleanEx()
nameEx("EuclRandMatrix")
### * EuclRandMatrix

flush(stderr()); flush(stdout())

### Name: EuclRandMatrix
### Title: Generating function for EuclRandMatrix-class
### Aliases: EuclRandMatrix
### Keywords: classes

### ** Examples

L1 <- list(function(x){x}, function(x){x^2}, function(x){x^3}, function(x){x^4}, 
           function(x){x^5}, function(x){x^6})
L2 <- list(function(x){exp(x)}, function(x){abs(x)}, 
           function(x){sin(x)}, function(x){floor(x)})

R1 <- EuclRandMatrix(Map = L1, nrow = 3, Domain = Reals(), dimension = 1)
R1[1:2, 2]
R1[1:2, 1:2]
Map(R1[1,2])
Map(t(R1)[2,1])

R2 <- EuclRandMatrix(Map = L2, ncol = 2, Domain = Reals(), dimension = 1)
(DL <- imageDistr(R2, Norm()))
plot(DL)

Map(gamma(R2)) # "Math" group

## "Arith" group
Map(2/R1)
Map(R2 * R2)


## The function is currently defined as
function(Map = list(function(x){1}), nrow = 1, ncol = 1,
                              Domain = NULL, dimension = 1) {
    if (missing(nrow)) 
        nrow <- ceiling(length(Map)/ncol)
    else if (missing(ncol)) 
        ncol <- ceiling(length(Map)/nrow)
    
    if(missing(Range))
        return(new("EuclRandMatrix", Map = Map, Domain = Domain, 
                   Range = EuclideanSpace(dimension = dimension),
                   Dim = as.integer(c(nrow, ncol))))
    else
        return(new("EuclRandMatrix", Map = Map, Domain = Domain, 
                   Range = Range, Dim = as.integer(c(nrow, ncol))))
}



cleanEx()
nameEx("EuclRandVarList-class")
### * EuclRandVarList-class

flush(stderr()); flush(stdout())

### Name: EuclRandVarList-class
### Title: List of Euclidean random variables
### Aliases: EuclRandVarList-class numberOfMaps
###   numberOfMaps,EuclRandVarList-method dimension,EuclRandVarList-method
###   evalRandVar,EuclRandVarList,numeric,missing-method
###   evalRandVar,EuclRandVarList,matrix,missing-method
###   evalRandVar,EuclRandVarList,numeric,Distribution-method
###   evalRandVar,EuclRandVarList,matrix,Distribution-method
###   imageDistr,EuclRandVarList,Distribution-method
###   t,EuclRandVarList-method show,EuclRandVarList-method
###   Arith,numeric,EuclRandVarList-method
###   Arith,EuclRandVarList,numeric-method
###   Arith,EuclRandVarList,EuclRandVarList-method
###   Math,EuclRandVarList-method %m%
###   %m%,EuclRandVarList,EuclRandVarList-method
###   %*%,matrix,EuclRandVarList-method %*%,EuclRandVarList,matrix-method
###   E,UnivariateDistribution,EuclRandVarList,missing-method
###   E,AbscontDistribution,EuclRandVarList,missing-method
###   E,DiscreteDistribution,EuclRandVarList,missing-method
###   E,MultivariateDistribution,EuclRandVarList,missing-method
###   E,DiscreteMVDistribution,EuclRandVarList,missing-method
###   E,UnivariateCondDistribution,EuclRandVarList,numeric-method
###   E,AbscontCondDistribution,EuclRandVarList,numeric-method
###   E,DiscreteCondDistribution,EuclRandVarList,numeric-method
### Keywords: classes

### ** Examples

L1 <- list(function(x){x}, function(x){x^2}, function(x){x^3}, function(x){x^4}, 
           function(x){x^5}, function(x){x^6})
L2 <- list(function(x){exp(x)}, function(x){abs(x)}, 
           function(x){sin(x)}, function(x){floor(x)})

R1 <- new("EuclRandVariable", Map = L2, Domain = Reals(), Range = Reals())
R2 <- EuclRandMatrix(Map = L1, ncol = 2, Domain = Reals(), dimension = 1)
R3 <- EuclRandMatrix(Map = L2, ncol = 2, Domain = Reals(), dimension = 1)

(RL1 <- new("EuclRandVarList", list(R1, R2, R3)))
dimension(RL1)
as(R1, "EuclRandVarList")
as(R2, "EuclRandVarList")

Map(exp(RL1)[[1]]) # "Math" group

## "Arith" group
Map((1 + RL1)[[1]])
Map((RL1 * 2)[[2]])
Map((RL1 / RL1)[[3]])



cleanEx()
nameEx("EuclRandVarList")
### * EuclRandVarList

flush(stderr()); flush(stdout())

### Name: EuclRandVarList
### Title: Generating function for EuclRandVarList-class
### Aliases: EuclRandVarList
### Keywords: classes

### ** Examples

L1 <- list(function(x){x}, function(x){x^2}, function(x){x^3}, function(x){x^4}, 
           function(x){x^5}, function(x){x^6})
L2 <- list(function(x){exp(x)}, function(x){abs(x)}, 
           function(x){sin(x)}, function(x){floor(x)})

R1 <- new("EuclRandVariable", Map = L2, Domain = Reals(), Range = Reals())
R2 <- EuclRandMatrix(Map = L1, ncol = 2, Domain = Reals(), dimension = 1)
R3 <- EuclRandMatrix(Map = L2, ncol = 2, Domain = Reals(), dimension = 1)

(RL1 <- EuclRandVarList(R1, R2, R3))
is(R1, "EuclRandVarList")
as(R1, "EuclRandVarList")
is(R2, "EuclRandVarList")
as(R2, "EuclRandVarList")

Map(exp(RL1)[[1]]) # "Math" group

## "Arith" group
Map((1 + RL1)[[1]])
Map((RL1 * 2)[[2]])
Map((RL1 / RL1)[[3]])

## The function is currently defined as
function(...){ 
    new("EuclRandVarList", list(...)) 
}



cleanEx()
nameEx("EuclRandVariable-class")
### * EuclRandVariable-class

flush(stderr()); flush(stdout())

### Name: EuclRandVariable-class
### Title: Euclidean random variable
### Aliases: EuclRandVariable-class
###   coerce,EuclRandVariable,EuclRandMatrix-method
###   coerce,EuclRandVariable,EuclRandVarList-method
###   Range<-,EuclRandVariable-method [,EuclRandVariable-method evalRandVar
###   evalRandVar,EuclRandVariable,numeric,missing-method
###   evalRandVar,EuclRandVariable,matrix,missing-method
###   evalRandVar,EuclRandVariable,numeric,Distribution-method
###   evalRandVar,EuclRandVariable,matrix,Distribution-method imageDistr
###   imageDistr,EuclRandVariable,Distribution-method
###   dimension,EuclRandVariable-method t,EuclRandVariable-method
###   %*%,matrix,EuclRandVariable-method
###   %*%,numeric,EuclRandVariable-method
###   %*%,EuclRandVariable,matrix-method
###   %*%,EuclRandVariable,numeric-method
###   %*%,EuclRandVariable,EuclRandVariable-method
###   %*%,EuclRandVariable,EuclRandMatrix-method
###   %*%,EuclRandMatrix,EuclRandVariable-method
###   Arith,numeric,EuclRandVariable-method
###   Arith,EuclRandVariable,numeric-method
###   Arith,EuclRandVariable,EuclRandVariable-method
###   Math,EuclRandVariable-method
###   E,UnivariateDistribution,EuclRandVariable,missing-method
###   E,AbscontDistribution,EuclRandVariable,missing-method
###   E,DiscreteDistribution,EuclRandVariable,missing-method
###   E,MultivariateDistribution,EuclRandVariable,missing-method
###   E,DiscreteMVDistribution,EuclRandVariable,missing-method
###   E,UnivariateCondDistribution,EuclRandVariable,numeric-method
###   E,AbscontCondDistribution,EuclRandVariable,numeric-method
###   E,DiscreteCondDistribution,EuclRandVariable,numeric-method
### Keywords: classes arith math

### ** Examples

L1 <- list(function(x){x}, function(x){x^2}, function(x){x^3}, function(x){x^4})
L2 <- list(function(x){exp(x)}, function(x){abs(x)}, 
           function(x){sin(x)}, function(x){floor(x)})

R1 <- new("EuclRandVariable", Map = L1, Domain = Reals(), Range = Reals())
dimension(R1)
Map(R1)
Range(R1)
R1[2]
Map(R1[3])
Map(R1[c(1,2,4)])
Map(R1[2:4])
evalRandVar(R1, rnorm(1))
x <- as.matrix(rnorm(10))
res.R1 <- evalRandVar(R1, x)
res.R1[2,,] # results for Map(R1)[[2]](x)
res.R1[2,1,] # results for Map(R1)[[2]](x[1,])

R2 <- EuclRandVariable(L2, Domain = Reals(), dimension = 1)
dimension(R2)
DL1 <- imageDistr(R2, Norm())
plot(DL1)

Domain(R2) <- EuclideanSpace(dimension = 2)
Range(R2) <- EuclideanSpace(dimension = 2)
dimension(R2)
(X <- matrix(c(x, rnorm(10)), ncol = 2))
res.R2 <- evalRandVar(R2, X)
res.R2[3,,1] # results for Map(R2)[[3]](X[,1])

Map(log(abs(R2))) # "Math" group generic

# "Arith" group generic
Map(3 + R1)
Map(c(1,3,5) * R1)
try(1:5 * R1) # error
Map(1:2 * R2)
Map(R2 - 5)
Map(R1 ^ R1)




cleanEx()
nameEx("EuclRandVariable")
### * EuclRandVariable

flush(stderr()); flush(stdout())

### Name: EuclRandVariable
### Title: Generating function for EuclRandVariable-class
### Aliases: EuclRandVariable
### Keywords: classes

### ** Examples

L1 <- list(function(x){x}, function(x){x^2}, function(x){x^3}, function(x){x^4})
L2 <- list(function(x){exp(x)}, function(x){abs(x)}, 
           function(x){sin(x)}, function(x){floor(x)})

R1 <- EuclRandVariable(Map = L1, Domain = Reals(), dimension = 1)
Map(R1)
Range(R1)
Range(R1) <- Reals()
R1[2]
Map(R1[3])
Map(R1[c(1,2,4)])
Map(R1[2:4])
evalRandVar(R1, rnorm(1))
x <- as.matrix(rnorm(10))
res.R1 <- evalRandVar(R1, x)
res.R1[2,,] # results for Map(R1)[[2]](x)
res.R1[2,1,] # results for Map(R1)[[2]](x[1,])

R2 <- EuclRandVariable(L2, Domain = Reals(), dimension = 1)
DL1 <- imageDistr(R2, Norm())
plot(DL1)

Domain(R2) <- EuclideanSpace(dimension = 2)
Range(R2) <- EuclideanSpace(dimension = 2)
(X <- matrix(c(x, rnorm(10)), ncol = 2))
res.R2 <- evalRandVar(R2, X)
res.R2[3,,1] # results for Map(R2)[[3]](X[,1])

Map(log(abs(R2))) # "Math" group generic

# "Arith" group generic
Map(3 + R1)
Map(c(1,3,5) * R1)
try(1:5 * R1) # error
Map(1:2 * R2)
Map(R2 - 5)
Map(R1 ^ R1)


## The function is currently defined as
function(Map = list(function(x){1}), Domain = NULL, dimension = 1, Range) {
    if(missing(Range))
        return(new("EuclRandVariable", Map = Map, Domain = Domain, 
                   Range = EuclideanSpace(dimension = dimension)))
    else
        return(new("EuclRandVariable", Map = Map, Domain = Domain, 
                   Range = Range))
}



cleanEx()
nameEx("RandVariable-class")
### * RandVariable-class

flush(stderr()); flush(stdout())

### Name: RandVariable-class
### Title: Random variable
### Aliases: RandVariable-class Map Domain Range compatibleDomains
###   Map,RandVariable-method Domain,RandVariable-method
###   Range,RandVariable-method Map<- Domain<- Range<-
###   Map<-,RandVariable-method Domain<-,RandVariable-method
###   Range<-,RandVariable-method
###   compatibleDomains,RandVariable,RandVariable-method
###   length,RandVariable-method show,RandVariable-method
### Keywords: classes

### ** Examples

(R1 <- new("RandVariable"))
Map(R1)
Domain(R1)
Range(R1)
Map(R1) <- list(function(x){ceiling(x)}, function(x){floor(x)})
Domain(R1) <- Reals()
Range(R1) <- Naturals()
R1
Map(R1)
length(R1)

R2 <- R1
Domain(R2) <- Naturals()
compatibleDomains(R1, R2)
Domain(R2) <- NULL
compatibleDomains(R1, R2)
Domain(R2) <- EuclideanSpace(dimension = 1)
compatibleDomains(R1, R2)
Domain(R2) <- EuclideanSpace(dimension = 2)
compatibleDomains(R1, R2)



cleanEx()
nameEx("RandVariable")
### * RandVariable

flush(stderr()); flush(stdout())

### Name: RandVariable
### Title: Generating function for RandVariable-class
### Aliases: RandVariable
### Keywords: classes

### ** Examples

(R1 <- RandVariable())
Map(R1)
Domain(R1)
Range(R1)
Map(R1) <- list(function(x){ceiling(x)}, function(x){floor(x)})
Domain(R1) <- Reals()
Range(R1) <- Naturals()
R1
Map(R1)
length(R1)

R2 <- R1
Domain(R2) <- Naturals()
compatibleDomains(R1, R2)
Domain(R2) <- NULL
compatibleDomains(R1, R2)
Domain(R2) <- EuclideanSpace(dimension = 1)
compatibleDomains(R1, R2)
Domain(R2) <- EuclideanSpace(dimension = 2)
compatibleDomains(R1, R2)

## The function is currently defined as
function(Map = list(function(x){ }), Domain = NULL, Range = NULL) {
    return(new("RandVariable", Map = Map, Domain = Domain, Range = Range))
}



cleanEx()
nameEx("RealRandVariable-class")
### * RealRandVariable-class

flush(stderr()); flush(stdout())

### Name: RealRandVariable-class
### Title: Real random variable
### Aliases: RealRandVariable-class Range<-,RealRandVariable-method
### Keywords: classes

### ** Examples

new("RealRandVariable", Map=list(function(x){x}), Range = Reals())



cleanEx()
nameEx("RealRandVariable")
### * RealRandVariable

flush(stderr()); flush(stdout())

### Name: RealRandVariable
### Title: Generating function for RealRandVariable-class
### Aliases: RealRandVariable
### Keywords: classes

### ** Examples

RealRandVariable(Map = list(function(x){x}), Domain = Reals())

## The function is currently defined as
function(Map = list(function(x){1}), Domain = NULL, Range) {
    if(missing(Range)) Range <- Reals()
    if(!is(Range, "Reals"))
        stop("'Range' has to be of class 'Reals'")

    return(new("RealRandVariable", Map = Map, 
               Domain = Domain, Range = Reals()))
}



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
