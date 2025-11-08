
## HW5_Class.R - Sparse Numeric S4 Class Implementation ##

setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos   = "integer",
    length = "integer"
  )
)

setValidity("sparse_numeric", function(object) {
  if(length(object@value) != length(object@pos)) {
    return("length of 'value' and 'pos' must match")
  }
  if(any(object@pos < 1 | object@pos > object@length)) {
    return("'pos' indices must be between 1 and 'length'")
  }
  if(any(duplicated(object@pos))) {
    return("'pos' indices cannot have duplicates")
  }
  TRUE
})

setAs("numeric", "sparse_numeric", function(from) {
  new("sparse_numeric",
      value = from[from != 0],
      pos   = as.integer(which(from != 0)),
      length = as.integer(length(from))
  )
})

setAs("sparse_numeric", "numeric", function(from) {
  out <- numeric(from@length)
  out[from@pos] <- from@value
  out
})

setGeneric("asNumeric", function(x) standardGeneric("asNumeric"))
setMethod("asNumeric", "sparse_numeric", function(x) as(x, "numeric"))


sparse_arith <- function(e1, e2, op) {
  if(e1@length != e2@length) stop("Vectors must have the same length")
  all_pos <- union(e1@pos, e2@pos)
  
  v1 <- numeric(length(all_pos)); names(v1) <- all_pos
  v2 <- numeric(length(all_pos)); names(v2) <- all_pos
  v1[as.character(e1@pos)] <- e1@value
  v2[as.character(e2@pos)] <- e2@value
  
  new_vals <- switch(op,
                     add = v1 + v2,
                     sub = v1 - v2,
                     mult = v1 * v2)
  nonzero <- new_vals != 0
  new("sparse_numeric",
      value = new_vals[nonzero],
      pos   = as.integer(all_pos[nonzero]),
      length = e1@length)
}


setMethod("+", c("sparse_numeric", "sparse_numeric"),
          function(e1, e2) sparse_arith(e1, e2, "add"))

setMethod("-", c("sparse_numeric", "sparse_numeric"),
          function(e1, e2) sparse_arith(e1, e2, "sub"))

setMethod("*", c("sparse_numeric", "sparse_numeric"),
          function(e1, e2) {
            common_pos <- intersect(e1@pos, e2@pos)
            if(length(common_pos) == 0) {
              return(new("sparse_numeric", value=numeric(0), pos=integer(0), length=e1@length))
            }
            v1 <- e1@value[match(common_pos, e1@pos)]
            v2 <- e2@value[match(common_pos, e2@pos)]
            new("sparse_numeric", value=v1*v2, pos=as.integer(common_pos), length=e1@length)
          })


setGeneric("sparse_add", function(x, y, ...) standardGeneric("sparse_add"))
setGeneric("sparse_sub", function(x, y, ...) standardGeneric("sparse_sub"))
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))

setMethod("sparse_add", c("sparse_numeric", "sparse_numeric"), function(x, y, ...) x + y)
setMethod("sparse_sub", c("sparse_numeric", "sparse_numeric"), function(x, y, ...) x - y)
setMethod("sparse_mult", c("sparse_numeric", "sparse_numeric"), function(x, y, ...) x * y)
setMethod("sparse_crossprod", c("sparse_numeric", "sparse_numeric"), function(x, y, ...) {
  sum(asNumeric(x) * asNumeric(y))
})


setMethod("show", "sparse_numeric", function(object) {
  cat("An object of class 'sparse_numeric'\n")
  cat("Length:", object@length, "\n")
  cat("Non-zero elements:", length(object@value), "\n")
  if(length(object@value) > 0) {
    cat("Positions:", object@pos, "\n")
    cat("Values   :", object@value, "\n")
  }
})


setMethod("plot", c("sparse_numeric", "sparse_numeric"), function(x, y, ...) {
  common_pos <- union(x@pos, y@pos)
  vx <- numeric(length(common_pos)); names(vx) <- common_pos
  vy <- numeric(length(common_pos)); names(vy) <- common_pos
  vx[as.character(x@pos)] <- x@value
  vy[as.character(y@pos)] <- y@value
  plot(common_pos, vx, type="h", col="blue", ylim=range(c(vx, vy)), xlab="Position", ylab="Value", main="Sparse Numeric Vectors")
  points(common_pos, vy, type="h", col="red")
  legend("topright", legend=c("x","y"), col=c("blue","red"), lty=1)
})


setGeneric("sum_sparse", function(x) standardGeneric("sum_sparse"))
setMethod("sum_sparse", "sparse_numeric", function(x) sum(x@value))

