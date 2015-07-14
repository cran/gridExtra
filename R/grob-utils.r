latticeGrob <- function(p, ...){
  grob(p=p, ..., cl="lattice")
}


#' @importFrom graphics plot
#' @export
drawDetails.lattice <- function(x, recording=FALSE){
  stopifnot(requireNamespace("lattice", quietly = TRUE)) 
  plot(x$p, newpage=FALSE)
}


row_heights <- function(m){
  do.call(unit.c, apply(m, 1, function(l)
    max(do.call(unit.c, lapply(l, grobHeight)))))
}

col_widths <- function(m){
  do.call(unit.c, apply(m, 2, function(l)
    max(do.call(unit.c, lapply(l, grobWidth)))))
}


insert.unit <- function (x, values, after = length(x)) {
  lengx <- length(x)
  if (lengx == 0) return(values)
  if (length(values) == 0) return(x)
  
  if (after <= 0) {
    unit.c(values, x)
  } else if (after >= lengx) {
    unit.c(x, values)
  } else {
    unit.c(x[1L:after], values, x[(after + 1L):lengx])
  }
}

z_normalise <- function (x, i = 1) 
{
  x$layout$z <- rank(x$layout$z, ties.method = "first") + i - 
    1
  x
}

z_arrange_gtables <- function (gtables, z) 
{
  if (length(gtables) != length(z)) {
    stop("'gtables' and 'z' must be the same length")
  }
  zmax <- 0
  for (i in order(z)) {
    if (nrow(gtables[[i]]$layout) > 0) {
      gtables[[i]] <- z_normalise(gtables[[i]], zmax + 
                                    1)
      zmax <- max(gtables[[i]]$layout$z)
    }
  }
  gtables
}

rbind.gtable <- function(..., size = "max", z = NULL) {
  gtables <- list(...)
  if (!is.null(z)) {
    gtables <- z_arrange_gtables(gtables, z)
  }
  Reduce(function(x, y) rbind_gtable(x, y, size = size), gtables)
}

rbind_gtable <- function(x, y, size = "max") {
  stopifnot(ncol(x) == ncol(y))
  if (nrow(x) == 0) return(y)
  if (nrow(y) == 0) return(x)
  
  y$layout$t <- y$layout$t + nrow(x)
  y$layout$b <- y$layout$b + nrow(x)
  x$layout <- rbind(x$layout, y$layout)
  
  x$heights <- insert.unit(x$heights, y$heights)
  x$rownames <- c(x$rownames, y$rownames)
  
  size <- match.arg(size, c("first", "last", 
                            "max", "min"))
  x$widths <- switch(size,
                     first = x$widths,
                     last = y$widths,
                     min = unit.pmin(x$widths, y$widths),
                     max = unit.pmax(x$widths, y$widths)
  )
  
  x$grobs <- append(x$grobs, y$grobs)
  
  x
}

cbind.gtable <- function(..., size = "max", z = NULL) {
  gtables <- list(...)
  if (!is.null(z)) {
    gtables <- z_arrange_gtables(gtables, z)
  }
  Reduce(function(x, y) cbind_gtable(x, y, size = size), gtables)
}

cbind_gtable <- function(x, y, size = "max") {
  stopifnot(nrow(x) == nrow(y))
  if (ncol(x) == 0) return(y)
  if (ncol(y) == 0) return(x)
  
  y$layout$l <- y$layout$l + ncol(x)
  y$layout$r <- y$layout$r + ncol(x)
  x$layout <- rbind(x$layout, y$layout)
  
  x$widths <- insert.unit(x$widths, y$widths)
  x$colnames <- c(x$colnames, y$colnames)
  
  size <- match.arg(size, c("first", "last", 
                            "max", "min"))
  
  x$heights <- switch(size,
                      first = x$heights,
                      last = y$heights,
                      min = unit.pmin(x$heights, y$heights),
                      max = unit.pmax(x$heights, y$heights))
  
  x$grobs <- append(x$grobs, y$grobs)
  
  x
}
