##' arrange ggplot2, lattice, and grobs on a page
##'
##' @aliases grid.arrange arrangeGrob latticeGrob drawDetails.lattice print.arrange
##' @title arrangeGrob
##' @param ...  plots of class ggplot2,  trellis, or grobs, and valid arguments to grid.layout
##' @param main string, or grob (requires a well-defined height, see example)
##' @param sub string, or grob (requires a well-defined height, see example)
##' @param legend string, or grob (requires a well-defined width, see example)
##' @param left string, or grob (requires a well-defined width, see example)
##' @param as.table logical: bottom-left to top-right or top-left to bottom-right
##' @param clip logical: clip every object to its viewport
##' @return return a frame grob; side-effect (plotting) if plot=T
##' @seealso \code{grid.layout}
##' 
##' @examples
##' \dontrun{
##' require(ggplot2)
##' plots = lapply(1:5, function(.x) qplot(1:10,rnorm(10),main=paste("plot",.x)))
##' require(gridExtra)
##' do.call(grid.arrange,  plots)
##' require(lattice)
##' grid.arrange(qplot(1:10), xyplot(1:10~1:10), tableGrob(head(iris)), nrow=2, as.table=TRUE, main="test main", sub=textGrob("test sub", gp=gpar(font=2)))
##' 
##' ## adding a common legend
##' library(ggplot2)
##' dsamp <- diamonds[sample(nrow(diamonds), 1000), ] 
##' 
##' p1 <- qplot(carat, price, data=dsamp, colour=clarity)
##' p2 <- qplot(carat, price, data=dsamp, colour=clarity, geom="path")
##' 
##' leg <- ggplotGrob(p1 + opts(keep="legend_box"))
##' ## one needs to provide the legend with a well-defined width
##' legend=gTree(children=gList(leg), cl="legendGrob")
##' widthDetails.legendGrob <- function(x) unit(2, "cm")
##' 
##' grid.arrange(p1 + opts(legend.position="none"),
##'         p2 + opts(legend.position="none"),
##'         legend=legend,
##'         main ="this is a title",
##'         left = "This is my global Y-axis title")
##' 
##' }
##' ## split figures into multiple pages with 2x2 layout
##' 
##'  plots = llply(1:12, function(.x) qplot(1:10,rnorm(10), main=paste("plot",.x)))
##'  
##'  params <- list(nrow=1, ncol=2)
##'  
##'  n <- do.call(prod, params)
##'  pages <- length(plots) %/% n
##'
##'  groups <- split(seq_along(plots), gl(pages + as.logical(length(plots) %% n), n, length(plots)))
##'
##'  print(groups)
##'  for (g in groups){
##'    dev.new()
##'    do.call(grid.arrange, c(plots[g], params))
##'  }


arrangeGrob <- function(..., as.table=FALSE, clip=TRUE,
                    main=NULL, sub=NULL, left=NULL, legend=NULL) {

  
  if(is.null(main)) main <- nullGrob()
  if(is.null(sub)) sub <- nullGrob()
  if(is.null(legend)) legend <- nullGrob()
  if(is.null(left)) left <- nullGrob()
  
  if(is.character(main)) main <- textGrob(main)
  if(is.character(sub)) sub <- textGrob(sub)
  if(is.character(legend)) legend <- textGrob(legend, rot=-90)
  if(is.character(left)) left <- textGrob(left, rot=90)

  arrange.class <- "arrange" # grob class
  
  dots <- list(...)
  
  params <- c("nrow", "ncol", "widths", "heights",
              "default.units", "respect", "just" )
 ## names(formals(grid.layout))
  layout.call <- intersect(names(dots), params)
  params.layout <- dots[layout.call]
  if(is.null(names(dots)))
    not.grobnames <- FALSE else
  not.grobnames <- names(dots) %in% layout.call
  
  grobs <- dots[! not.grobnames ]
  
  n <- length(grobs)
  
  nm <- n2mfrow(n)
  
  if(is.null(params.layout$nrow) & is.null(params.layout$ncol)) 
    {
      params.layout$nrow = nm[1]
     params.layout$ncol = nm[2]
    }
  if(is.null(params.layout$nrow))
    params.layout$nrow = ceiling(n/params.layout$ncol)
  if(is.null(params.layout$ncol))
    params.layout$ncol = ceiling(n/params.layout$nrow)
  
  nrow <- params.layout$nrow 
  ncol <- params.layout$ncol
  
  
  params.layout$nrow <- params.layout$nrow 
  params.layout$ncol <- params.layout$ncol 
  
  
  lay <- do.call(grid.layout, params.layout)
  
  fg <- frameGrob(layout=lay)

  ## if a ggplot is present, make the grob derive from the ggplot class
  classes <- lapply(grobs, class)
  inherit.ggplot <- any("ggplot" %in% unlist(classes))
  if(inherit.ggplot) arrange.class <- c(arrange.class, "ggplot")
  
  ii.p <- 1
  for(ii.row in seq(1, nrow)){
    ii.table.row <- ii.row 
    if(as.table) {ii.table.row <- nrow - ii.table.row + 1}
    for(ii.col in seq(1, ncol)){
      ii.table <- ii.p
      if(ii.p > n) break
      
      ##  select current grob
      cl <- class(grobs[[ii.table]])
      ct <- if("grob" %in% cl) "grob" else cl
      g.tmp <- switch(ct,
                      ggplot = ggplotGrob(grobs[[ii.table]]),
##                       trellis = grid.grabExpr(print(grobs[[ii.table]])),
                      trellis = latticeGrob(grobs[[ii.table]]),
                      grob = grobs[[ii.table]], 
                      stop("input must be grobs!"))
      
      if(clip) # gTree seems like overkill here ?
        g.tmp <- gTree(children=gList(clipGrob(), g.tmp))
      
      fg <- placeGrob(fg, g.tmp, row=ii.table.row, col=ii.col)
      ii.p <- ii.p + 1
    }
  }

  ## optional annotations in a frame grob
  wl <- unit(1, "grobwidth", left) 
  wr <- unit(1, "grobwidth", legend)
  hb <- unit(1, "grobheight", sub)
  ht <- unit(1, "grobheight", main)
  
  annotate.lay <- grid.layout(3, 3,
                              widths=unit.c(wl, unit(1, "npc")-wl-wr, wr),
                              heights=unit.c(ht, unit(1, "npc")-hb-ht, hb))
  
  af <- frameGrob(layout=annotate.lay)
  
  af <- placeGrob(af, fg, row=2, col=2)
  af <- placeGrob(af, main, row=1, col=2)
  af <- placeGrob(af, sub, row=3, col=2)
  af <- placeGrob(af, left, row=2, col=1)
  af <- placeGrob(af, legend, row=2, col=3)
  
 
  invisible(gTree(children=gList(af), cl=arrange.class))
}

grid.arrange <- function(..., as.table=FALSE, clip=TRUE,
                    main=NULL, sub=NULL, left=NULL, legend=NULL,
					newpage=TRUE){
    if(newpage) grid.newpage()
    grid.draw(arrangeGrob(...,as.table=as.table, clip=clip,
	                    main=main, sub=sub, left=left, legend=legend))
}

latticeGrob <- function(p, ...){
 grob(p=p, ..., cl="lattice")
}

drawDetails.lattice <- function(x, recording=FALSE){
  lattice:::plot.trellis(x$p, newpage=FALSE)
}


print.arrange <- function(x, newpage = is.null(vp), vp = NULL, ...) {
  if(newpage) grid.newpage()
  grid.draw(editGrob(x, vp=vp))
}
