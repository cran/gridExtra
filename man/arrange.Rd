\name{arrange}
\alias{arrange}
\title{arrange}
\usage{arrange(..., as.table=FALSE, plot=TRUE, clip=TRUE, main, sub, left, legend,
    newpage=FALSE)
}
\description{arrange ggplot2, lattice, and grobs on a page}
\alias{arrange}
\alias{latticeGrob}
\alias{drawDetails.lattice}
\value{return a frame grob; side-effect (plotting) if plot=T}
\seealso{\code{grid.layout}}
\arguments{\item{...}{plots of class ggplot2,  trellis, or grobs, and valid arguments to grid.layout}
\item{main}{string, or grob (requires a well-defined height, see example)}
\item{sub}{string, or grob (requires a well-defined height, see example)}
\item{legend}{string, or grob (requires a well-defined width, see example)}
\item{left}{string, or grob (requires a well-defined width, see example)}
\item{plot}{logical plot or not}
\item{as.table}{logical: bottom-left to top-right or top-left to bottom-right}
\item{clip}{logical: clip every object to its viewport}
\item{newpage}{logical}
}
\examples{\dontrun{
require(ggplot2)
plots = lapply(1:5, function(.x) qplot(1:10,rnorm(10),main=paste("plot",.x)))
do.call(arrange,  plots)
require(gridExtra)
require(lattice)
arrange(qplot(1:10), xyplot(1:10~1:10), tableGrob(head(iris)), nrow=2, as.table=TRUE, main="test main", sub=textGrob("test sub", gp=gpar(font=2)))

## adding a common legend
library(ggplot2)
dsamp <- diamonds[sample(nrow(diamonds), 1000), ] 

p1 <- qplot(carat, price, data=dsamp, colour=clarity)
p2 <- qplot(carat, price, data=dsamp, colour=clarity, geom="path")

leg <- ggplotGrob(p1 + opts(keep="legend_box"))
## one needs to provide the legend with a well-defined width
legend=gTree(children=gList(leg), cl="legendGrob")
widthDetails.legendGrob <- function(x) unit(2, "cm")

arrange(p1 + opts(legend.position="none"),
p2 + opts(legend.position="none"),
legend=legend,
main ="this is a title",
left = "This is my global Y-axis title")

}}

