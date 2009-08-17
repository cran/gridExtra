\name{ngonGrob}
\alias{ngonGrob}
\title{ngonGrob}
\usage{ngonGrob(x, y, sides=5, size=1, angle=rep(pi/2, length(x)), ar=rep(1,
    length(x)), gp=gpar(colour = "grey50", fill = "grey90", linejoin =
    "mitre"), units.def="native", units.size="mm")
}
\description{regular polygon grob}
\alias{ngonGrob}
\alias{grid.ngon}
\value{grob}
\arguments{\item{x}{x unit}
\item{y}{y unit}
\item{sides}{sides}
\item{size}{size}
\item{angle}{angle}
\item{ar}{ar}
\item{units.def}{units.def}
\item{units.size}{units.size}
\item{gp}{gpar}
}
\examples{pushViewport(dataViewport(0:1, 0:1, width=unit(2, "cm"), height=unit(2, "cm")))

xy <- polygon.regular(6, TRUE)
grid.ngon(0.5, 0.5, 6, 10, units.size="mm")
for(ii in 1:NROW(xy)){
grid.ngon(xy[ii, 1]+0.5, xy[ii, 2]+0.5, 6, 10, units.size="mm")
}
upViewport()}

