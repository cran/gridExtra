\name{borderGrob}
\alias{borderGrob}
\title{borderGrob}
\usage{borderGrob(type=1, colour="white", vp, ...)
}
\description{an open rectangular borderdraw}
\alias{borderGrob}
\alias{grid.border}
\value{a grob}
\seealso{\code{grid.segments}, \code{grid.points}}
\arguments{\item{type}{which borders to draw}
\item{colour}{colour}
\item{...}{additional arguments passed to gpar()}
\item{vp}{viewport}
}
\examples{pushViewport(viewport(width=0.5, height=0.5, layout=grid.layout(4, 4, w=0.9, height=0.9)))
vp = viewport(width=0.9, height=0.9)
type <- 1
for(ii in 1:4){
for(jj in 1:4){
pushViewport(viewport(layout.pos.r=ii, layout.pos.c=jj))
grid.rect(gp=gpar(col="grey",fill="black"))
grid.text(paste("t = ", type), gp=gpar(col="white"))
grid.border(type, vp=vp)
upViewport()
type <- type + 1
}
}}

