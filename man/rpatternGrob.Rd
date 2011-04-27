\name{rpatternGrob}
\title{rpatternGrob}
\usage{rpatternGrob(x=unit(0.5, "npc"), y=unit(0.5, "npc"),
    width=unit(1, "npc"), height=unit(1, "npc"),
    motif=matrix("white"), AR=1, motif.width=unit(5, "mm"),
    motif.height=AR * motif.width, pattern.offset=c(0, 0),
    default.units="npc", clip=TRUE, gp=gpar(fill = NA),
    ...)}

\description{
  rectangular grob with raster fill pattern
}
\alias{rpatternGrob}
\alias{tile.motif}
\alias{widthDetails.rpattern}
\alias{heightDetails.rpattern}
\alias{drawDetails.rpattern}
\alias{grid.rpattern}
\value{grob of class rpattern}
\arguments{
  \item{x}{x unit}
  \item{y}{y unit}
  \item{width}{width}
  \item{height}{height}
  \item{motif}{motif}
  \item{AR}{AR}
  \item{motif.width}{motif.width}
  \item{motif.height}{motif.height}
  \item{pattern.offset}{pattern.offset}
  \item{default.units}{default.units}
  \item{clip}{clip}
  \item{gp}{gp}
  \item{...}{additional params passed to the grob}
}
\examples{.lines45 <- matrix("white", ncol=100, nrow=100)
diag(.lines45) <- "black"
grid.rpattern(motif=.lines45)}
