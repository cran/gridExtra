\name{colorstripGrob}
\title{colorstrip}
\usage{colorstripGrob(fill=1:3, colour=fill, draw=TRUE,
    raster=FALSE, direction=c("vertical", "horizontal"))}

\description{
  strip of colors
}
\alias{colorstripGrob}
\alias{grid.colorstrip}
\value{a grob}
\seealso{\code{grid.rect}}
\arguments{
  \item{fill}{vector of colours}
  \item{colour}{vector of colours}
  \item{draw}{logical}
  \item{direction}{direction (horizontal or vertical)}
  \item{raster}{logical,  use grid.raster (with interpolation)}
}
\examples{colorstripGrob()
\dontrun{
require(RColorBrewer)
colorstripGrob(brewer.pal(3, "Set1"))
}}
