\name{pixmapGrob}
\alias{pixmapGrob}
\title{create a grob from pixmap object...}
\usage{pixmapGrob(pic, x=0.5, y=0.5, scale=1, raster=FALSE,
    angle=0, vp = NULL, ...)}

\description{
  create a grob from pixmap object
}
\value{a gTree of class 'pixmap', with natural width and height in points}
\arguments{
  \item{pic}{pixmap object}
  \item{x}{x unit}
  \item{y}{y unit}
  \item{scale}{scale}
  \item{raster}{raster}
  \item{angle}{angle}
  \item{vp}{viewport}
  \item{...}{optional grob parameters,  passed to imageGrob or rasterGrob}
}
\examples{library(pixmap)
library(RGraphics)
x <- read.pnm(system.file("pictures/logo.ppm", package="pixmap")[1])
g1 <- pixmapGrob(x)
dev.new(width=g1$width/72, height=g1$height/72) 
grid.draw(g1)}
