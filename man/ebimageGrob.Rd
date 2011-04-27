\name{ebimageGrob}
\alias{ebimageGrob}
\title{create a grob from EBImage object...}
\usage{ebimageGrob(pic, x=0.5, y=0.5, scale=1, raster=FALSE,
    angle, ...)}

\description{
  create a grob from EBImage object
}
\value{a gTree of class 'ebimage', with natural width and height in points}
\arguments{
  \item{pic}{object of class Image}
  \item{x}{x unit}
  \item{y}{y unit}
  \item{scale}{numeric scale factor}
  \item{raster}{logical: use rasterGrob(R>=2.11) or imageGrob(RGraphics)}
  \item{angle}{numeric: angle in degrees}
  \item{...}{optional grob parameters,  passed to imageGrob or rasterGrob}
}
\examples{\dontrun{
library(EBImage)
library(RGraphics)
x <- readImage("http://www.google.com/logos/teachersday09.gif")
g1 <- ebimageGrob(x)
dev.new(width=g1$width/72, height=g1$height/72) 
grid.draw(g1) 
}}
