## > ls(package:gridExtra)
## Error in try(name) : object 'package' not found
##  [1] "arcTextGrob"               "arrange"                 
##  [5] "barbedGrob"                "borderGrob"                 "colorstripGrob"             "ebimageGrob"               "ellipseGrob"                  
##        "latticeGrob"                     "ngonGrob"                 
## [37] "patternGrob"                 "pixmapGrob"                "## polygon.regular"          
## [41] "polygon.star"              "polygon1"                  "polygon2"                  "read.tiff"                         "rpatternGrob"                  "tableGrob"               


## grobs
require(ggplot2)
require(gridExtra)
grep("Grob",ls(package:gridExtra),val=T)

arcTextGrob.example <- function() {
  grid.arcText()
}

barbedGrob.example <- function() {
  
  g <- barbedGrob(size=unit(1:5, "char"), only=FALSE, 
                  gp=gpar(col="red", lex=3, fill="blue", alpha=0.5, pch=3))
  
  pushViewport(vp=viewport(width=1, height=1))
  grid.rect(gp=gpar(fill="thistle2"))
  grid.grill(gp=gpar(col="lavenderblush1", lwd=3, lty=3))
  grid.draw(g)
} 
borderGrob.example <- function() {
  
pushViewport(viewport(width=0.5, height=0.5, layout=grid.layout(4, 4, w=0.9, height=0.9)))

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
}
}

colorstripGrob.example <- function() {
  cols <- c("#4C00F0", "#0046EC", "#00E2BF", "#00FF00", "#00FF00",
            "#92E500", "#FFA100", "#FF3100", "#FF0000")
  g1 <- colorstripGrob(cols, raster=FALSE, direction="horizontal")
  g2 <- colorstripGrob(cols, raster=TRUE, direction="horizontal")
  g3 <- colorstripGrob(cols, raster=FALSE, direction="vertical")
  g4 <- colorstripGrob(cols, raster=TRUE, direction="vertical")
  
  arrange(g1, g2, g3, g4)
}

ebimageGrob.example <- function() {
  library(EBImage)
  library(RGraphics)
  x <- readImage("http://www.google.com/logos/teachersday09.gif")
  g1 <- ebimageGrob(x)
  g2 <- ebimageGrob(x, raster=TRUE)
  arrange(g1, g2)
}

ellipseGrob.example <- function() {
  g = ellipseGrob(1:10/11,1:10/11,size=runif(10, 5, 10),ar=1:5,angle=rnorm(10),
    def="npc", gp=gpar(fill=grey(1:10/11)))
  grid.draw(g)
} 
ngonGrob.example <- function() {
  pushViewport(dataViewport(0:1, 0:1, width=unit(2, "cm"), height=unit(2, "cm")))
  
  xy <- polygon.regular(6, TRUE)
  grid.ngon(0.5, 0.5, 6, 10, units.size="mm")
  for(ii in 1:NROW(xy)){
    grid.ngon(xy[ii, 1]+0.5, xy[ii, 2]+0.5, 6, 10, units.size="mm")
  }
  upViewport()
} 
patternGrob.example <- function() {
  grid.pattern(x=seq(1/6, 5/6, length=6), width=unit(1/8,"npc"), height=unit(0.5,"npc"),
               motif.width=unit(10, "mm"),  pattern=c(1:6), orientation=45, motif.alpha=0.5,
               motif.cex=c(1, 0.5), motif.col=1:2, motif.fill=NA,
               gp=gpar(fill="blue", lwd=2, alpha=0.5),  clip=T)
}

pixmapGrob.example <- function() {
  library(pixmap)
  library(RGraphics)
  x <- read.pnm(system.file("pictures/logo.ppm", package="pixmap")[1])
  g1 <- pixmapGrob(x)
  g2 <- pixmapGrob(x, raster=TRUE)
  arrange(g1, g2)
}

rpatternGrob.example <- function() {
  .lines45 <- matrix("white", ncol=100, nrow=100)
  diag(.lines45) <- "black"
  grid.rpattern(motif=.lines45)
} 
tableGrob.example <- function() {
  e = expression(alpha,"testing large width", integral(f(x)*dx, a, b)) 
  lg <- lapply(c("theme.blank", "theme.default", "theme.white",  "theme.vertical",  "theme.list", "theme.black"),
               function(x) tableGrob(head(iris[, 1:3]), cols=e, theme=get(x)()))
  do.call(arrange, lg)
} 

run <- function(x, width=8, height=width){

  xs <- deparse(substitute(x))
  print(xs)
  png(paste(xs, ".png", sep=""), height=height, width=width, units="in", res=300)
  eval(parse(text=paste( xs, ".example()", sep="")))
  dev.off()

}

set.seed(123)

run(arcTextGrob)
run(barbedGrob) 
run(borderGrob, width=5) 
run(colorstripGrob) 
run(ebimageGrob, width=5) 
run(ellipseGrob) 
run(ngonGrob, width=4) 
run(patternGrob, height=5) 
run(pixmapGrob, width=4) 
run(rpatternGrob, width=3) 
run(tableGrob) 
