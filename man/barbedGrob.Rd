\name{barbedGrob}
\title{barbedGrob}
\usage{barbedGrob(x=stats::runif(10), y=stats::runif(10),
    size=unit(sample(1:4, 10, repl = TRUE), "char"),
    pch=21, space=1, only.lines=FALSE, gp=gpar(), name,
    default.units="npc", vp)}

\description{
  plot lines and symbols
}
\alias{barbedGrob}
\alias{grid.barbed}
\value{a grob}
\seealso{\code{grid.segments}, \code{grid.points}}
\arguments{
  \item{x}{coordinates}
  \item{y}{coordinates}
  \item{size}{unit vector for the symbols}
  \item{pch}{vector of symbol types}
  \item{space}{numeric scaling factor for the exclusion zone (see Details)}
  \item{only.lines}{logical: should only split lines be returned?}
  \item{gp}{gpar() object for the symbols}
  \item{name}{grob name}
  \item{default.units}{default units}
  \item{vp}{viewport}
}
\examples{grid.barbed(name="test")
grid.edit("test", gp=gpar(fill="blue", lwd=3))
grid.edit("test::points", pch=22)
grid.newpage()
g <- 
barbedGrob(size=unit(1:5, "char"), only=FALSE, 
gp=gpar(col="red", lex=3, fill="blue", alpha=0.5, pch=3))

pushViewport(vp=viewport(width=1, height=1))
grid.rect(gp=gpar(fill="thistle2"))
grid.grill(gp=gpar(col="lavenderblush1", lwd=3, lty=3))
grid.draw(g)
x <- c(0.2, 0.7)
y <- x
dev.new(width=3, height=7) 
grid.newpage()
g <- 
barbedGrob(x, y, size=unit(c(2, 10), "mm"))
pushViewport(vp=viewport(width=1, height=1))
grid.draw(g)
grid.points(x, y, pch=3)}
