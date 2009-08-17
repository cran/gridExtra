\name{tableGrob}
\alias{tableGrob}
\title{tableGrob}
\usage{tableGrob(d, rows=rownames(d), cols=colnames(d), parse=FALSE, show.rownames=TRUE,
    show.colnames=TRUE, row.just="center", col.just="center",
    core.just="center", separator="white", show.box=FALSE,
    show.csep=FALSE, show.rsep=FALSE, equal.width=FALSE,
    equal.height=FALSE, padding.h=unit(4, "mm"), padding.v=unit(4,
    "mm"), gp, gpar.coretext=gpar(col = "black", cex = 1),
    gpar.coltext=gpar(col = "black", cex = 1, fontface = "bold"),
    gpar.rowtext=gpar(col = "black", cex = 0.8, fontface = "italic"),
    h.odd.alpha=1, h.even.alpha=1, v.odd.alpha=1, v.even.alpha=1,
    gpar.corefill=gpar(fill = "grey95", col = "white"),
    gpar.rowfill=gpar(fill = "grey90", col = "white"),
    gpar.colfill=gpar(fill = "grey90", col = "white"), theme, ...)
}
\description{create a list of text and fill grobs and calculates the sizes for a table display}
\alias{tableGrob}
\alias{grid.table}
\alias{drawDetails.table}
\alias{widthDetails.table}
\alias{heightDetails.table}
\alias{arrangeTableGrobs}
\alias{makeTableGrobs}
\value{a grob of class table}
\arguments{\item{d}{data.frame}
\item{rows}{vector of row names}
\item{cols}{vector of col names}
\item{parse}{logical, parse labels as expressions}
\item{row.just}{justification of labels}
\item{col.just}{justification of labels}
\item{core.just}{justification of labels}
\item{separator}{colour of the border lines}
\item{show.box}{logical}
\item{show.csep}{logical separator for colnames}
\item{show.rsep}{logical separator for rownames}
\item{equal.width}{logical}
\item{equal.height}{logical}
\item{padding.h}{unit of horizontal margin,  per cell}
\item{padding.v}{unit of vertical margin,  per cell}
\item{gpar.coretext}{gpar() for inner text}
\item{gpar.corefill}{gpar() for inner fill}
\item{gpar.coltext}{gpar() for colnames text}
\item{h.odd.alpha}{h.odd.alpha}
\item{h.even.alpha}{h.even.alpha}
\item{v.odd.alpha}{v.odd.alpha}
\item{v.even.alpha}{v.even.alpha}
\item{gpar.colfill}{gpar() for colnames fill}
\item{gpar.rowtext}{gpar() for rownames text}
\item{gpar.rowfill}{gpar() for rownames fill}
\item{show.rownames}{logical}
\item{show.colnames}{logical}
\item{gp}{gpar}
\item{theme}{theme (list of aesthetic elements)}
\item{...}{passed to grob}
}
\examples{grid.table(head(iris), h.even.alpha=1, h.odd.alpha=1,  v.even.alpha=0.5, v.odd.alpha=1)
grid.newpage()
grid.draw(tableGrob(head(iris, 10), name="test"))
e = expression(alpha,"testing very large width", hat(beta), integral(f(x)*dx, a, b), "abc") 
grid.edit("test", cols=e, show.rownames=FALSE, rows=NULL,
gpar.corefill = gpar(fill="white", col=NA),
grep=TRUE, global=TRUE)
grid.newpage()
grid.draw(tableGrob(head(iris, 10),
show.csep=TRUE, show.rsep=TRUE, show.box=TRUE, separator="grey", name="test"))
grid.edit("test",gp=gpar(fontsize=8, lwd=2),  equal.width=TRUE, grep=TRUE, global=TRUE)
# visualize themes
lg <- lapply(c("theme.blank", "theme.default", "theme.white",  "theme.vertical",  "theme.list", "theme.black"),
function(x) tableGrob(head(iris[, 1:3]), theme=get(x)()))
grid.newpage()
do.call(grid.arrange, lg)
\dontrun{
## timing: a bit slow due to repeated on-the-fly calculations 
pdf("test2.pdf", height=50)
print(system.time( grid.table(iris)) ) # about 12s here
dev.off()
}}

