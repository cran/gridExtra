\name{arcTextGrob}
\title{place text labels on a circle and display relations with arcs...}
\usage{arcTextGrob(x=unit(0.5, "npc"), y=unit(0.5, "npc"),
    labels=library()$results[, 1],
    links=sample(seq_along(labels), 50, rep = TRUE),
    default.units="npc", gp=gpar(), ...)}

\description{
  place text labels on a circle and display relations with
  arcs
}
\alias{arcTextGrob}
\alias{grid.arcText}
\value{grob}
\arguments{
  \item{x}{x unit}
  \item{y}{y unit}
  \item{labels}{text labels}
  \item{links}{integer vector}
  \item{default.units}{default units}
  \item{gp}{gpar}
  \item{...}{additional params (unused)}
}
\examples{library(grid)
set.seed(1234)
grid.newpage()
grid.arcText()}
