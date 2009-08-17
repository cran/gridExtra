\name{interleaven}
\title{interleaven}
\usage{interleaven(x=letters[1:3], y=1:12, n=4)}

\description{
  mix two vectors
}
\alias{interleaven}
\value{a vector}
\seealso{\code{rep}, \code{ggplot2:interleave}}
\arguments{
  \item{x}{vector}
  \item{y}{vector}
  \item{n}{integer}
}
\examples{interleaven(replicate(3, rectGrob(), simplify=FALSE), replicate(12, virtualGrob, simplify=FALSE), 4)}
