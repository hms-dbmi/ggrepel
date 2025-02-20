% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/repel_text.R
\name{repel_text}
\alias{repel_text}
\title{Repulsive textual annotations.}
\usage{
repel_text(
  label_coords,
  xrange = range(label_coords$x),
  yrange = range(label_coords$y),
  xaxs = 0.04,
  yaxs = 0.04,
  mar = par("mar"),
  fontsize = 12,
  box.padding = 0.25,
  point.padding = 1e-06,
  point.size = 1,
  force = 1,
  force_pull = 1,
  max.time = 0.5,
  max.iter = 10000,
  max.overlaps = 10,
  nudge_x = 0,
  nudge_y = 0,
  na.rm = FALSE,
  show.legend = NA,
  direction = c("both", "y", "x"),
  seed = NA,
  verbose = FALSE
)
}
\arguments{
\item{label_coords}{A data frame with columns 'x', 'y', and 'label'.}

\item{xrange}{The range of x coordinates.}

\item{yrange}{The range of y coordinates.}

\item{xaxs}{the fraction to extend \code{xrange} on either side. Default is 0.04.
See \code{?par} parameter \code{xaxs}.}

\item{yaxs}{the fraction to extend \code{yrange} on either side. Default is 0.04.
See \code{?par} parameter \code{yaxs}.}

\item{mar}{Margins in order bottom, left, top, right in units of lines. Default is \code{par('mar')}.}

\item{fontsize}{Font size in points. Default is 12.}

\item{box.padding}{Amount of padding around bounding box, as unit or number.
Defaults to 0.25. (Default unit is lines, but other units can be specified
by passing \code{unit(x, "units")}).}

\item{point.padding}{Amount of padding around labeled point, as unit or
number. Defaults to 0. (Default unit is lines, but other units can be
specified by passing \code{unit(x, "units")}).}

\item{force}{Force of repulsion between overlapping text labels. Defaults
to 1.}

\item{force_pull}{Force of attraction between a text label and its
corresponding data point. Defaults to 1.}

\item{max.time}{Maximum number of seconds to try to resolve overlaps.
Defaults to 0.5.}

\item{max.iter}{Maximum number of iterations to try to resolve overlaps.
Defaults to 10000.}

\item{max.overlaps}{Exclude text labels that overlap too many things.
Defaults to 10.}

\item{nudge_x, nudge_y}{Horizontal and vertical adjustments to nudge the
starting position of each text label. The units for \code{nudge_x} and
\code{nudge_y} are the same as for the data units on the x-axis and y-axis.}

\item{direction}{"both", "x", or "y" -- direction in which to adjust position of labels}

\item{seed}{Random seed passed to \code{\link[base]{set.seed}}. Defaults to
\code{NA}, which means that \code{set.seed} will not be called.}

\item{verbose}{If \code{TRUE}, some diagnostics of the repel algorithm are printed}

\item{position}{Position adjustment, either as a string, or the result of
a call to a position adjustment function.}
}
\description{
Get repelled label coordinates. Based off of \code{ggrepel}.
}
\section{Alignment with \code{hjust} or \code{vjust}}{

The arguments \code{hjust} and \code{vjust} are supported, but they only
control the initial positioning, so repulsive forces may disrupt alignment.
Alignment with \code{hjust} will be preserved if labels only move up and down
by using \code{direction="y"}. For \code{vjust}, use \code{direction="x"}.
}

\examples{

plot(mpg ~ wt, data = mtcars)
label_coords <- mtcars[, c('wt', 'mpg')]
colnames(label_coords) <- c('x', 'y')
label_coords$label <- row.names(mtcars)
repels <- repel_text(label_coords)
segments(label_coords$x, label_coords$y, repels$x, repels$y, col = 'blue', lty=2)
text(repels$x, repels$y, labels = repels$label)

}
