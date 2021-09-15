#' Repulsive textual annotations.
#'
#' Get repelled label coordinates. Based off of \code{ggrepel}.
#'
#' @section Alignment with \code{hjust} or \code{vjust}:
#' The arguments \code{hjust} and \code{vjust} are supported, but they only
#' control the initial positioning, so repulsive forces may disrupt alignment.
#' Alignment with \code{hjust} will be preserved if labels only move up and down
#' by using \code{direction="y"}. For \code{vjust}, use \code{direction="x"}.
#'
#' @param label_coords A data frame with columns 'x', 'y', and 'label'.
#' @param xrange The range of x coordinates.
#' @param yrange The range of y coordinates.
#' @param position Position adjustment, either as a string, or the result of
#'  a call to a position adjustment function.
#' @param nudge_x,nudge_y Horizontal and vertical adjustments to nudge the
#'   starting position of each text label. The units for \code{nudge_x} and
#'   \code{nudge_y} are the same as for the data units on the x-axis and y-axis.
#' @param box.padding Amount of padding around bounding box, as unit or number.
#'   Defaults to 0.25. (Default unit is lines, but other units can be specified
#'   by passing \code{unit(x, "units")}).
#' @param point.padding Amount of padding around labeled point, as unit or
#'   number. Defaults to 0. (Default unit is lines, but other units can be
#'   specified by passing \code{unit(x, "units")}).
#' @param force Force of repulsion between overlapping text labels. Defaults
#'   to 1.
#' @param force_pull Force of attraction between a text label and its
#'   corresponding data point. Defaults to 1.
#' @param max.time Maximum number of seconds to try to resolve overlaps.
#'   Defaults to 0.5.
#' @param max.iter Maximum number of iterations to try to resolve overlaps.
#'   Defaults to 10000.
#' @param max.overlaps Exclude text labels that overlap too many things.
#'   Defaults to 10.
#' @param direction "both", "x", or "y" -- direction in which to adjust position of labels
#' @param seed Random seed passed to \code{\link[base]{set.seed}}. Defaults to
#'   \code{NA}, which means that \code{set.seed} will not be called.
#' @param verbose If \code{TRUE}, some diagnostics of the repel algorithm are printed
#'
#' @examples
#'
#' plot(mpg ~ wt, data = mtcars)
#' label_coords <- mtcars[, c('wt', 'mpg')]
#' colnames(label_coords) <- c('x', 'y')
#' label_coords$label <- row.names(mtcars)
#' repels <- repel_text(label_coords)
#' segments(label_coords$x, label_coords$y, repels$x, repels$y, col = 'blue', lty=2)
#' text(repels$x, repels$y, labels = repels$label)
#'
#' @export
repel_text <- function(
  label_coords,
  xrange = range(label_coords$x),
  yrange = range(label_coords$y),
  box.padding = 0.25,
  point.padding = 1e-6,
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
  direction = c("both","y","x"),
  seed = NA,
  verbose = FALSE) {

  if (!is.na(seed)) set.seed(seed)

  .pt <- 72.27 / 25.4
  direction <- match.arg(direction)
  box_padding_x <- grid::convertWidth(to_unit(box.padding), "npc", valueOnly = TRUE)
  box_padding_y <- grid::convertHeight(to_unit(box.padding), "npc", valueOnly = TRUE)

  data <- data.frame(
    x = scales::rescale(label_coords$x, c(0, 1), xrange),
    y = scales::rescale(label_coords$y, c(0, 1), yrange),
    label = label_coords$label,
    size = 3.88,
    angle = 0,
    family = '',
    fontface = 1,
    lineheight = 1.2,
    hjust = 0.5,
    vjust = 0.5,
    point.size = point.size,
    nudge_x = nudge_x,
    nudge_y = nudge_y
  )

  # Create a dataframe with x1 y1 x2 y2
  boxes <- lapply(seq_len(nrow(data)), function(i) {
    row <- data[i, , drop = FALSE]
    tg <- grid::textGrob(
      row$label,
      row$x, row$y, default.units = "npc",
      rot = row$angle,
      hjust = row$hjust,
      vjust = row$vjust,
      gp = grid::gpar(
        fontsize   = row$size * .pt,
        fontfamily = row$family,
        fontface   = row$fontface,
        lineheight = row$lineheight
      )
    )
    x1 <- grid::convertWidth(grid::grobX(tg, "west"), "npc", TRUE)
    x2 <- grid::convertWidth(grid::grobX(tg, "east"), "npc", TRUE)
    y1 <- grid::convertHeight(grid::grobY(tg, "south"), "npc", TRUE)
    y2 <- grid::convertHeight(grid::grobY(tg, "north"), "npc", TRUE)
    c(
      "x1" = x1 - box_padding_x + row$nudge_x,
      "y1" = y1 - box_padding_y + row$nudge_y,
      "x2" = x2 + box_padding_x + row$nudge_x,
      "y2" = y2 + box_padding_y + row$nudge_y
    )
  })

  boxes <- do.call(rbind, boxes)


  # Beware the magic numbers. I do not understand them.
  # I just accept them as necessary to get the code to work.
  p_width <- grid::convertWidth(grid::unit(1, "npc"), "inch", TRUE)
  p_height <- grid::convertHeight(grid::unit(1, "npc"), "inch", TRUE)
  p_ratio <- (p_width / p_height)
  if (p_ratio > 1) {
    p_ratio <- p_ratio ^ (1 / (1.15 * p_ratio))
  }
  point_size <- p_ratio * grid::convertWidth(
    to_unit(data$point.size), "npc", valueOnly = TRUE
  ) / 13

  point_padding <- p_ratio * grid::convertWidth(
    to_unit(point.padding), "npc", valueOnly = TRUE
  ) / 13


  repel <- repel_boxes2(
    data_points     = as.matrix(data[, c("x","y")]),
    point_size      = point_size,
    point_padding_x = point_padding,
    point_padding_y = point_padding,
    boxes           = boxes,
    xlim            = c(0, 1),
    ylim            = c(0, 1),
    hjust           = data$hjust,
    vjust           = data$vjust,
    force_push      = force * 1e-6,
    force_pull      = force_pull * 1e-2,
    max_time        = max.time,
    max_iter        = ifelse(is.infinite(max.iter), 1e9, max.iter),
    max_overlaps    = max.overlaps,
    direction       = direction,
    verbose         = verbose
  )

  # scale back to coords
  repel$label <- data$label
  data$x <- scales::rescale(repel$x, xrange, c(0, 1))
  data$y <- scales::rescale(repel$y, yrange, c(0, 1))
  return(data[, c('x', 'y', 'label')])
}
