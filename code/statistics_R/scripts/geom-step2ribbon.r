# Adapted from the \code{RmcdrPlugin.KMggplot2} (slightly modified)

#' Step ribbon plots.
#'
#' \code{geom_step2ribbon} is an extension of the \code{geom_ribbon}, and
#' is optimized for Kaplan-Meier plots with pointwise confidence intervals
#' or a confidence band.
#'
#' @seealso
#'   \code{\link[ggplot2]{geom_ribbon}} \code{geom_step2ribbon}
#'   inherits from \code{geom_ribbon}.
#' @inheritParams ggplot2:::geom_ribbon
#' @examples
#' library(ggplot2)
#' huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
#' h <- ggplot(huron, aes(year))
#' h + geom_step2ribbon(aes(ymin = level - 1, ymax = level + 1), fill = "grey70") +
#'     geom_step(aes(y = level))
#' h + geom_ribbon(aes(ymin = level - 1, ymax = level + 1), fill = "grey70") +
#'     geom_line(aes(y = level))
#' @rdname geom_step2ribbon
#' @importFrom ggplot2 layer GeomRibbon
#' @export
geom_step2ribbon <- function(
  mapping     = NULL,
  data        = NULL,
  stat        = "identity",
  position    = "identity",
  direction   = "hv",
  na.rm       = FALSE,
  show.legend = NA,
  inherit.aes = TRUE, ...) {
  
  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomStep2ribbon,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(na.rm = na.rm, direction = direction, ... )
  )
  
}

#' @rdname geom_step2ribbon
#' @format NULL
#' @usage NULL
#' @export

GeomStep2ribbon <- ggproto(
  "Geomstep2ribbon", GeomRibbon,
  
  extra_params = c("direction", "na.rm"),
  
  draw_group = function(data, panel_scales, coord, na.rm = FALSE,  direction = "hv") {
    if (na.rm) data <- data[complete.cases(data[c("x", "ymin", "ymax")]), ]
    data <- ggplot2:::dapply(data, "group", stairstep, direction = direction)
    
    data   <- rbind(data, data)
    data   <- data[order(data$x), ]
    #data$x <- c(data$x[2:nrow(data)], NA)
    data   <- data[complete.cases(data["x"]), ]
    GeomRibbon$draw_group(data, panel_scales, coord, na.rm = FALSE)
    
  }
  
)


stairstep <- function(data, direction = "hv") {
  direction <- match.arg(direction, c("hv", "vh", "mid"))
  data <- as.data.frame(data)[order(data$x), ]
  n <- nrow(data)

    if (n <= 1) {
    # Need at least one observation
    return(data[0, , drop = FALSE])
  }
  
  if (direction == "vh") {
    xs <- rep(1:n, each = 2)[-2*n]
    ys <- c(1, rep(2:n, each = 2))
  } else if (direction == "hv") {
    ys <- rep(1:n, each = 2)[-2*n]
    xs <- c(1, rep(2:n, each = 2))
  } else if (direction == "mid") {
    xs <- rep(1:(n-1), each = 2)
    ys <- rep(1:n, each = 2)
  } else {
    abort("Parameter `direction` is invalid.")
  }
 
  if (direction == "mid") {
    gaps <- data$x[-1] - data$x[-n]
    mid_x <- data$x[-n] + gaps/2 # map the mid-point between adjacent x-values
    x <- c(data$x[1], mid_x[xs], data$x[n])
    ymin <- c(data$ymin[ys])
    ymax <- c(data$ymax[ys])
    data_attr <- data[c(1,xs,n), setdiff(names(data), c("x", "ymin", "ymax"))]
  } else {
    x <- data$x[xs]
    ymin <- data$ymin[ys]
    ymax <- data$ymax[ys]
    data_attr <- data[xs, setdiff(names(data), c("x", "ymin", "ymax"))]
  }

    ggplot2:::new_data_frame(c(list(x = x, ymin = ymin, ymax = ymax), data_attr))
}



library(ggplot2)
huron <- data.frame(year = 1968:1972, level = as.vector(LakeHuron)[1:5])
direction <- 'mid'
ggplot(huron, aes(year)) + 
  geom_step2ribbon(aes(ymin = level - 1, ymax = level + 1), fill = "grey70", direction = direction) +
   geom_step(aes(y = level), direction = direction) + theme_minimal()

