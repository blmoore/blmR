
theme_blm_null <- function(base_size=12, base_family="sans") {
## Base courtesy of: https://github.com/jrnold/ggthemes
  theme(
    line = element_line(colour = "grey95", size = 0.5,
                        linetype = 1, lineend = "butt"),
    rect = element_rect(fill = "white", colour = "grey90",
                        size = 0.5, linetype = 1),
    text =  element_text(family = base_family, face = "plain",
                         colour = "grey20", size = base_size,
                         hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
    axis.text = element_text(),
    strip.text = element_text(),
    axis.line = element_blank(),
    axis.title = element_text(size=14, face=2),
    axis.title.x = element_text(),
    axis.title.y = element_text(angle=90),
    axis.text = element_text(size=10),
    axis.text.x = element_text(),
    axis.text.y = element_text(),
    axis.ticks = element_line(),
    axis.ticks.x = element_line(),
    axis.ticks.y = element_line(),
    axis.ticks.length =  unit(0.15, "cm"),
    axis.ticks.margin =  unit(0.1, "cm"),
    axis.line = element_line(color="grey80"),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    legend.background = element_rect(colour = "grey95", fill="grey95"),
    legend.margin = unit(0.2, "cm"),
    # legend.key = element_rect(),
    legend.key = element_blank(),
    legend.key.size = unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(),
    legend.text.align = NULL,
    legend.title = element_text(face=2, color="grey25"),
    legend.title.align = .5,
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    ## Must have colour=NA or covers the plot
    # panel.background = element_rect(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    # panel.border = element_rect(fill=NA),
    panel.border = element_blank(), 
    panel.margin = unit(0.25, "lines"),
    panel.grid = element_line(),
    panel.grid.major = element_line(size=.4),
    panel.grid.major.x = element_line(),
    panel.grid.major.y = element_line(),
    panel.grid.minor = element_line(),
    panel.grid.minor.x = element_line(size=.1),
    panel.grid.minor.y = element_line(size=.1),
    ## transparent bg?
    plot.background = element_rect(fill = "transparent", colour = NA),
    plot.title = element_text(color="#ffa775", face=2, size=16),
    plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"),
    strip.background = element_rect(fill="grey90", color="grey90"),
    strip.text = element_text(color="grey25"),
    strip.text.x = element_text(),
    strip.text.y = element_text(angle = -90),
    complete = TRUE)
}

theme_blm_sizes <- function() {
  theme(
    axis.text.y = element_text(hjust = 1),
    strip.text = element_text(size = rel(0.8)),
    axis.text = element_text(size = rel(0.8)),
    axis.text.x = element_text(vjust = 1),
    legend.text = element_text(size = rel(0.8)),
    legend.title = element_text(size = rel(0.8), hjust = 0),
    panel.grid.minor = element_line(size = 0.25),
    plot.title = element_text(size = rel(1.2)),
    strip.text.y = element_text(angle = -90)
  )
}

#' blm custom ggplot2 theme
#'
#' Custom ggplot2 theme with \url{blm.io} colourscheme.
#' Reasonably minimalist but retains grid lines which might
#' be better off stripped depending on data.
#' 
#' @export
#' @examples
#' library("ggplot2")
#' library("grid")
#' 
#' # default theme
#' ggplot(data.frame(x=1:100, y=rnorm(100)), aes(x, y)) + 
#'   geom_point()
#'  
#' #blm
#' ggplot(data.frame(x=1:100, y=rnorm(100)), aes(x, y)) + 
#'   geom_point() + theme_blm()
theme_blm <- function(base_size=12, base_family="", use_sizes=TRUE) {
  thm <- theme_blm_null(base_size=base_size, base_family=base_family)
  if (use_sizes) {
    thm + theme_blm_sizes()
  }
  thm
}


