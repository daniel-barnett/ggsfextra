#' @export
geom_sftext <- function(mapping = aes(), data = NULL, stat = "sf",
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, ...) {

  # Automatically determin name of geometry column
  ## if (!is.null(data) && is_sf(data)) {
    ## geometry_col <- attr(data, "sf_column")
  ## } else {
    ## geometry_col <- "geometry"
  ## }
  ## if (is.null(mapping$geometry)) {
    ## mapping$geometry <- as.name(geometry_col)
  ## }

    mapping$geometry <- as.name("geometry")

  c(
    layer(
      geom = GeomSfText,
      data = data,
      mapping = mapping,
      stat = stat,
      position = position,
      show.legend = FALSE,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        ...
      )
    ),
    coord_sf(default = TRUE)
  )
}

#' @export
scale_type.sfc <- function(x) "identity"

GeomSfText <- ggproto("GeomSfText", Geom,
  required_aes = c("geometry", "label"),

  default_aes = aes(
    colour = "black", size = 3.88, angle = 0, hjust = 0.5,
    vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2
  ),

  draw_panel = function(data, panel_params, coord, parse = FALSE,
                        na.rm = FALSE, check_overlap = FALSE) {
    lab <- data$label
    if (is.numeric(lab)) {
        lab <- round(lab, 2)
    }

    if (parse) {
      lab <- parse(text = as.character(lab))
    }

    data <- coord$transform(data, panel_params)
    if (is.character(data$vjust)) {
      data$vjust <- compute_just(data$vjust, data$y)
    }

    if (is.character(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$x)
    }

    text.coords <- sf::st_coordinates(data$geometry)

    col.bright <- apply(col2rgb(data$colour), 2,
                        function(x) sqrt(0.241 * x[1]^2 + 0.691*x[2]^2 + 0.068*x[3]^2))
    data$colour <- ifelse(col.bright < 130, "gray90", "gray10")
    grid::textGrob(lab, text.coords[, "X"], text.coords[, "Y"], default.units = "native",
                 hjust = data$hjust, vjust = data$vjust,
                 rot = data$angle,
                 gp = grid::gpar(
                     col = alpha(data$colour, data$alpha),
                     fontsize = data$size * .pt,
                     fontfamily = data$family,
                     fontface = data$fontface,
                     lineheight = data$lineheight
                 ),
                 check.overlap = check_overlap)
  },

  draw_key = draw_key_text
)
