
##' Plots a minimalist time series line chart at each coordinate
##' provided
##'
##' .. content for \details{} ..
##' @title Sparkline layer for each map point
##' @param mapping 
##' @param data 
##' @param stat 
##' @param position 
##' @param na.rm 
##' @param show.legend 
##' @param inherit.aes 
##' @param ... 
##' @return 
##' @author 
##' @export
geom_categorical <- function(mapping = aes(), data = NULL, stat = "sf",
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, ...) {
    mapping$geometry <- as.name("geometry")

    # Some extra stuff to make NAs transparent while not obscuring them
    ## cat_val_var <- as.character(mapping$cat_val)
    ## data_col <- as.data.frame(data)[, cat_val_var]
    ## levels(data_col) <- c(levels(data_col), "NA")
    ## alpha.vals <- ifelse(is.na(data_col), 0, 1)
    ## data_col[is.na(data_col)] <- "NA"
    ## data[, cat_val_var] <- data_col

  c(
    ggplot2::layer(
      geom = GeomCategorical,
      data = data,
      mapping = mapping,
      stat = stat,
      position = position,
      show.legend = if (is.character(show.legend)) TRUE else show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        legend = if (is.character(show.legend)) show.legend else "polygon",
        ## alpha = alpha.vals,
        ...
      )
    ),
    coord_sf(default = TRUE)
  )
}

GeomCategorical <- ggproto("GeomSf", Geom,
                          required_aes = c("geometry",
                                           "cat_x", # THIS HAS TO BE LINE_X...
                                           "cat_val",
                                           "group"),
                          default_aes = aes(
                              colour = NULL,
                              fill = NULL,
                              alpha = 1
                          ),
                          
                          draw_panel = function(data, panel_params, coord, legend = NULL) {
                              if (!inherits(coord, "CoordSf")) {
                                  stop("geom_sf() must be used with coord_sf()", call. = FALSE)
                              }

                              ## print(data)
                              
                                        # Need to refactor this to generate one grob per geometry type
                              coord <- coord$transform(data, panel_params)

                              ## Scale all lines to have the same ranges
                              x.rng <- range(coord$cat_x)
                              ## y.rng <- range(coord$cat_val)
                              coord$cat_x <- (coord$cat_x - x.rng[1]) / (x.rng[2] - x.rng[1])
                              ## coord$cat_val <- (coord$cat_val - y.rng[1]) / (y.rng[2] - y.rng[1])
                              ## print(coord)
                              
                              ## Size of each sparkline's viewport (in native)
                              vp.width <- 0.05
                              vp.height <- 0.025

                              ## Extract out the position of each group's centroid and
                              ## hence the bounding box of the line graph
                              unique.points <- match(unique(coord$group), coord$group)
                              curr.pos <- matrix(ncol = 6, nrow = length(unique.points)) 
                              colnames(curr.pos) <- c("x", "y", "x0", "y0", "x1", "y1")
                              curr.pos[, c("x", "y")] <- sf::st_coordinates(coord[unique.points, "geometry"])
                              curr.pos[, "x0"] <- curr.pos[, "x"] - vp.width / 2
                              curr.pos[, "y0"] <- curr.pos[, "y"] - vp.height / 2
                              curr.pos[, "x1"] <- curr.pos[, "x"] + vp.width / 2
                              curr.pos[, "y1"] <- curr.pos[, "y"] + vp.height / 2
                              
                              ## Make sure none of the bounding boxes overlap
                              cent.coords <- as.matrix(ggrepel:::repel_boxes(
                                                                     data_points = curr.pos[, c("x", "y")],
                                                                     point_padding_x = 0.0,
                                                                     point_padding_y = 0.0,
                                                                     boxes = curr.pos[,3:6],
                                                                     xlim = c(0, 1),
                                                                     ylim = c(0, 1),
                                                                     direction = "both"))
                              
                              ## Viewport of each line graph
                              cent.vp <- apply(cent.coords, 1, function(row)
                                  grid::viewport(row["x"], row["y"],
                                           width =  vp.width, height = vp.height,
                                           default.units = "native"))
                              
                              groups <- unique(coord$group)
                              grobs <- lapply(1:length(groups), function(i) {
                                  has.moved <- !isTRUE(all.equal(curr.pos[i, c("x", "y")],
                                                                 cent.coords[i, c("x", "y")], tolerance = 0.01))
                                  sf_categorical(coord[coord$group == groups[i], , drop = FALSE],
                                               vp = cent.vp[[i]], has.moved = has.moved)
                              })
                              
                              do.call(grid::gList, grobs)
                          },
                          
                          draw_key = function(data, params, size) {
                              data <- utils::modifyList(ggplot2:::default_aesthetics(params$legend), data)
                              draw_key_polygon(data, params, size)
                          }
                         )


sf_categorical <- function(coords, vp, has.moved = FALSE) {
    row <- coords[1, , drop = FALSE]
    geometry <- row$geometry[[1]]
    
    row <- utils::modifyList(ggplot2:::GeomPolygon$default_aes, row)
    
    x_coords_ind <- rep(1:nrow(coords), times = rep(c(2, 4, 2), c(1, nrow(coords) - 2, 1)))
    rect_grob <- grid::polygonGrob(coords[x_coords_ind, "cat_x"],
                                   rep(c(0.25, 0.75, 0.75, 0.25), times = nrow(coords) - 1),
                                   id = rep(1:(nrow(coords) - 1), each = 4),
                                   vp = vp,
                                   ## gp = grid::gpar(fill = coords$fill, col = alpha(coords$colour, 0.5), alpha = coords$alpha))
                                   gp = grid::gpar(fill = coords$fill, col = NA, alpha = coords$alpha))
    border_grob <- grid::rectGrob(height = 0.5, gp = grid::gpar(fill = NA, colour = "blue"),
                                  default.units = "native", vp = vp)
    grid::grobTree(rect_grob, border_grob)
}
