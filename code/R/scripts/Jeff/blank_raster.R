blank_raster <- function(x, res) {
  assertthat::assert_that(inherits(x, "Spatial"), is.numeric(res),
                          all(is.finite(res)), length(res) %in% c(1, 2))
  # initialize resolution inputs
  if (length(res) == 1)
    res <- c(res, res)
  # extract coordinates
  if ((raster::xmax(x) - raster::xmin(x)) <= res[1]) {
    xpos <- c(raster::xmin(x), res[1])
  } else {
    xpos <- seq(raster::xmin(x),
                raster::xmax(x) + (res[1] * (((raster::xmax(x) -
                  raster::xmin(x)) %% res[1]) != 0)),
                res[1])
  }
  if ((raster::ymax(x) - raster::ymin(x)) <= res[2]) {
    ypos <- c(raster::ymin(x), res[2])
  } else {
    ypos <- seq(raster::ymin(x),
                raster::ymax(x) + (res[2] * (((raster::ymax(x) -
                  raster::ymin(x)) %% res[2]) != 0)),
                res[2])
  }
  # generate raster from sp
  rast <- raster::raster(xmn = min(xpos), xmx = max(xpos), ymn = min(ypos),
                         ymx = max(ypos), nrow = length(ypos) - 1,
                         ncol = length(xpos) - 1)
  return(raster::setValues(rast, 1))
}

