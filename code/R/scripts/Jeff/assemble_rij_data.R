#' Assemble rij data
#'
#' Assemble rij data (aka planning unit-vs-species data).
#'
#' @param pu \code{\link[raster]{RasterLayer}} object with planning units.
#'
#' @param features \code{\link{sp}{SpatialPolygonsDataFrame}} with features.
#'
#' @param threads \code{integer} number of threads for processing.
#'
#' @return \code{\link[base]{data.frame}} wih three columns:
#'  \code{pu} containing \code{integer} planning unit identifiers;
#'  \code{species} containing \code{integer} feature identifiers;
#'  \code{amount} containing \code{numeric} amount of the features in the
#'  planning units.
#'
#' @export
assemble_rij_data <- function(pu, features, threads) {
  # validate arguments
  assertthat::assert_that(inherits(pu, "RasterLayer"),
                          inherits(features, "SpatialPolygonsDataFrame"),
                          assertthat::is.count(threads),
                          raster::compareCRS(pu@crs, features@proj4string))
  # calculate the area of the raster cells in square kilometers
  area <- units::set_units(prod(raster::res(pu)), m^2)
  area <- units::set_units(area, km^2)
  area <- as.numeric(area)
  # prepare cluster for parallel processing
  if (isTRUE(threads > 1)) {
    cl <- parallel::makeCluster(threads, "PSOCK")
    parallel::clusterEvalQ(cl, {library(raster)})
    parallel::clusterExport(cl, c("pu", "features", "area"),
                            envir = environment())
    doParallel::registerDoParallel(cl)
  }
  # main processing
  result <- plyr::ldply(seq_len(nrow(features@data)),
                        .parallel = isTRUE(threads > 1),
                        function(i) {
    # extract planning unit ids that overlap with the feature
    ids <- unlist(raster::extract(pu, features[i, ]), recursive = TRUE,
                  use.names = FALSE)
    ids <- unique(ids)
    ids <- ids[!is.na(ids)]
    # return data.frame
    if (length(ids) > 0) {
      return(data.frame(pu = ids, species = i, amount = area))
    } else {
      return(data.frame())
    }
  })
  # kill cluster
  if (isTRUE(threads > 1)) {
    doParallel::stopImplicitCluster()
    cl <- parallel::stopCluster(cl)
  }
  # return result
  return(result)
}
