#' prepare_raster_of_sampling_area
#'
#' @param pop_raster
#' @param area_polygon
#' @keywords
#' @keywords
#' @export
#' @examples
#' @importFrom magrittr %>%
#' @importFrom fasterize fasterize
#'
prepare_raster_of_sampling_area<-function(pop_raster,area_polygon) {

  # cut raster to extent of border area
  cropped_raster<-crop(pop_raster,area_polygon)

  # convert polygon to raster
  area_raster<-fasterize(area_polygon %>%
                             st_as_sf(),
                         cropped_raster)

  # mask cropped_raster to area_raster
  mask(cropped_raster,area_raster)
}
