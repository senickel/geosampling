#' prepare_sampling_area
#'
#' @param adm0
#' @param border_spatialline
#' @param lakes
#' @param width_in_km in km
#' @param split_width in km
#' @keywords
#' @keywords
#' @export
#' @examples
#' @importFrom magrittr %>%
#'
prepare_sampling_area<-function(adm0,border_spatialline,lakes,width_in_km,split_width=NA) {
  area <- prepare_area(
    adm0,
    border_spatialline,
    width_in_km,
    split_width)

  polygon <- prepare_area_polygon(
    area,
    adm0,
    lakes)

  polygon
}
