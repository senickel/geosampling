#' polygon_from_coords
#'
#' @param coords
#' @param p4s
#' @keywords
#' @keywords
#' @export
#' @examples
#' @importFrom magrittr %>%
#'
#'
polygon_from_coords<-function(coords,p4s) {
  coords %>%
    Polygon() %>%
    list %>%
    Polygons(ID="1") %>%
    list %>%
    SpatialPolygons(proj4string = CRS(p4s))
}
