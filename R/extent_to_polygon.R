#' extent_to_polygon
#'
#' @param exobj
#' @keywords
#' @keywords
#' @export
#' @examples
#' @importFrom magrittr %>%
#'
extent_to_polygon <- function(exobj) {

  if (is(exobj,"SpatialPolygons")) exobj <- extent(exobj)
  if (!is(exobj,"Extent")) stop("exobj is neither SpatialPolygons nor Extent")

  data.frame(long=c(exobj[1],exobj[1],exobj[2],exobj[2]),
             lat=c(exobj[3],exobj[4],exobj[4],exobj[3])) %>%
    Polygon %>%
    list() %>%
    Polygons(ID=1) %>%
    list %>%
    SpatialPolygons(proj4string = CRS(proj4string(shape)))
}
