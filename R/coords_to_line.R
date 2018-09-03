#' coords_to_line
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
coords_to_line<-function(coords,p4s) {
  coords %>%
    Line %>%
    list %>%
    Lines(ID="1") %>%
    list %>%
    SpatialLines(proj4string = CRS(p4s))
}
