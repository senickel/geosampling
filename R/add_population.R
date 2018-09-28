#' add_population
#'
#' @param poly
#' @param raster
#' @keywords
#' @keywords
#' @export
#' @examples
#' @importFrom magrittr %>%
#' @importFrom fasterize fasterize
#' @importFrom sf st_as_sf
#' @import raster

add_population <- function(poly,raster) {
  poly$pop <- sapply(1:length(poly),function(x) {
    db1 <- poly[x,]
    db1 %>%
      st_as_sf() %>%
      fasterize(raster) %>%
      mask(raster,.) %>%
      getValues() %>%
      sum(na.rm=TRUE)
  })
  poly$prob <-  poly$pop/sum(poly$pop)
  poly
}
