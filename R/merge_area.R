#' merge_area
#'
#' @param a
#' @param b
#' @param start_end
#' @keywords
#' @keywords
#' @export
#' @examples
#' @importFrom magrittr %>%
#'
merge_area <- function(a,b) {

  together <- bind(a,
                 b) %>%
    gUnaryUnion()

  max_size_poly <- sapply(together@polygons[[1]]@Polygons,
                        function(x) x@coords %>% nrow) %>%
    which.max()

  bor <- together@polygons[[1]]@Polygons[[max_size_poly]] %>%
    list %>% Polygons(ID="1") %>% list %>%
    SpatialPolygons()

  proj4string(bor) <- proj4string(a)

  bor
}
