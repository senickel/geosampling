#' prepare_area_polygon
#'
#' @param border_polygon
#' @param shape
#' @param lakes
#' @keywords
#' @keywords
#' @export
#' @examples
#' @importFrom magrittr %>%
#'
#'
prepare_area_polygon<-function(border_polygon,shape,lakes) {

  border_polygon2 <-
    spTransform(border_polygon,
                CRS('+init=EPSG:32736'))

  border_polygon3 <- gBuffer(border_polygon2,
                          byid=TRUE, width=0)

  border_polygon4 <-
    spTransform(border_polygon3,
                CRS(proj4string(border_polygon)))

  border_polygon5 <- gDifference(border_polygon4,lakes)

  border_polygon6 <- border_polygon5 %>%
    SpatialPolygonsDataFrame(data=data.frame(1))

  biggest_poly<-sapply(border_polygon6@polygons[[1]]@Polygons,function(x) x@area) %>%
    which.max()

  border_polygon7 <- border_polygon6@polygons[[1]]@Polygons[[biggest_poly]] %>%
    list() %>%
    Polygons(ID=1) %>%
    list %>%
    SpatialPolygons(proj4string = CRS(proj4string(border_polygon6))) %>%
    SpatialPolygonsDataFrame(data=data.frame(1))

  crop(border_polygon6,border_polygon7)
}
