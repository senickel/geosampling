#' buffer_shape
#'
#' @param shape
#' @param width_in_km
#' @keywords
#' @keywords
#' @export
#' @examples
#' @importFrom magrittr %>%
#'
#'
buffer_shape<-function(shape,width_in_km) {
  if ("SpatialPolygons"%in%is(shape)) shape=gUnaryUnion(shape)
  shape_reproj=spTransform(shape,CRS('+init=EPSG:32736'))
  shape_reproj=gBuffer(shape_reproj,width=1000*width_in_km)
  shape_2=spTransform(shape_reproj,CRS(proj4string(shape)))
  list(shape,shape_2)
}
