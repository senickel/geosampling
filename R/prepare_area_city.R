#' prepare_area_city
#'
#' @param shape1
#' @param shape2
#' @param lakes
#' @param width_in_km
#' @param split_width
#' @keywords
#' @keywords
#' @export
#' @examples
#' @importFrom magrittr %>%
#'
#'
prepare_area_city<-function(shape1,shape2,lakes,width_in_km=100,split_width=NA) {
  border_buf<-buffer_shape(shape2,width_in_km)
  border_area<-crop(border_buf[[2]],shape1)
  border_area<-gDifference(border_area,lakes)
  if (is.numeric(split_width)) {
    lower<-buffer_shape(border_buf,split_width)
    border_area<-gDifference(border_area,lower[[2]])
  }
  border_area
}
