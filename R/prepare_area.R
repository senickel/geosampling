#' prepare_area
#'
#' @param shape1
#' @param shape2
#' @param width_in_km in km
#' @param split_width in km
#' @keywords
#' @keywords
#' @export
#' @examples
#' @importFrom magrittr %>%
#'
prepare_area<-function(shape1,shape2,width_in_km=100,split_width=NA) {
  shape1_list<-buffer_shape(shape1,width_in_km)
  shape2_list<-buffer_shape(shape2,width_in_km)


  area<-crop(shape2_list[[2]],shape1_list[[1]])

  if (is.numeric(split_width)) {
    # get  border
    shape1_list_small<-buffer_shape(shape1,width_in_km = 1/10000)
    shape2_list_small<-buffer_shape(shape2,width_in_km = 1/10000)
    shape2_3_small=crop(shape2_list_small[[2]],shape1_list_small[[1]])
    # buffer around border
    lower<-buffer_shape(shape2_3_small,split_width)
    area<-gDifference(area,lower[[2]])
  }
  area
}
