#' perpendicular_line
#'
#' @param coef_obj
#' @param point
#' @param width_in_km
#' @param p4s
#' @keywords
#' @keywords
#' @export
#' @examples
#' @importFrom magrittr %>%
#'

perpendicular_line<-function(coef_obj,point,width_in_km,p4s) {

  b1 <- point[2]-(-1*(1/coef_obj[2]))*point[1]

  orthogonal_line <- new_line_thru_math(c(b1,1/coef_obj[2]*-1),
                                      c(180,-180),
                                      p4s)

  buf_100k <- SpatialPoints(point %>%
                            as.matrix() %>%
                            t,
                          proj4string = CRS(p4s)) %>%
    buffer_shape(width_in_km = width_in_km)

  crop(orthogonal_line,buf_100k[[2]])

}
