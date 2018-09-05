#' new_line_thru_math
#'
#' @param coef_obj
#' @param x_vector
#' @param p4s
#' @keywords
#' @keywords
#' @export
#' @examples
#' @importFrom magrittr %>%
#'


new_line_thru_math<-function(coef_obj,x_vector,p4s) {

  lapply(x_vector,function(x) {
    data.frame(x=x,y=coef_obj[2]*x+coef_obj[1])
  }) %>%
    do.call(rbind,.) %>%
    Line %>%
    list %>%
    Lines(ID="1") %>%
    list() %>%
    SpatialLines(proj4string = CRS(p4s))

}
