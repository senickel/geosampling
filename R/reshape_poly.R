#' reshape_poly
#'
#' @param poly
#' @param poly_before
#' @param name_of_unit
#' @keywords
#' @keywords
#' @export
#' @examples
#' @importFrom magrittr %>%
#'
#'
reshape_poly<-function(poly,poly_before,name_of_unit) {
  poly@data<-cbind(poly_before@data,poly@data,
                   row.names = NULL)
  poly@data[,grepl("is_",colnames(poly@data))]<-FALSE

  poly$id<-1:length(poly)
  if ("dummy"%in%colnames(poly@data))
    poly@data<-poly@data %>%
    dplyr::select(-dummy)

  colnames(poly@data)[(ncol(poly)-2):
                        ncol(poly)]<-
    paste0(name_of_unit,"_",
           colnames(poly@data)[(ncol(poly)-2):
                                 ncol(poly)])

  poly@data[,paste0("is_",name_of_unit)]<-TRUE
  poly
}
