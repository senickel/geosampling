#' longer_than_one_bind
#'
#' @param list
#' @keywords
#' @keywords
#' @export
#' @examples
#' checking.nominal()
#' @importFrom magrittr %>%
#'
#'
longer_than_one_bind<-function(list) {
  if (!"list"%in%is(list)) return(list)
  if (length(list)==0) return(NULL)
  if (length(list)==1) return(list[[1]])
  list %>%
    do.call(bind,.)
}
