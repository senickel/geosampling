#' from_list_to_poly3
#'
#' @param list
#' @param position2
#' @keywords
#' @keywords
#' @export
#' @examples
#' checking.nominal()
#' @importFrom magrittr %>%
#'
#'

from_list_to_poly3<-function(list,
                             position2=1) {

    bind_list<-lapply(1:length(list),function(x) {
      list[[x]][[position2]]
    })
    bind_list<-bind_list[!sapply(bind_list,is.null)]
    if (length(bind_list)==0) return()
    if (length(bind_list)==1) return(bind_list[[1]])
    bind_list %>%
      do.call(bind,.)
  # }
}
