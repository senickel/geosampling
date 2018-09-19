#' .add_centroid
#'
#' @param sp_obj
#' @keywords
#' @keywords
#' @export
#' @examples
#' checking.nominal()
#' @importFrom magrittr %>%
#' @importFrom rgeos gCentroid

.add_centroid <- function(sp_obj) {
  sp_obj@data[,c("long","lat")] <- lapply(1:length(sp_obj),function(x) {
    gCentroid(sp_obj[x,])@coords
  }) %>%
    do.call(rbind,.)
  sp_obj
}
