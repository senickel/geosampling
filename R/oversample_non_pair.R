#' oversample_non_pair
#'
#' @param bins
#' @param original_min_unit2
#' @param original_sample_unit2
#' @param by_factor
#' @keywords
#' @keywords
#' @export
#' @examples
#' @importFrom magrittr %>%
#'
#'
oversample_non_pair<-function(
  bins,
  original_min_unit2=2,
  original_sample_unit2=200,
  by_factor=1.1) {

  # determine possible maximum in one bin
  possible_maximum<-(original_sample_unit2-(length(bins)-1)*original_min_unit2)
  # we want by_factor times the maximum amount for each bin
  oversample_red<-ceiling(possible_maximum*by_factor)
  # we want oversample_blue*number of bins in total
  total_units<-oversample_red*length(bins)
  c(sample_unit2=total_units,min_unit2=oversample_red)
}

