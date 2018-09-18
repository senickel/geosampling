#' oversample_pair
#'
#' @param bins
#' @param original_min_blue
#' @param original_sample_blue
#' @param by_factor
#' @keywords
#' @keywords
#' @export
#' @examples
#' @importFrom magrittr %>%
#'
#'
oversample_pair<-function(
  bins,
  original_min_unit3=2,
  original_sample_unit3=50,
  by_factor=2) {

  # determine possible maximum in one bin
  possible_maximum<-(original_sample_unit3-(length(bins)-1)*original_min_unit3)
  # we want by_factor times the maximum amount for each bin
  oversample_blue<-ceiling(possible_maximum*by_factor)
  # we want oversample_blue*number of bins in total
  total_units<-oversample_blue*length(bins)
  c(sample_unit3=total_units,min_unit3=oversample_blue)
}

