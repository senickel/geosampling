#' oversample_non_pair
#'
#' @param bins
#' @param original_min_red
#' @param original_sample_red
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
  original_min_red=2,
  original_sample_red=200,
  by_factor=1.1) {

  # determine possible maximum in one bin
  possible_maximum<-(original_sample_red-(length(bins)-1)*original_min_red)
  # we want by_factor times the maximum amount for each bin
  oversample_red<-ceiling(possible_maximum*by_factor)
  # we want oversample_blue*number of bins in total
  total_units<-oversample_red*length(bins)
  c(sample_red=total_units,min_red=oversample_red)
}

