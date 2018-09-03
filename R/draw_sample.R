#' draw_sample
#'
#' @param poly
#' @param raster_file
#' @param index
#' @param probability
#' @param orig_sample_size
#' @param seed
#' @keywords
#' @keywords
#' @export
#' @examples
#' @importFrom magrittr %>%
#'
#'

draw_sample<-function(poly,
                      raster_file,
                      index,
                      probability,
                      orig_sample_size,
                      seed=1337) {
  sample_size<-orig_sample_size

  if ((index %>% length)<=sample_size) { # if there are less units to
    drawn_sample<-index                  # sample than aimed for sample all
  } else {                                    # if this is not the case
    set.seed(seed)
    drawn_sample<-                            # select the sample WITHOUT replacing them
      sample(index,
             size = sample_size,
             prob=probability,
             replace = FALSE)
  }

  drawn_sample
}
