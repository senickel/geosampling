#' sample_non_pair
#'
#' @param sampling_bins
#' @param red_raster_complete
#' @param yellow_raster_complete
#' @param min_red
#' @param sample_red
#' @param random_number
#' @keywords
#' @keywords
#' @export
#' @examples
#' @importFrom magrittr %>%
#'

sample_non_pair<-function(
  sampling_bins, # polygon file
  # blue_raster_complete, # 5k raster file
  red_raster_complete,
  yellow_raster_complete,
  min_red=1, # how many units per bin are going to be sampled at least
  # sample_blue=50, # total number of blue units
  sample_red=200, # number of red units per blue unit
  # sample_yellow=2,
  random_number,
  verbose=2) {

  min_red<-min_red %>%
    as.numeric
  sample_red<-sample_red %>%
    as.numeric

  set.seed(random_number) # set seed

  # FIRST RANDOM: PPS for each bin
  # take out the minimum number
  random_reds<-sample_red-
    (length(sampling_bins))*
    min_red
  #
  if (random_reds>0) {
    red_bins_random<-sample(
      1:length(sampling_bins), # number of bins
      random_reds, # number of blue units that needs to be sampled
      prob = sampling_bins$prob, # probability of sampling bins
      replace=TRUE) %>%   # sampling WITH replacement because
      # blue units can land in the same bin
      table %>% # frequency table
      c %>%
      data.frame(frequency=.,number=names(.) %>%
                   as.numeric) # make a data.frame out of frequency table
  } else {
    red_bins_random<-data.frame(frequency=0,number=1:length(sampling_bins))
  }


  red_bins<-data.frame( # data.frame with
    number=1:length(sampling_bins), # id of bins
    bins=min_red) %>% # minimum of blue units per bin
    left_join(red_bins_random,by=c("number")) %>% # merge sampled bins
    mutate(
      frequency=
        ifelse(is.na(frequency),0,frequency)) %>% # if units where not sampled
    # they should get a 0 instead of NA
    mutate(bins=bins+frequency) %>% # add the number of minimum bins and the number of units sampled
    dplyr::select(-frequency) # drop the number of units sampled


  # loop over bins
  lapply(1:nrow(red_bins),function(rb1) {
    if (verbose>0) message(paste("Bin",rb1))
    rb<-red_bins[rb1,] %>%
      unlist
    poly_shape<-sampling_bins[rb[1],] # select bin

    # add ids etc.
    poly_shape$bin_id<-rb[1]
    poly_shape@data<-poly_shape@data %>%
      dplyr::select(-X1) %>%
      rename(bin_population=pop,
             bin_probability=prob)
    poly_shape$is_bin<-TRUE

    raster_part_red_crop<-
      crop(red_raster_complete,poly_shape) # crop 5k raster to bin

    raster_part_red_cover<-            # define how much of the pixels are
      rasterize(poly_shape,             # covered by the bin.
                raster_part_red_crop,
                getCover=TRUE)

    # set to 0 when cover is less than 30%
    # in other words: do not sample pixels that lie with less than 30%
    # inside of the bin
    cover_values<-cover_val(getValues(raster_part_red_cover))

    # cover_values<-ifelse(getValues(raster_part_red_cover)<30,0,
    # getValues(raster_part_red_cover))

    # reduce the number of people living in that pixel by the coverage
    # so that we do not assume that a pixel has a lot of people in it if it
    # only lies in parts in the bin in question
    red_raster<-setValues(raster_part_red_crop,
                          getValues(raster_part_red_crop)*
                            cover_values/100)

    # cut yellow_raster_complete to bin
    yellow_raster<-crop(yellow_raster_complete,poly_shape)

    # draw the sample. look at function sample_from_raster
    # for further information

    # message("made it 1")
    selected_reds<-sample_from_raster(
      raster_file = red_raster,
      poly_shape = poly_shape,
      sample_size = rb[2],
      seed = random_number,
      check_raster=yellow_raster,
      verbose=verbose) %>%
      reshape_poly(poly_before = poly_shape, # add ids, prob, pop and unify colnames
                   name_of_unit = "red")

    # return list with objects
    poly_complete<-list(poly_shape,
                        selected_reds
    ) %>%
      do.call(bind,.)

    poly_complete@data[,grepl("is_",colnames(poly_complete@data))]<-
      poly_complete@data[,grepl("is_",colnames(poly_complete@data))] %>%
      apply(2,function(x) {
        if (any(is.na(x))) x[is.na(x)]<-FALSE
        x
      })

    poly_complete
  }) %>%
    do.call(bind,.)

}
