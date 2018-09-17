#' sample_non_pair
#'
#' @param sampling_bins
#' @param raster_1
#' @param raster_2
#' @param min_unit2
#' @param sample_unit2
#' @param random_number
#' @keywords
#' @keywords
#' @export
#' @examples
#' @importFrom magrittr %>%
#'

sample_non_pair<-function(
  sampling_bins, # polygon file
  raster_1,
  raster_2,
  min_unit2=1, # how many units per bin are going to be sampled at least
  sample_unit2,
  random_number,
  verbose=2) {

  min_unit2<-min_unit2 %>%
    as.numeric
  sample_unit2<-sample_unit2 %>%
    as.numeric

  set.seed(random_number) # set seed

  # FIRST RANDOM: PPS for each bin
  # take out the minimum number
  random_unit2<-sample_unit2-
    (length(sampling_bins))*
    min_unit2
  #
  if (random_unit2>0) {
    unit2_bins_random<-sample(
      1:length(sampling_bins), # number of bins
      random_unit2, # number of unit_2 units that needs to be sampled
      prob = sampling_bins$prob, # probability of sampling bins
      replace=TRUE) %>%   # sampling WITH replacement because
      table %>% # frequency table
      c %>%
      data.frame(frequency=.,number=names(.) %>%
                   as.numeric) # make a data.frame out of frequency table
  } else {
    unit2_bins_random<-data.frame(frequency=0,number=1:length(sampling_bins))
  }


  unit2_bins<-data.frame( # data.frame with
    number=1:length(sampling_bins), # id of bins
    bins=min_unit2) %>% # minimum of unit_2 units per bin
    left_join(unit2_bins_random,by=c("number")) %>% # merge sampled bins
    mutate(
      frequency=
        ifelse(is.na(frequency),0,frequency)) %>% # if units where not sampled
    # they should get a 0 instead of NA
    mutate(bins=bins+frequency) %>% # add the number of minimum bins and the number of units sampled
    dplyr::select(-frequency) # drop the number of units sampled


  # loop over bins
  lapply(1:nrow(unit2_bins),function(rb1) {
    if (verbose>0) message(paste("Bin",rb1))
    rb<-unit2_bins[rb1,] %>%
      unlist
    poly_shape<-sampling_bins[rb[1],] # select bin

    # add ids etc.
    poly_shape$bin_id<-rb[1]
    poly_shape@data<-poly_shape@data %>%
      dplyr::select(-X1) %>%
      rename(bin_population=pop,
             bin_probability=prob)
    poly_shape$is_bin<-TRUE

    raster_part_unit2_crop<-
      crop(raster_2,poly_shape) # crop 5k raster to bin

    raster_part_unit2_cover<-            # define how much of the pixels are
      rasterize(poly_shape,             # covered by the bin.
                raster_part_unit2_crop,
                getCover=TRUE)

    # set to 0 when cover is less than 30%
    # in other words: do not sample pixels that lie with less than 30%
    # inside of the bin
    cover_values<-cover_val(getValues(raster_part_unit2_cover))


    # reduce the number of people living in that pixel by the coverage
    # so that we do not assume that a pixel has a lot of people in it if it
    # only lies in parts in the bin in question
    unit2_raster<-setValues(raster_part_unit2_crop,
                          getValues(raster_part_unit2_crop)*
                            cover_values/100)

    # cut raster_1 to bin
    unit1_raster<-crop(raster_1,poly_shape)

    # draw the sample. look at function sample_from_raster
    # for further information

    # message("made it 1")
    selected_unit2<-sample_from_raster(
      raster_file = unit2_raster,
      poly_shape = poly_shape,
      sample_size = rb[2],
      seed = random_number,
      check_raster=unit1_raster,
      verbose=verbose) %>%
      reshape_poly(poly_before = poly_shape, # add ids, prob, pop and unify colnames
                   name_of_unit = "unit2")

    # return list with objects
    poly_complete<-list(poly_shape,
                        selected_unit2
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
