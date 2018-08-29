#' sample_pair
#'
#' @param sampling_bins
#' @param blue_raster_complete
#' @param red_raster_complete
#' @param yellow_raster_complete
#' @param min_blue
#' @param sample_blue
#' @param sample_red
#' @param random_number
#' @keywords
#' @keywords
#' @export
#' @examples
#' @importFrom magrittr %>%
#'

sample_pair<-function(
  sampling_bins, # polygon file
  blue_raster_complete, # 5k raster file
  red_raster_complete,
  yellow_raster_complete,
  min_blue=1, # how many units per bin are going to be sampled at least
  sample_blue=100, # total number of blue units
  sample_red=2, # number of red units per blue unit
  random_number) {

  min_blue<-min_blue %>%
    as.numeric
  sample_blue<-sample_blue %>%
    as.numeric
  sample_red<-sample_red %>%
    as.numeric

  set.seed(random_number) # set seed

  # FIRST RANDOM: PPS for each bin
  # take out the minimum number
  random_blues<-sample_blue-
    (length(sampling_bins))*
    min_blue
  #
  if (random_blues>0) {
    blue_bins_random<-sample(
      1:length(sampling_bins), # number of bins
      random_blues, # number of blue units that needs to be sampled
      prob = sampling_bins$prob, # probability of sampling bins
      replace=TRUE) %>%   # sampling WITH replacement because
      # blue units can land in the same bin
      table %>% # frequency table
      c %>%
      data.frame(frequency=.,number=names(.) %>%
                   as.numeric) # make a data.frame out of frequency table
  } else {
    blue_bins_random<-data.frame(frequency=0,number=1:length(sampling_bins))
  }


  blue_bins<-data.frame( # data.frame with
    number=1:length(sampling_bins), # id of bins
    bins=min_blue) %>% # minimum of blue units per bin
    left_join(blue_bins_random,by=c("number")) %>% # merge sampled bins
    mutate(
      frequency=
        ifelse(is.na(frequency),0,frequency)) %>% # if units where not sampled
    # they should get a 0 instead of NA
    mutate(bins=bins+frequency) %>% # add the number of minimum bins and the number of units sampled
    dplyr::select(-frequency) # drop the number of units sampled


  # loop over bins
  lapply(1:nrow(blue_bins),function(bb1) {
    message(paste("Bin",bb1))
    bb<-blue_bins[bb1,] %>%
      unlist
    poly_shape<-sampling_bins[bb[1],] # select bin

    # add ids etc.
    poly_shape$bin_id<-bb[1]
    poly_shape@data<-poly_shape@data %>%
      dplyr::select(-X1) %>%
      rename(bin_population=pop,
             bin_probability=prob)
    poly_shape$is_bin<-TRUE

    raster_part_blue_crop<-
      crop(blue_raster_complete,poly_shape) # crop 5k raster to bin

    raster_part_blue_cover<-            # define how much of the pixels are
      rasterize(poly_shape,             # covered by the bin.
                raster_part_blue_crop,
                getCover=TRUE)

    # set to 0 when cover is less than 30%
    # in other words: do not sample pixels that lie with less than 30%
    # inside of the bin
    cover_values<-cover_val(getValues(raster_part_blue_cover))
    # cover_values<-ifelse(getValues(raster_part_blue_cover)<30,0,
    #                      getValues(raster_part_blue_cover))

    # reduce the number of people living in that pixel by the coverage
    # so that we do not assume that a pixel has a lot of people in it if it
    # only lies in parts in the bin in question
    blue_raster<-setValues(raster_part_blue_crop,
                           getValues(raster_part_blue_crop)*
                             cover_values/100)

    # cut yellow_raster_complete to bin
    yellow_raster<-crop(yellow_raster_complete,poly_shape)

    # draw the sample. look at function sample_from_raster
    # for further information

    # message("made it 1")
    selected_blues<-sample_from_raster(
      raster_file = blue_raster,
      poly_shape = poly_shape,
      sample_size = bb[2],
      seed = random_number,
      check_raster=yellow_raster) %>%
      reshape_poly(poly_before = poly_shape, # add ids, prob, pop and unify colnames
                   name_of_unit = "blue")

    # message("made it 2")
    # SAMPLING OF RED
    red_raster<-lapply(1:length(selected_blues),function(i) {
      r1<-crop(red_raster_complete,selected_blues[i,])
      s1<-mask(r1,selected_blues[i,])
      mask(s1,poly_shape)
    })

    # message("made it 3")
    selected_reds<-lapply(1:length(selected_blues),function(i) {
      # message(i)
      yellow_raster2<-crop(yellow_raster,red_raster[[i]])

      sample_from_raster(raster_file = red_raster[[i]],
                         poly_shape = poly_shape,

                         sample_size = sample_red,
                         seed = random_number,
                         check_raster = yellow_raster2) %>%
        reshape_poly(poly_before = selected_blues[i,], # add ids, prob, pop and unify colnames
                     name_of_unit = "red")
    }) %>%
      longer_than_one_bind()


    # return list with objects
    poly_complete<-list(poly_shape,
                        selected_blues,
                        selected_reds#,
                        # selected_yellows
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
