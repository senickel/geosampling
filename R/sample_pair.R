#' sample_pair
#'
#' @param sampling_bins
#' @param raster_1
#' @param raster_2
#' @param raster_3
#' @param min_unit3
#' @param sample_unit3
#' @param sample_unit2
#' @param random_number
#' @keywords
#' @keywords
#' @export
#' @examples
#' @importFrom magrittr %>%
#'

sample_pair<-function(
  sampling_bins, # polygon file
  raster_1,
  raster_2,
  raster_3, # 5k raster file
  min_unit3=1, # how many units per bin are going to be sampled at least
  sample_unit3=100, # total number of units3
  sample_unit2=2, # number of units2 per unit3
  random_number,
  verbose=2) {

  min_unit3<-min_unit3 %>%
    as.numeric
  sample_unit3<-sample_unit3 %>%
    as.numeric
  sample_unit2<-sample_unit2 %>%
    as.numeric

  set.seed(random_number) # set seed

  # FIRST RANDOM: PPS for each bin
  # take out the minimum number
  random_unit3<-sample_unit3-
    (length(sampling_bins))*
    min_unit3
  #


  if (random_unit3>0) { # only execute if there are any unit3 to be collected

    ## making sure that if there are not enough units in one bin,
    # to sample the difference from the other bins

    sampling_df<-lapply(1:length(sampling_bins),function(x) {

      poly_shape<-sampling_bins[x,] # select bin
      # add ids etc.
      poly_shape$bin_id<-x
      poly_shape@data<-poly_shape@data %>%
        dplyr::select(-X1) %>%
        rename(bin_population=pop,
               bin_probability=prob)
      poly_shape$is_bin<-TRUE

      raster_part_unit3_crop<-
        crop(raster_3,poly_shape) # crop 5k raster to bin

      raster_part_unit3_cover<-            # define how much of the pixels are
        rasterize(poly_shape,             # covered by the bin.
                  raster_part_unit3_crop,
                  getCover=TRUE)

      cover_values<-cover_val(getValues(raster_part_unit3_cover))
      data.frame(bin=x,diff=0,amount=cover_values[cover_values>0] %>%
                   length,reached=FALSE)

    }) %>%
      do.call(rbind,.)

    # substract minimum amount because its added later on
    sampling_df$amount<-sampling_df$amount-min_unit3

    # prep for loop
    sampling_df2<-sampling_df
    sample_unit3<-random_unit3

    end_draw<-data.frame(Freq=0,number=1:length(sampling_bins))


    while(sample_unit3>0) {

      drawn_unit3_bins<-sample(
        which(!sampling_df2$reached), # number of bins
        sample_unit3, # number of unit3 that needs to be sampled
        prob = sampling_bins$prob[which(!sampling_df2$reached)], # probability of sampling bins
        replace=TRUE) %>%   # sampling WITH replacement because
        # unit3 can land in the same bin
        table %>% # frequency table
        c %>%
        data.frame(frequency=.,number=names(.) %>%
                     as.numeric)

      middle_draw<-full_join(drawn_unit3_bins,end_draw,by="number") %>%
        mutate(frequency=ifelse(is.na(frequency),0,frequency)) %>%
        arrange(number) %>%
        mutate(Freq=Freq+frequency) %>%
        dplyr::select(-frequency)



      sampling_df3<-cbind(sampling_df2,
                          middle_draw) %>%
        mutate(diff=ifelse(Freq>amount,amount,Freq),
               reached=diff==amount)

      end_draw<-sampling_df3 %>%
        dplyr::select(number,diff) %>%
        rename(Freq=diff)

      sampling_df2<-sampling_df3 %>%
        dplyr::select(bin,diff,amount,reached)

      sample_unit3<-sample_unit3-sum(sampling_df2$diff)

    }

    unit3_bins_random<-end_draw %>%
      rename(frequency=Freq)

  } else {
    unit3_bins_random<-data.frame(frequency=0,number=1:length(sampling_bins))
  }


  unit3_bins<-data.frame( # data.frame with
    number=1:length(sampling_bins), # id of bins
    bins=min_unit3) %>% # minimum of unit3 per bin
    left_join(unit3_bins_random,by=c("number")) %>% # merge sampled bins
    mutate(
      frequency=
        ifelse(is.na(frequency),0,frequency)) %>% # if units where not sampled
    # they should get a 0 instead of NA
    mutate(bins=bins+frequency) %>% # add the number of minimum bins and the number of units sampled
    dplyr::select(-frequency) # drop the number of units sampled


  # loop over bins
  picked1<-lapply(1:nrow(unit3_bins),function(bb1) {
    if (verbose>0) message(paste("Bin",bb1))
    bb<-unit3_bins[bb1,] %>%
      unlist
    poly_shape<-sampling_bins[bb[1],] # select bin

    # add ids etc.
    poly_shape$bin_id<-bb[1]
    poly_shape@data<-poly_shape@data %>%
      dplyr::select(-X1) %>%
      rename(bin_population=pop,
             bin_probability=prob)
    poly_shape$is_bin<-TRUE

    raster_part_unit3_crop<-
      crop(raster_3,poly_shape) # crop 5k raster to bin

    raster_part_unit3_cover<-            # define how much of the pixels are
      rasterize(poly_shape,             # covered by the bin.
                raster_part_unit3_crop,
                getCover=TRUE)

    # set to 0 when cover is less than 30%
    # in other words: do not sample pixels that lie with less than 30%
    # inside of the bin
    cover_values<-cover_val(getValues(raster_part_unit3_cover))

    # reduce the number of people living in that pixel by the coverage
    # so that we do not assume that a pixel has a lot of people in it if it
    # only lies in parts in the bin in question
    unit3_raster<-setValues(raster_part_unit3_crop,
                           getValues(raster_part_unit3_crop)*
                             cover_values/100)

    # cut yellow_raster_complete to bin
    unit1_raster<-crop(raster_1,poly_shape)

    # draw the sample. look at function sample_from_raster
    # for further information

    # message("made it 1")
    selected_unit3<-sample_from_raster(
      raster_file = unit3_raster,
      poly_shape = poly_shape,
      sample_size = bb[2],
      seed = random_number,
      check_raster=unit1_raster,
      verbose=verbose) %>%
      reshape_poly(poly_before = poly_shape, # add ids, prob, pop and unify colnames
                   name_of_unit = "unit3")

    # message("made it 2")
    # SAMPLING OF UNIT2
    unit2_raster<-lapply(1:length(selected_unit3),function(i) {
      r1<-crop(raster_2,selected_unit3[i,])
      s1<-mask(r1,selected_unit3[i,])
      mask(s1,poly_shape)
    })

    # message("made it 3")
    selected_unit2<-lapply(1:length(selected_unit3),function(i) {
      # message(i)
      unit1_raster2<-crop(unit1_raster,unit2_raster[[i]])

      sample_from_raster(raster_file = unit2_raster[[i]],
                         poly_shape = poly_shape,

                         sample_size = sample_unit2,
                         seed = random_number,
                         check_raster = unit1_raster2,
                         verbose=verbose) %>%
        reshape_poly(poly_before = selected_unit3[i,], # add ids, prob, pop and unify colnames
                     name_of_unit = "unit2")
    }) %>%
      longer_than_one_bind()


    # return list with objects
    poly_complete<-list(poly_shape,
                        selected_unit3,
                        selected_unit2#,
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




