#' simulation_wrapper_pair_extract
#'
#' @param selected_part
#' @param extract_list List of validation rasters which are cut to the sampling bin outlines.
#' @param random_number
#' @param original_min_blue
#' @param original_sample_blue
#' @param verbose If message should be printed. 0 no messages, 1 some, 2 all. 2 is default.
#' @keywords
#' @keywords
#' @export
#' @examples
#' @importFrom magrittr %>%
#'

simulation_wrapper_pair_extract<-function(selected_part,
                                              extract_list,
                                              random_number,
                                          original_min_blue=1,
                                          original_sample_blue=100,
                                              verbose=2) {
  if (verbose>0) message(selected_part)
  yellow<-raster(paste0("../preparations/sampling_rasters/",selected_part,".tif"))
  red<-raster(paste0("../preparations/sampling_rasters/",selected_part,"_1k.tif"))
  blue<-raster(paste0("../preparations/sampling_rasters/",selected_part,"_5k.tif"))
  bins<-readOGR(paste0("../preparations/sampling_shapes/",selected_part,".shp"),
                verbose=FALSE)
  key_variable<-extract_list[[selected_part]]


  normal_sample1<-sample_pair(
    sampling_bins = bins,
    blue_raster_complete = blue,
    red_raster_complete = red,
    yellow_raster_complete = yellow,
    min_blue = original_min_blue,
    sample_blue= original_sample_blue,
    sample_red=2,
    random_number = random_number)

  normal_sample2<-data_normalization_pair(
    sample_obj = normal_sample1,
    selected_part = selected_part)

  normal_sample2<-normal_sample2[normal_sample2$type=="Red",]

  df<-lapply(1:length(normal_sample2),function(id) {
    lapply(key_variable,function(rs) {
      # message(id,"@",names(rs))
      cropped<-crop(rs,normal_sample2[id,]) %>%
        getValues()
      cropped[is.na(cropped)]<-0
      sum(cropped)
    }) %>%
      do.call(cbind,.)
  }) %>%
    do.call(rbind,.) %>%
    as.data.frame()

  df$area<-selected_part
  df$random_number<-random_number
  df$pairwise<-1
  output_path<-paste0("output_machine1/",selected_part,".csv")
  fwrite(df,output_path,append = TRUE)

}
