#' simulation_wrapper_non_pair_extract
#'
#' @param selected_part
#' @param extract_list List of validation rasters which are cut to the sampling bin outlines.
#' @param random_number
#' @param original_min_red
#' @param original_sample_red
#' @param verbose If message should be printed. 0 no messages, 1 some, 2 all. 2 is default.
#' @keywords
#' @keywords
#' @export
#' @examples
#' @importFrom magrittr %>%
#'
simulation_wrapper_non_pair_extract<-function(selected_part,
                                      extract_list,
                                      random_number,
                                      original_min_red=2,
                             original_sample_red=200,
                             verbose=2) {
  if (verbose>0) message(selected_part)
  yellow<-raster(paste0("../preparations/sampling_rasters/",selected_part,".tif"))
  red<-raster(paste0("../preparations/sampling_rasters/",selected_part,"_1k.tif"))
  blue<-raster(paste0("../preparations/sampling_rasters/",selected_part,"_5k.tif"))
  bins<-readOGR(paste0("../preparations/sampling_shapes/",selected_part,".shp"),
                verbose=FALSE)
  key_variable<-extract_list[[selected_part]]


  normal_sample1<-sample_non_pair(
    sampling_bins = bins,
    red_raster_complete = red,
    yellow_raster_complete = yellow,
    min_red = original_min_red,
    sample_red = original_sample_red,
    random_number = random_number,
    verbose=verbose)

  normal_sample2<-data_normalization_non_pair(
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
  df$pairwise<-0
  output_path<-paste0("output_machine1/",selected_part,".csv")
  fwrite(df,output_path,append = TRUE)

}
