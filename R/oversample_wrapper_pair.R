#' oversample_wrapper_pair
#'
#' @param selected_part
#' @param random_number
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

oversample_wrapper_pair<-function(selected_part,random_number,original_min_blue=1,
                                      original_sample_blue=200,by_factor=1.1) {
  message(selected_part)
  yellow<-raster(paste0("../preparations/sampling_rasters/",selected_part,".tif"))
  red<-raster(paste0("../preparations/sampling_rasters/",selected_part,"_1k.tif"))
  blue<-raster(paste0("../preparations/sampling_rasters/",selected_part,"_5k.tif"))
  bins<-readOGR(paste0("../preparations/sampling_shapes/",selected_part,".shp"),
                verbose=FALSE)


  oversampled_values<-oversample_pair(bins=bins,original_min_blue = original_min_blue,
                                          original_sample_blue=  original_sample_blue,
                                          by_factor=by_factor)


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
    normal_sample1,
    selected_part)


  samp1<-sample_pair(
    sampling_bins = bins,
    blue_raster_complete = blue,
    red_raster_complete = red,
    yellow_raster_complete = yellow,
    min_blue = oversampled_values["min_blue"],
    sample_blue = oversampled_values["sample_blue"],
    random_number = random_number)

  samp2<-data_normalization_pair(
    samp1,
    selected_part = selected_part)

  samp2@data$pick<-ifelse(samp2$Name%in%normal_sample2$Name,"Sample","Replacement")

  samp2_sf<-st_as_sf(samp2)

  st_write(samp2_sf %>%
             dplyr::select(-Description),
           dsn=paste0("output/paired/shp/",selected_part,".shp"),
           driver="ESRI Shapefile",
           layer=selected_part,
           delete_dsn = TRUE)


  st_write(samp2_sf,layer=selected_part,
           dsn=paste0("output/paired/kml/",selected_part,".kml"),
           driver="KML",delete_dsn =TRUE)

}
