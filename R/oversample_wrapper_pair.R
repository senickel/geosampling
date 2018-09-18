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

oversample_wrapper_pair<-function(
  name,
  bins,
  raster_1,
  raster_2,
  raster_3,
  random_number,
  original_min_unit3=1,
  original_sample_unit3=200,
  by_factor=1.1,
  verbose=2) {

  if (verbose > 0) message(name)
  # raster_1<-raster(paste0("../preparations/sampling_rasters/",selected_part,".tif"))
  # raster_2<-raster(paste0("../preparations/sampling_rasters/",selected_part,"_1k.tif"))
  # raster_3<-raster(paste0("../preparations/sampling_rasters/",selected_part,"_5k.tif"))
  # bins<-readOGR(paste0("../preparations/sampling_shapes/",selected_part,".shp"),
  #               verbose=FALSE)


  oversampled_values<-oversample_pair(bins=bins,
                                      original_min_unit3 = original_min_unit3,
                                      original_sample_unit3=  original_sample_unit3,
                                      by_factor=by_factor)


  normal_sample1<-sample_pair(
    sampling_bins = bins,
    raster_1 = raster_1,
    raster_2 = raster_2,
    raster_3 = raster_3,
    min_unit3 = original_min_unit3,
    sample_unit3= original_sample_unit3,
    sample_unit2=2,
    random_number = random_number,
    verbose = verbose)


  normal_sample2<-data_normalization_pair(
    normal_sample1,
    name)


  samp1<-sample_pair(
    sampling_bins = bins,
    raster_1 = raster_1,
    raster_2 = raster_2,
    raster_3 = raster_3,
    min_unit3 = oversampled_values["min_unit3"],
    sample_unit3 = oversampled_values["sample_unit3"],
    sample_unit2 = 2,
    random_number = random_number,
    verbose= verbose)

  samp2<-data_normalization_pair(
    samp1,
    name)

  samp2@data$pick<-ifelse(samp2$Name%in%normal_sample2$Name,"Sample","Replacement")
#
#   samp2_sf<-st_as_sf(samp2)
#
#   st_write(samp2_sf %>%
#              dplyr::select(-Description),
#            dsn=paste0("output/paired/shp/",name,".shp"),
#            driver="ESRI Shapefile",
#            layer=name,
#            delete_dsn = TRUE)
#
#
#   st_write(samp2_sf,layer=name,
#            dsn=paste0("output/paired/kml/",name,".kml"),
#            driver="KML",delete_dsn =TRUE)
  samp2

}
