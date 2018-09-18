#' oversample_wrapper_non_pair
#'
#' @param name
#' @param bins
#' @param raster_1
#' @param raster_2
#' @param random_number
#' @param original_min_unit2
#' @param original_sample_unit2
#' @param by_factor
#' @keywords
#' @keywords
#' @export
#' @examples
#' @importFrom magrittr %>%
#'
#'

oversample_wrapper_non_pair<-function(
  name,
  bins,
  raster_1,
  raster_2,
  random_number,
  original_min_unit2=2,
  original_sample_unit2=200,
  by_factor=1.1,
  verbose=2) {

  if (verbose > 0) message(name)
  # raster_1<-raster(paste0("../preparations/sampling_rasters/",name,".tif"))
  # raster_2<-raster(paste0("../preparations/sampling_rasters/",name,"_1k.tif"))
  # blue<-raster(paste0("../preparations/sampling_rasters/",name,"_5k.tif"))
  # bins<-readOGR(paste0("../preparations/sampling_shapes/",name,".shp"),
  #               verbose=FALSE)


  oversampled_values<-oversample_non_pair(bins=bins,
                                          original_min_unit2 =original_min_unit2,
                                          original_sample_unit2 =  original_sample_unit2,
                                          by_factor=by_factor)


  normal_sample1<-sample_non_pair(
    sampling_bins = bins,
    raster_2 = raster_2,
    raster_1 = raster_1,
    min_unit2 = original_min_unit2,
    sample_unit2 = original_sample_unit2,
    random_number = random_number,
    verbose = verbose)

  normal_sample2<-data_normalization_non_pair(
    sample_obj = normal_sample1,
    name = name)

  samp1<-sample_non_pair(
    sampling_bins = bins,
    raster_2 = raster_2,
    raster_1 = raster_1,
    min_unit2 = oversampled_values["min_unit2"],
    sample_unit2 = oversampled_values["sample_unit2"],
    random_number = random_number,
    verbose = verbose)

  samp2<-data_normalization_non_pair(
    samp1,
    name = name)

  samp2@data$pick<-ifelse(samp2$Name%in%normal_sample2$Name,"Sample","Replacement")

  # samp2_sf<-st_as_sf(samp2)

  # st_write(samp2_sf %>%
  #            dplyr::select(-Description),
  #          dsn=paste0("output/non-paired/shp/",name,".shp"),
  #          driver="ESRI Shapefile",
  #          layer=name,
  #          delete_dsn = TRUE)
  #
  #
  # st_write(samp2_sf,layer=name,
  #          dsn=paste0("output/non-paired/kml/",name,".kml"),
  #          driver="KML",delete_dsn =TRUE)

  return(samp2)
}
