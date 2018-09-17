#' sample_from_raster
#'
#' @param raster_file
#' @param poly_shape
#' @param sample_size
#' @param seed
#' @param check_raster
#' @keywords
#' @keywords
#' @export
#' @examples
#' @importFrom magrittr %>%
#'
#'
sample_from_raster<-function(raster_file,
                             poly_shape,
                             sample_size=20,
                             seed=1337,
                             check_raster=NULL,
                             verbose=2) {
  set.seed(seed)
  poly_shape_sf<-st_as_sf(poly_shape)

  # make a data.frame out of raster and replace NA with 0
  gv<-getValues(raster_file) # get values from raster
  gv[is.na(gv)]<-0 # replace NA with 0
  raster_file<-setValues(raster_file,gv) # replace values

  raster_df<-as(raster_file,"SpatialPixelsDataFrame") %>% # to data.frame
    as.data.frame()
  colnames(raster_df)[1]<-"value" # set name of variable to "value"

  # calculate probability for each pixel
  raster_df<-raster_df %>%
    mutate(prob_na=value/sum(value,na.rm=TRUE), # probability, if pixel is NA, result is NA
           prob=ifelse(is.na(prob_na),0,prob_na)) # set all NAs to 0

  # make an polygon out of data.frame
  poly<-raster_df %>%
    dplyr::select(x,y) %>%
    SpatialPointsDataFrame(data=raster_df,
                           proj4string = CRS(proj4string(raster_file))) %>%
    SpatialPixels() %>%
    as("SpatialPolygons")

  # only include those in the polygon that have a non 0 probability
  poly<-poly[which(raster_df$prob!=0),]
  # prob<-raster_df$prob[which(raster_df$prob!=0)]
  poly$population<-raster_df$value[which(raster_df$prob!=0)]

  poly$probability<-raster_df$prob[which(raster_df$prob!=0)]
  poly$id<-1:length(poly)
  # draw sample
  # message("sample_from_raster 1")
  if (is.null(check_raster)) {
    drawn_sample<-draw_sample(poly = poly,
                              raster_file = raster_file,
                              index = poly$id, # indexes of polygons
                              probability =poly$probability,
                              orig_sample_size = sample_size,
                              seed=seed)

    poly<-poly %>%
      as("SpatialPolygonsDataFrame")
  } else {

    drawn_sample<-c()
    poly2<-poly
    sample_size2<-sample_size
    loop_counter<-0
    while(sample_size2>0) {
      loop_counter<-loop_counter +1
      if (verbose==2) message(paste("Loop:",loop_counter,
                                    "\nNeeds to pick:",sample_size2,"units",
                                    "\nUnits left:",length(poly2)))

      drawn_sample2<-draw_sample(poly = poly2,
                                 raster_file = raster_file,
                                 index = poly2$id, # indexes of polygons
                                 probability =poly2$probability,
                                 orig_sample_size = sample_size2,
                                 seed=seed)


      # check
      check_sample<-sapply(drawn_sample2,function(x) {

        cropped_raster<-crop(check_raster,poly2[poly2$id==x,])

        poly_shape_rasterized<-
          fasterize(sf = poly_shape_sf,raster = cropped_raster)

        masked<-mask(cropped_raster,poly_shape_rasterized)
        ((masked %>%
            getValues %>% is.na() %>%
            sum)/length(masked))<=0.7
      })

      drawn_sample<-c(drawn_sample,drawn_sample2[check_sample])

      # change sample size
      sample_size2<-sample_size-length(drawn_sample)
      # delete sampled and empty sampled from poly and from prob
      poly2<-poly2[!poly2$id%in%drawn_sample2,]
      if (length(poly2)==0) break
    }
    if (verbose==2) message(paste("Picked",length(drawn_sample),"units"))
  }
  return(poly[drawn_sample,])
}
