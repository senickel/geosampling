#' prepare_sampling_bins_city
#'
#' @param adm0
#' @param coords
#' @param lakes
#' @keywords
#' @keywords
#' @export
#' @examples
#' @importFrom magrittr %>%
#' @importFrom magrittr %>%
#'
prepare_sampling_bins_city<-function(adm0,coords,lakes) {
  p4s<-proj4string(adm0)

  if (any(is.na(suppressWarnings(as.numeric(coords))))) stop("Coordinates are not numeric.")
  if (!is(adm0,"SpatialPolygons")) stop("adm0 is not a SpatialPolygon")
  if (!is(lakes,"SpatialPolygons")) stop("lakes is not a SpatialPolygon")

 if (is.data.frame(coords)|is.vector(coords)) {
    coords<-coords %>%
      as.numeric %>%
      as.matrix %>%
      t
  } else if (!is.matrix(coords)) {
    stop("coords must be a vector of length 2, a matrix of dim(1,2), or a data.frame of dim(1,2)")
  }
  city<-coords %>%
    SpatialPoints(proj4string = CRS(p4s))

  part_0_to_25_1=prepare_area_city(adm0,city,lakes,width_in_km = 25)

  part_0_to_25=prepare_border_polygon(part_0_to_25_1,adm0,lakes)


  part_25_to_50_1=prepare_area_city(adm0,city,lakes,width_in_km = 50)

  part_25_to_50_2=prepare_border_polygon(part_25_to_50_1,adm0,lakes)

  part_25_to_50=gDifference(part_25_to_50_2,part_0_to_25)

  line_coords<-
    rbind(coords,
          coords) %>%
    as.data.frame

  south_north=line_coords
  south_north[,2]<-south_north[,2] + c(5,-5)

  east_west=line_coords
  east_west[,1]<-east_west[,1] + c(5,-5)
  south_north_line<-coords_to_line(south_north,p4s)
  east_west_line<-coords_to_line(east_west,p4s)


  cross_extent<-rbind(south_north_line,east_west_line) %>%
    extent

  bins1<-list(rbind(coords,
                    east_west[2,],
                    c(cross_extent[1],cross_extent[4]),
                    south_north[1,]) %>%
                polygon_from_coords(p4s=p4s),
              rbind(coords,
                    east_west[1,],
                    c(cross_extent[2],cross_extent[4]),
                    south_north[1,]) %>%
                polygon_from_coords(p4s=p4s),
              rbind(coords,
                    east_west[1,],
                    c(cross_extent[2],cross_extent[3]),
                    south_north[2,]) %>%
                polygon_from_coords(p4s=p4s),
              rbind(coords,
                    east_west[2,],
                    c(cross_extent[1],cross_extent[3]),
                    south_north[2,]) %>%
                polygon_from_coords(p4s=p4s)) %>%
    do.call(bind,.)

  a_cropped<-lapply(1:length(bins1),function(x) {
    crop(part_0_to_25,bins1[x,])
  })
  a_cropped_all<-a_cropped[!sapply(a_cropped,is.null)]
  if (length(a_cropped_all)>1) {
    a_cropped2<-a_cropped_all %>%
      do.call(bind,.)
  } else {
    a_cropped2<-a_cropped_all[[1]]
  }


  b_cropped<-lapply(1:length(bins1),function(x) {
    crop(part_25_to_50,bins1[x,])
  })
  b_cropped_all<-b_cropped[!sapply(b_cropped,is.null)]
  if (length(b_cropped_all)>1) {
    b_cropped2<-b_cropped_all %>%
      do.call(bind,.)
  } else {
    b_cropped2<-b_cropped_all[[1]]
  }


  bind(a_cropped2,b_cropped2)
}
