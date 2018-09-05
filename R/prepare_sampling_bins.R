#' prepare_sampling_bins
#'
#' @param a
#' @param b
#' @param start_end
#' @keywords
#' @keywords
#' @export
#' @examples
#' @importFrom magrittr %>%
#'

prepare_sampling_bins<-function(a,b,start_end,number_of_bins=5) {

  p4s<-proj4string(a)

  polygon<-merge_area(a,b)

  ex_matrix<-extent(polygon) %>%
    as.matrix

  extent_polygon<-extent_to_polygon(polygon)

  line1_spatial<-start_end %>%
    coords_to_line(p4s)

  line1_math<-lm(y~x,start_end)$coefficient

  global_line<-new_line_thru_math(line1_math,c(-180,180),p4s)

    refined_line1<-crop(global_line,line1_spatial)

  # now shift line by 100k to the right and to the left
  # first take a point of refined_line1
  p1<-refined_line1@lines[[1]]@Lines[[1]]@coords[1,]
  p2<-refined_line1@lines[[1]]@Lines[[1]]@coords[2,]

  perp1<-perpendicular_line(line1_math,p1,1000,p4s)
  perp2<-perpendicular_line(line1_math,p2,1000,p4s)

  perp1_coords<-perp1@lines[[1]]@Lines[[1]]@coords %>%
    as.data.frame()

  perp2_coords<-perp2@lines[[1]]@Lines[[1]]@coords %>%
    as.data.frame()


  # get points on parallel lines in equal distances so
  # that they are split in 5 bins

  # parallel lines:
  # upper
  par1_coords<-rbind(perp1_coords[1,],
                     perp2_coords[1,]) %>%
    as.data.frame()

  par1_line_math<-lm(y~x,data=par1_coords)$coefficient

  par1<-par1_coords %>%
    coords_to_line(p4s)

  # lower
  par2_coords<-rbind(perp1_coords[2,],
                     perp2_coords[2,]) %>%
    as.data.frame()

  par2_line_math<-lm(y~x,data=par2_coords)$coefficient

  par2<-par2_coords %>%
    coords_to_line(p4s)

  # get x coordinates for points on parallel lines:
  x_coords_par1<-seq(par1_coords$x %>% min,
                     par1_coords$x %>% max,
                     length.out = number_of_bins+1)

  x_coords_par2<-seq(par2_coords$x %>% min,
                     par2_coords$x %>% max,
                     length.out = number_of_bins+1)

  # get y coordinates for points on paralell lines:
  points_on_par1<-data.frame(x=x_coords_par1,
                             y=par1_line_math[2]*x_coords_par1+par1_line_math[1])

  points_on_par2<-data.frame(x=x_coords_par2,
                             y=par2_line_math[2]*x_coords_par2+par2_line_math[1])




  bins1<-lapply(1:(nrow(points_on_par1)-1),function(x) {
    rbind(points_on_par1[x:(x+1),],
          points_on_par2[(x+1):x,]) %>%
      polygon_from_coords(p4s)
  }) %>%
    do.call(bind,.)

  # if parts of poly peak out
  if (!is.null(gDifference(polygon,bins1))) {


    # include overlaps in extra perps for ends
    # make 4 perpendicular lines to global line through extrem points of polygon_extent
    # delelete the lines that intersect with refined_line1
    # remaining lines are the end of global line
    extreme_points<-data.frame(x=c(ex_matrix[1,1],ex_matrix[1,1],ex_matrix[1,2],ex_matrix[1,2]),
                               y=c(ex_matrix[2,1],ex_matrix[2,2],ex_matrix[2,1],ex_matrix[2,2]))
    extreme_lines<-
      apply(extreme_points,1,perpendicular_line,coef_obj = line1_math,width_in_km = 1000,p4s = p4s)

    extreme_lines2<-extreme_lines[sapply(extreme_lines,function(x) {
      !gIntersects(x,refined_line1)
    })]

    if (length(extreme_lines2)>1) {

      extreme_lines3<-extreme_lines2 %>%
        do.call(rbind,.)

    } else {
      extreme_lines3<-extreme_lines2[[1]]
    }

    intersect_points<-gIntersection(global_line,extreme_lines3)
    intersect_coords<-intersect_points@coords
    refined_line2<-new_line_thru_math(line1_math,c(start_end[,1],intersect_coords[,1]) %>%
                                        sort,p4s)



    # refined_line2<-crop(global_line,extent(polygon))
    refined_line3<-refined_line2 %>%
      gDifference(buffer_shape(refined_line1,1/1e5)[[2]])

    # make perpendicular lines from the outer most refined_line3
    g<-1
    while((refined_line3@lines[[1]]@Lines %>% length)>=g) {
      point_for_perp<-refined_line3@lines[[1]]@Lines[[g]]@coords
      point_for_perp_buf<-point_for_perp %>%
        coords_to_line(p4s) %>%
        buffer_shape(10)

      merge_bin_number<-over(point_for_perp_buf[[2]],bins1)


      p1<-point_for_perp[which.max(point_for_perp[,1]),]

      if (merge_bin_number==1)
        p1<-point_for_perp[which.min(point_for_perp[,1]),]

      perp_add<-perpendicular_line(line1_math,p1,1000,p4s)

      perp_add_coords<-perp_add@lines[[1]]@Lines[[1]]@coords %>%
        as.data.frame


      bins_add<-rbind(points_on_par1[6,],
                      perp_add_coords[1,],
                      perp_add_coords[2,],
                      points_on_par2[6,]) %>%
        polygon_from_coords(p4s)

      if (merge_bin_number==1)
        bins_add<-rbind(perp_add_coords[1,],
                        points_on_par1[1,],
                        points_on_par2[1,],
                        perp_add_coords[2,]) %>%
        polygon_from_coords(p4s)


      if (merge_bin_number==1) {

        bins1<-bind(bind(bins1[merge_bin_number,],bins_add) %>%
                      gUnaryUnion(),
                    bins1[!c(1:length(bins1))%in%merge_bin_number,])

      } else {

        bins1<-bind(bins1[!c(1:length(bins1))%in%merge_bin_number,],
                    bind(bins1[merge_bin_number,],bins_add) %>%
                      gUnaryUnion())

      }
      g<-g+1
    }
  }


  a_cropped<-lapply(1:length(bins1),function(x) crop(a,bins1[x,])) %>%
    do.call(bind,.)

  b_cropped<-lapply(1:length(bins1),function(x) crop(b,bins1[x,])) %>%
    do.call(bind,.)

  bind(a_cropped,b_cropped)
}
