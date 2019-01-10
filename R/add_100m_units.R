#' add_100m_units
#'
#' @param spj_obj
#' @keywords
#' @keywords
#' @export
#' @examples
#' @importFrom magrittr %>%
#' @importFrom glue glue
#'

add_100m_units <- function(spj_obj,pop) {
  spj_obj_sf <- spj_obj %>%
    st_as_sf()

  unit2_sf <- c(1:sum(spj_obj_sf$type=="Bin")) %>%
    lapply(function(bin_number) {

      bin1 <- spj_obj_sf %>%
        filter(type=="Bin") %>%
        slice(bin_number)


      unit2 <- spj_obj_sf %>%
        filter(grepl(bin1$Name,Name)&type=="Unit_2")


      units <- lapply(1:nrow(unit2),function(unit_number) {

        bin1_raster <- crop(pop,
                            bin1 %>%
                              as("Spatial"))

        samp_unit <- unit2 %>%
          slice(unit_number) %>%
          as("Spatial") %>%
          crop(bin1 %>%
                 as("Spatial"))

        sample_raster_1 <- crop(bin1_raster,samp_unit)

        sample_raster_2 <- setValues(sample_raster_1,
                                     ifelse(getValues(sample_raster_1)==0,0.0001,
                                            getValues(sample_raster_1)))

        masked <- fasterize::fasterize(sf = samp_unit %>%
                                         st_as_sf(),
                                       raster = sample_raster_2) %>%
          mask(sample_raster_2) %>%
          getValues()

        sample_raster <-
          setValues(sample_raster_2,
                    ifelse(is.na(masked),0.0001,getValues(sample_raster_2)))


        res <- geosampling::sample_from_raster(
          raster_file = sample_raster,
          poly_shape = samp_unit,
          sample_size = 100,
          seed = 680)


        k1unit <- samp_unit[,"Name"]

        res2 <- reshape_poly(poly=res,
                             poly_before =k1unit, # add ids, prob, pop and unify colnames
                             name_of_unit = "unit1") %>%
          st_as_sf %>%
          mutate(Name = paste(Name,unit1_id,sep="@"))

        res2
      }) %>%
        do.call(rbind,.)

      units
    }) %>%
    do.call(rbind,.)



  unit2_sf <- unit2_sf %>%
    mutate(type = "Unit_1") %>%
    rename(pop = unit1_population,
           prob = unit1_probability)

  unit2_sf <- .add_centroid(unit2_sf %>% as("Spatial"))

  unit2_sf <- unit2_sf %>%
    st_as_sf() %>%
    mutate(Description =
             glue("Type: {type}; Probablity: {round(prob,2)}; Lat/Long: {lat}, {long}"),
           pick = "",
           almost_empty = "")

  rbind(spj_obj_sf,unit2_sf %>%
          dplyr::select(-unit1_id,-is_unit1)) %>%
    as("Spatial")
}
