#' prepare_100m_eyeballing
#'
#' @param obj_100m SpatialPolygonsDataFrame
#' @keywords
#' @keywords
#' @export
#' @examples
#' @importFrom magrittr %>%
#'

prepare_100m_eyeballing <- function(obj_100m) {

  dat2 <- obj_100m@data %>%
    dplyr::select(long,lat,Name,type) %>%
    mutate(lat = as.numeric(lat),
           long = as.numeric(long))


  distance <- 0.0008333

  # define corners
  unit2s <- dat2 %>%
    filter(type == "Unit_2") %>%
    dplyr::select(Name) %>%
    unlist()

  select_matrix <- matrix(1:100,ncol=10,byrow = TRUE)


  only100s <- lapply(unit2s,function(u1) {
    dat3 <- dat2 %>%
      filter(Name == u1)

    longlat_u1 <- dat3 %>%
      dplyr::select(long,lat)
    longs <- c(longlat_u1$long + distance * 4.5,
               longlat_u1$long - distance * 4.5) %>%
      sort
    lats <- c(longlat_u1$lat + distance * 4.5,
              longlat_u1$lat - distance * 4.5) %>%
      sort

    grid_lats <- seq(lats[1],lats[2],distance) %>%
      sort(decreasing = TRUE)

    grid_longs <- seq(longs[1],longs[2],distance) %>%
      sort

    p1s <- dat2 %>%
      filter(grepl(paste0(u1,"@"),Name))

    p1s$geo_order <- sapply(p1s$Name,function(p1) {
      p2 <- p1s %>%
        filter(Name==p1)
      select_matrix[abs(grid_lats-p2$lat) %>% which.min,
                    abs(grid_longs-p2$long) %>% which.min]
    })
    p1s
  }) %>%
    do.call(rbind,.)

  only100s2 <- only100s %>%
    dplyr::select(Name,type,geo_order,long,lat) %>%
    mutate(Topunit = strsplit(Name,"@") %>%
             lapply(function(x) paste(x[1],x[2],x[3],sep = "@")) %>%
             unlist())

  only100s3 <- only100s2 %>%
    dplyr::select(Topunit,Name,geo_order) %>%
    mutate(Full = 0) %>%
    arrange(Name)

  only100s4 <- only100s3$Topunit %>% unique %>%
    lapply(function(x) {
      help_df <- only100s3 %>%
        filter(Topunit == x)
      data.frame(Topname=x,
                 Name = paste0(help_df$Name,collapse=";"),
                 geo_order = paste0(help_df$geo_order,collapse=";"),
                 Full = paste0(help_df$Full,collapse=";"))

    }) %>%
    do.call(rbind,.)

  return(only100s4)
}
