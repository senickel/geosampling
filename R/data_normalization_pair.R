#' data_normalization_pair
#'
#' @param sample_obj
#' @param name
#' @keywords
#' @keywords
#' @export
#' @examples
#' checking.nominal()
#' @importFrom magrittr %>%
#' @useDynLib geosampling
#' @importFrom Rcpp sourceCpp
#'
data_normalization_pair<-function(sample_obj,name) {
  sample_obj@data$country_id<-name

  sample_obj@data$Name<-sample_obj@data %>%
    dplyr::select(country_id,bin_id,unit3_id,unit2_id) %>%
    apply(1,function(x) {
      paste0(x[!is.na(x)],collapse="@") %>%
        gsub(" ","",.)
    })

  sample_obj@data$type<-sample_obj@data %>%
    dplyr::select(contains("is_")) %>%
    apply(1,function(x) {
      if(x[1]) return("Bin")
      if(x[2]) return("Unit_3")
      return("Unit_2")
    })

  sample_obj@data$pop<-sample_obj@data %>%
    dplyr::select(contains("population")) %>%
    apply(1,function(x) {
      if (!is.na(x[3])) return(x[3])
      if (!is.na(x[2])) return(x[2])
      x[1]
    }) %>%
    as.numeric

  sample_obj@data$prob<-sample_obj@data %>%
    dplyr::select(contains("probability")) %>%
    apply(1,function(x) {
      if (!is.na(x[3])) return(x[3])
      if (!is.na(x[2])) return(x[2])
      x[1]
    }) %>%
    as.numeric



  sample_obj@data<-sample_obj@data[,!grepl("_",colnames(sample_obj@data))]

  sample_obj <- .add_centroid(sample_obj)

  sample_obj@data$Description<-sample_obj@data %>%
    apply(1,function(x) paste0("Type: ",x[2],"; Probability:",round(x[4] %>%
                                                                      as.numeric,2),
                               "; Lat/Long: ",paste(x[6],x[5],sep=", ")))

  sample_obj
}
