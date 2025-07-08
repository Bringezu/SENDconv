#' curateDTC
#' @param    d SEND domain data frame
#' @return dd data frame for domain with DTC columns formatted as PosixCT
#' @importFrom rlang :=
#' @importFrom stringr str_detect
#' @importFrom lubridate parse_date_time

#' @export
#'
#'
curateDTC<-function(d) {
  ## define Pattern for DTC fields
  pattern<-'DTC$'
  ## Select colnames from pattern
  dtc_fields<-colnames(d)[str_detect(colnames(d), pattern)]
  ## loop across fields found
  for (i in seq_along(dtc_fields)){
    dtc_f<-paste0(dtc_fields[i],'_orig')
    d<-d %>% dplyr::mutate(!!dtc_f:=get(dtc_fields[i]))
  }
  d<-d %>% dplyr::mutate_at(dtc_fields, ~parse_date_time(., orders=
                                                                      c("dmY",
                                                                        "mdy",
                                                                        "dmY_HMS",
                                                                        "ymd",
                                                                        "Ymd_HMS",
                                                                        "Ymd",
                                                                        "Y",
                                                                        "Ymd_HM",
                                                                        "ymd_HM")
  ))

  return(d)
}
