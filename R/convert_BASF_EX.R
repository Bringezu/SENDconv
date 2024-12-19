#' convert_BASF_EX
#'
#' @param domainName A character vector that contains the acronym for the domain
#' @param domainData The data frame containing the export of Provantis
#'
#' @return SEND Converted Domain as data frame
#' @export
#'
#' @examples
#'  domainName<-'EX'
#'  domainData<-loadDomain(domainName)
#'  SEND<-convertMA(domainName, domainData)
#'
convert_BASF_EX<-function(domainName, domainData) {
  stopifnot(is.character(domainName), length(domainName) ==1)
  SEND_names <-unlist(dictionary %>% dplyr::filter(`Domain Prefix`==domainName) %>% dplyr::select(`Variable Name`))
  out_data<-tibble::as_tibble(domainData[[1]])
  names(out_data)[1]<-SEND_names[[1]] # STUDYID
  names(out_data)[6]<-SEND_names[[3]] # USUBJID
  #names(out_data)[3]<-SEND_names[[9]] # EXDOSTXT
  names(out_data)[12]<-SEND_names[[21]] # EXSTDTC
  names(out_data)[11]<-SEND_names[[23]] # EXSTDY
  # names(out_data)[6]<-SEND_names[[13]] # TIME-sLOT UNCHANED 
  names(out_data)[26]<-SEND_names[[10]] # EXDOSU
  names(out_data)[17]<-SEND_names[[8]] # EXDOSE
  names(out_data)[15]<-SEND_names[[7]] # EXTRT
  names(out_data)[17]<-SEND_names[[18]] # EXVAMT
  names(out_data)[25]<-SEND_names[[13]] # EXROUTE
  names(out_data)[18]<-SEND_names[[19]] # EXVAMTU
  #names(out_data)[18]<-SEND_names[[17]] # EXTRTV
  
  out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
  out_data<-out_data %>% tibble::add_column(DOMAIN='EX',.before="USUBJID") # add Domain column
  
  # remove unused columns
  out_data<-out_data %>% dplyr::select(any_of(unname(SEND_names)))
  
  
  
  # out_data$EXTRTV<-toupper(iconv(out_data$EXTRTV, "LATIN2", "UTF-8"))
  # out_data$EXDOSTXT<-toupper(iconv(out_data$EXDOSTXT, "LATIN2", "UTF-8"))
  # out_data$EXROUTE<-toupper(iconv(out_data$EXROUTE, "LATIN2", "UTF-8"))
  # out_data$EXDOSU<-toupper(iconv(out_data$EXDOSU, "LATIN2", "UTF-8"))
  # out_data$EXVAMTU<-toupper(iconv(out_data$EXVAMTU, "LATIN2", "UTF-8"))
  
  # Set Date Fomat
  # out_data$EXSTDTC<- format(as.POSIXct(out_data$EXSTDTC,format='%Y/%m/%d %H:%M:%S'))
  
  return(out_data)
}