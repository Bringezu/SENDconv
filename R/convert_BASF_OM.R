#' convert_BASF_OM
#' This module converts the Provantis Export for Organ Measurements to a SEND like structure leading to a OM Domain.
#' @param domainName A character vector that contains the acronym for the domain
#' @param domainData The data frame containing the export of Provantis
#'
#' @return SEND Converted Domain as dataframe
#' @export
#'
#' @examples
#'  domainName<-'OM'
#'  domainData<-loadDomain(domainName)
#'  SEND<-convert_BASF_OM(domainName, domainData)
#'
convert_BASF_OM<-function(domainName, domainData) {
  stopifnot(is.character(domainName), length(domainName) ==1)
  SEND_names<-rdSendig('OM')
  out_data<-tibble::as_tibble(domainData)
  
  names(out_data)[1]<-names(SEND_names[1]) # STUDYID
  names(out_data)[5]<-names(SEND_names[3]) # USUBJID
  names(out_data)[10]<-names(SEND_names[23]) # RPHASE
  names(out_data)[11]<-names(SEND_names[29]) # OMRPDY
  names(out_data)[12]<-names(SEND_names[26]) # OMDY
  names(out_data)[13]<-names(SEND_names[25]) # OMDTC
  names(out_data)[17]<-names(SEND_names[14]) # OMSPEC
  names(out_data)[19]<-names(SEND_names[7]) # OMRES
  names(out_data)[20]<-names(SEND_names[8]) # OMRESU
  
  
  out_data<-out_data %>% tibble::add_column(DOMAIN='OM',.before="USUBJID") # add Domain column
  out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
  
  # remove unused columns
  out_data<-out_data %>% dplyr::select(any_of(names(SEND_names)))
  
  
  # toupper all relevant cols
  out_data$OMSPEC<-toupper(out_data$OMSPEC)
  
    # modify OMSTRESN
  suppressWarnings(out_data<- out_data %>% dplyr::mutate(OMSTRESN=as.numeric(OMORRES)))
  
  # modify OMSTRESC
  out_data<- out_data %>% dplyr::mutate(OMSTRESC=as.character(OMORRES))
  
  # set correct date fomat
  Sys.setlocale("LC_ALL", "English")
  out_data$OMDTC<- format(as.POSIXct(out_data$OMDTC,format="%d-%b-%Y %H:%M"))
  Sys.setlocale("LC_ALL", "de_DE.UTF-8")
  
  # toupper all relevant cols
  out_data<-out_data %>% dplyr::mutate_all(.funs=toupper)
  
  
  return(out_data)
}