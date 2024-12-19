#' convert_BASFF_FW
#'
#' @param domainName A character vector that contains the acronym for the domain
#' @param domainData The data frame containing the export of Provantis
#'
#' @return SEND Converted Domain as dataframe
#' @export
#'
#' @examples
#'  domainName<-'FW'
#'  domainData<-loadDomain(domainName)
#'  SEND<-convertFW(domainName, domainData)
#'
convert_BASF_FW<-function(domainName, domainData) {
  stopifnot(is.character(domainName), length(domainName) ==1)
  SEND_names <-unlist(dictionary %>% dplyr::filter(`Domain Prefix`=="FW") %>% dplyr::select(`Variable Name`))
  out_data<-tibble::as_tibble(domainData[[1]])
  
  names(out_data)[1]<-SEND_names[[1]] # STUDYID
  names(out_data)[6]<-SEND_names[[3]] # USUBJID
  names(out_data)[15]<-SEND_names[[4]] # POOLID
  names(out_data)[14]<-SEND_names[[8]] # FWTEST
  names(out_data)[13]<-SEND_names[[18]] # FWDTC
  names(out_data)[12]<-SEND_names[[20]] # FWDY
  names(out_data)[19]<-SEND_names[[9]] # FWORRES
  names(out_data)[20]<-SEND_names[[10]] # FWORRESU
  
  
  out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
  
  # out_data<-out_data %>% tibble::add_column(DOMAIN='FW',.before="USUBJID") # add Domain column
  out_data<-out_data %>% dplyr::mutate(FWTESTCD = ifelse(FWTEST == 'Food', 'FOOD' , ''))
  
  out_data<-out_data %>% dplyr::mutate(FWSTRESN = as.numeric(out_data$FWORRES), FWSTRESU = FWORRESU)
  out_data<-out_data %>% dplyr::mutate(FWSTRESC = as.character(out_data$FWORRES))
  
  # remove unused columns
  out_data<-out_data %>% dplyr::select(any_of(unname(SEND_names)))
  
  
  return(out_data)
  
}