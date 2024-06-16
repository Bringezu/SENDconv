#' convertFW
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
convertFW<-function(domainName, domainData) {
  stopifnot(is.character(domainName), length(domainName) ==1)
  SEND_names <-unlist(dictionary %>% dplyr::filter(`Domain Prefix`==domainName) %>% dplyr::select(`Variable Name`))
  out_data<-tibble::as_tibble(domainData[[1]])
  SEND_names <-unlist(dictionary %>% filter(`Domain Prefix`==domain) %>% select(`Variable Name`))
  
  names(out_data)[1]<-SEND_names[[1]] # STUDYID
  names(out_data)[2]<-SEND_names[[4]] # POOLID
  names(out_data)[3]<-SEND_names[[8]] # FWTEST
  names(out_data)[4]<-SEND_names[[7]] # FWTESTCD
  names(out_data)[5]<-SEND_names[[18]] # FWDTC
  names(out_data)[6]<-SEND_names[[20]] # FWDY
  names(out_data)[7]<-SEND_names[[9]] # FWORRES
  names(out_data)[8]<-SEND_names[[10]] # FWORRESU
  #names(out_data)[9]<-SEND_names[[33]] # EGDY
  
  # out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
  
  out_data<-out_data %>% tibble::add_column(DOMAIN='FW',.before="POOLID") # add Domain column
  
  out_data<-out_data %>% dplyr::mutate(FWORRES=ifelse(is.na(FWORRES), `Food Residue`,FWORRES ))
  out_data<-out_data %>% dplyr::mutate(FWORRESU=ifelse(FWORRESU=="", `Units (Residue)`, FWORRESU))
  
  out_data<-out_data %>% dplyr::mutate(FWSTRESN = as.numeric(out_data$FWORRES), FWSTRESU = FWORRESU)
  out_data<-out_data %>% dplyr::mutate(FWSTRESC = as.character(out_data$FWORRES))
  
  # remove unused columns
  out_data<-out_data %>% dplyr::select(-c('Units (Waste)','Food Waste', 'Food Residue','Units (Residue)'))
  
  # Set Date Fomat
  out_data$FWDTC<- format(as.POSIXct(out_data$FWDTC,format='%Y/%m/%d %H:%M:%S'))
  
  return(out_data)
  
}