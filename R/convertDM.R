#' convertDM
#' This module converts the Provantis Export for Demographics to a SEND like structure leading to a DM Domain.
#' @param domainName A character vector that contains the acronym for the domain
#' @param domainData The data frame containing the export of Provantis
#'
#' @return SEND Converted Domain as data frame
#'
#' @examples
#'  domainName<-'DM'
#'  domainData<-loadDomain(domainName)
#'  SEND<-convertMA(domainName, domainData)
#'
convertDM<-function(domainName, domainData) {
  stopifnot(is.character(domainName), length(domainName) ==1)
  SEND_names <-unlist(dictionary %>% dplyr::filter(`Domain Prefix`==domainName) %>% dplyr::select(`Variable Name`))
  out_data<-tibble::as_tibble(domainData[[1]])
  names(out_data)[1]<-SEND_names[[1]] # STUDYID
  names(out_data)[2]<-SEND_names[[3]] # USUBJID
  names(out_data)[3]<-SEND_names[[14]] # SEX
  names(out_data)[4]<-SEND_names[[10]] # BRTHDTC
  # not used names(out_data)[5]
  # not used names(out_data)[6]
  names(out_data)[7] <-SEND_names[[7]] # RFXSTDTC
  names(out_data)[8] <-SEND_names[[8]]# RFXENDTC
  # not used names(out_data)[9]
  # not used names(out_data)[10]
  # not used names(out_data)[11]
  # not used names(out_data)[12]
  names(out_data)[13]<-SEND_names[[18]] # ARMCD
  names(out_data)[14]<-SEND_names[[19]] # ARM
  names(out_data)[15]<-SEND_names[[15]] # SPECIES
  names(out_data)[16]<-SEND_names[[16]] # STRAIN
  
  out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
  
  out_data<-out_data %>% tibble::add_column(DOMAIN='DM',.before="USUBJID") # add Domain column
  
  # Convert Dates
  out_data$BRTHDTC<- format(as.POSIXct(out_data$BRTHDTC,format='%Y/%m/%d %H:%M:%S'))
  out_data$RFXSTDTC<- format(as.POSIXct(out_data$RFXSTDTC,format='%Y/%m/%d %H:%M:%S'))
  out_data$RFXENDTC<- format(as.POSIXct(out_data$RFXENDTC,format='%Y/%m/%d %H:%M:%S'))
  
  # remove unused columns
  out_data<-out_data %>% dplyr::select(-c('Time of Death','Animal Current Cage Number','Cage Exit Date','Group Number'))
  
  return(out_data)
  
}