#' convertVS
#' This module converts the Provantis Export for Vital Signs to a SEND like structure leading to a VS Domain.
#' @param domainName A character vector that contains the acronym for the domain
#' @param domainData The data frame containing the export of Provantis
#'
#' @return SEND Converted Domain as data frame
#' @export
#'
#' @examples
#'  domainName<-'VS'
#'  domainData<-loadDomain(domainName)
#'  SEND<-convertMA(domainName, domainData)
#'
convertVS<-function(domainName, domainData) {
  stopifnot(is.character(domainName), length(domainName) ==1)
  SEND_names <-unlist(dictionary %>% dplyr::filter(`Domain Prefix`==domainName) %>% dplyr::select(`Variable Name`))
  out_data<-tibble::as_tibble(domainData[[1]])
  names(out_data)[1]<-SEND_names[[1]] # STUDYID
  names(out_data)[2]<-SEND_names[[3]] # USUBJID
  names(out_data)[3]<-SEND_names[[7]] # Avtivity --> VSTESTCD
  names(out_data)[4]<-SEND_names[[8]] # Parameter --> VSTEST
  names(out_data)[5]<-SEND_names[[12]] # Result Value --> VSORRES
  names(out_data)[6]<-SEND_names[[13]] # Result Unit --> VSORRESU
  names(out_data)[7]<-SEND_names[[14]] # Textual Value --> VSSTRESC
  names(out_data)[8]<-SEND_names[[27]] # Result Time --> VSDTC
  names(out_data)[9]<-SEND_names[[29]] # Result Day --> VSDY
  
  out_data<-out_data %>% tibble::add_column(DOMAIN='VS',.before="USUBJID") # add Domain column
  out_data<-out_data %>% tibble::add_column(VSSTRESN='',.before="VSDTC") # add RESTRESN column
  out_data<-out_data %>% tibble::add_column(VSSTRESU='',.before="VSDTC") # add RESTRESN column
  
  out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
  
  # remove unused columns
  out_data<-out_data %>% dplyr::select(-c('Time Slot'))
  
  # capitalize
  out_data$VSTESTCD<-toupper(out_data$VSTESTCD)
  out_data$VSTEST<-toupper(out_data$VSTEST)
  
  out_data$VSSTRESN<-as.numeric(out_data$VSORRES)
  out_data$VSSTRESU<-out_data$VSORRESU
  
  # format date / time
  out_data$VSDTC<- format(as.POSIXct(out_data$VSDTC,format='%Y/%m/%d %H:%M:%S'))
  
  return(out_data)
}