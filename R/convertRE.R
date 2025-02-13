#' convertRE
#' This module converts the Provantis Export for Respiratory Data to a SEND like structure leading to a RE Domain.
#' @param domainName A character vector that contains the acronym for the domain
#' @param domainData The data frame containing the export of Provantis
#'
#' @return SEND Converted Domain as data frame
#' @export
#'
#' @examples
#'  domainName<-'RE'
#'  SEND<-convertMA(domainData)
#'
convertRE<-function(domainData) {
  
  # load SEND names from Standard
  SEND_names<-rdSendig('RE')
  out_data<-tibble::as_tibble(domainData[[1]])
  
  names(out_data)[1]<-names(SEND_names[1]) # STUDYID
  names(out_data)[2]<-names(SEND_names[3]) # USUBJID
  names(out_data)[3]<-names(SEND_names[7]) # Avtivity --> RETESTCD
  names(out_data)[4]<-names(SEND_names[8]) # Parameter --> RETEST
  names(out_data)[5]<-names(SEND_names[10]) # Result Value --> REORRES
  names(out_data)[6]<-names(SEND_names[11]) # Result Unit --> REORRESU
  names(out_data)[7]<-names(SEND_names[12]) # Textual Value --> RESTRESC
  names(out_data)[8]<-names(SEND_names[24]) # Result Time --> REDTC
  names(out_data)[9]<-names(SEND_names[26]) # Result Day --> REDY
  
  out_data<-out_data %>% tibble::add_column(DOMAIN='RE',.before="USUBJID") # add Domain column
  out_data<-out_data %>% tibble::add_column(RESTRESN='',.before="REDTC") # add RESTRESN column
  out_data<-out_data %>% tibble::add_column(RESTRESU='',.before="REDTC") # add RESTRESN column
  
  out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
  
  # capitalize
  out_data$RETESTCD<-toupper(out_data$RETESTCD)
  out_data$RETEST<-toupper(out_data$RETEST)
  
  out_data$RESTRESN<-as.numeric(out_data$REORRES)
  out_data$RESTRESU<-out_data$REORRESU
  
  # format date / time
  out_data$REDTC<- format(as.POSIXct(out_data$REDTC,format='%Y/%m/%d %H:%M:%S'))
  
  # align names according to standard 
  out_data<-out_data%>%
    dplyr::select(dplyr::any_of(names(SEND_names)))
  
  return(out_data)
}