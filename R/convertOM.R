#' convertOM
#' This module converts the Provantis Export for Organ Measurements to a SEND like structure leading to a OM Domain.
#' @param domainName A character vector that contains the acronym for the domain
#' @param domainData The data frame containing the export of Provantis
#'
#' @return SEND Converted Domain as dataframe
#' @export
#'
#' @examples
#'  domainName<-'OM'
#'  SEND<-convertOM(domainData)
#'
convertOM<-function(domainData) {
  
  # load SEND names from Standard
  SEND_names<-rdSendig('OM')
  out_data<-tibble::as_tibble(domainData[[1]])
  
  names(out_data)[1]<-names(SEND_names[1]) # STUDYID
  names(out_data)[2]<-names(SEND_names[3]) # USUBJID
  names(out_data)[3]<-names(SEND_names[26]) # Avtivity --> OMNOMLBL 
  names(out_data)[4]<-names(SEND_names[14]) # Parameter --> OMSPEC
  names(out_data)[5]<-names(SEND_names[7]) # Result Value --> OMORRES
  names(out_data)[6]<-names(SEND_names[8]) # Result Unit --> OMORRESU
  names(out_data)[7]<-names(SEND_names[9]) # Textual Value --> OMORRESC
  names(out_data)[8]<-names(SEND_names[23]) # Result Time --> OMDTC
  names(out_data)[9]<-names(SEND_names[24]) # Result Day --> OMDY
  
  out_data<-out_data %>% tibble::add_column(DOMAIN='OM',.before="USUBJID") # add Domain column
  out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
  
  # remove unused columns
  out_data<-out_data %>% dplyr::select(-c('Time Slot'))
  
  
  #add result in different formats
  out_data<-out_data %>% dplyr::mutate(OMSTRESC=as.character(OMORRES))
  out_data<-out_data %>% dplyr::mutate(OMSTRESN=as.numeric(OMORRES))
  
  # toupper all relevant cols
  out_data$OMSPEC<-toupper(out_data$OMSPEC)
  out_data$OMNOMLBL<-toupper(out_data$OMNOMLBL)
  
  # format date / time
  out_data$OMDTC<- format(as.POSIXct(out_data$OMDTC,format='%Y/%m/%d %H:%M:%S'))
  
  # align names according to standard 
  out_data<-out_data%>%
    dplyr::select(dplyr::any_of(names(SEND_names)))
  
  return(out_data)
}