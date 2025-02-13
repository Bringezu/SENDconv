#' convertVS
#' This module converts the Provantis Export for Vital Signs to a SEND like structure leading to a VS Domain.
#' @param domainName A character vector that contains the acronym for the domain
#' @param domainData The data frame containing the export of Provantis
#'
#' @return SEND Converted Domain as data frame
#' @export
#'
#' @examples
#'  SEND<-convertVS(domainData)
#'
convertVS<-function(domainData) {

  # load SEND names from Standard
  SEND_names<-rdSendig('VS')
  out_data<-tibble::as_tibble(domainData[[1]])
  
  names(out_data)[1]<-names(SEND_names[1]) # STUDYID
  names(out_data)[2]<-names(SEND_names[3]) # USUBJID
  names(out_data)[3]<-names(SEND_names[7]) # Avtivity --> VSTESTCD
  names(out_data)[4]<-names(SEND_names[8]) # Parameter --> VSTEST
  names(out_data)[5]<-names(SEND_names[12]) # Result Value --> VSORRES
  names(out_data)[6]<-names(SEND_names[13]) # Result Unit --> VSORRESU
  names(out_data)[7]<-names(SEND_names[14]) # Textual Value --> VSSTRESC
  names(out_data)[8]<-names(SEND_names[27]) # Result Time --> VSDTC
  names(out_data)[9]<-names(SEND_names[29]) # Result Day --> VSDY
  
  out_data<-out_data %>% tibble::add_column(DOMAIN='VS',.before="USUBJID") # add Domain column
  out_data<-out_data %>% tibble::add_column(VSSTRESN='',.before="VSDTC") # add RESTRESN column
  out_data<-out_data %>% tibble::add_column(VSSTRESU='',.before="VSDTC") # add RESTRESN column
  
  out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID

  # capitalize
  out_data$VSTESTCD<-toupper(out_data$VSTESTCD)
  out_data$VSTEST<-toupper(out_data$VSTEST)
  
  out_data$VSSTRESN<-as.numeric(out_data$VSORRES)
  out_data$VSSTRESU<-out_data$VSORRESU
  
  # format date / time
  out_data$VSDTC<- format(as.POSIXct(out_data$VSDTC,format='%Y/%m/%d %H:%M:%S'))
  
  # align names according to standard 
  out_data<-out_data%>%
    dplyr::select(dplyr::any_of(names(SEND_names)))
  
  return(out_data)
}