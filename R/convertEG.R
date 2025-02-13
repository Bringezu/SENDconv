#' convertEG
#'
#' @param domainName A character vector that contains the acronym for the domain
#' @param domainData The data frame containing the export of Provantis
#'
#' @return SEND Converted Domain as data frame
#' @export
#'
#' @examples
#'  domainName<-'EG'
#'  SEND<-convertEG(domainData)
#'
convertEG<-function(domainData) {

  # load SEND names from Standard
  SEND_names<-rdSendig('EG')
  out_data<-tibble::as_tibble(domainData[[1]])
  
  
  names(out_data)[1]<-names(SEND_names[1]) # STUDYID
  names(out_data)[2]<-names(SEND_names[3]) # USUBJID
  names(out_data)[3]<-names(SEND_names[10]) # EGCAT
  names(out_data)[4]<-names(SEND_names[9]) # EGTEST
  names(out_data)[5]<-names(SEND_names[12]) # EGORRES
  names(out_data)[6]<-names(SEND_names[13]) # EGORRES
  names(out_data)[7]<-names(SEND_names[14]) # EGSTRESC
  names(out_data)[8]<-names(SEND_names[31]) # EGDTC
  names(out_data)[9]<-names(SEND_names[33]) # EGDY
  
  out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
  
  out_data<-out_data %>% tibble::add_column(DOMAIN='EG',.before="USUBJID") # add Domain column
  out_data<-out_data %>% tibble::add_column(EGTESTCD="",.before="EGTEST") # add EGTESTCD column
  out_data<-out_data %>% tibble::add_column(EGNOMDY="",.after="EGDY") # add EGNOMDY column
  
  # copy values from EGDY to EGNOMDY
  out_data$EGNOMDY<-out_data$EGDY
  
  # Set Date Fomat
  out_data$EGDTC<- format(as.POSIXct(out_data$EGDTC,format='%Y/%m/%d %H:%M:%S'))
  
  # align names according to standard 
  out_data<-out_data%>%
    dplyr::select(dplyr::any_of(names(SEND_names)))
  
  return(out_data)
}