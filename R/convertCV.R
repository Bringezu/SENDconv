#' convertCV
#' This module converts the Provantis Export for Cardiovascular Data to a SEND like structure leading to a CV Domain.
#' @param domainName A character vector that contains the acronym for the domain
#' @param domainData The data frame containing the export of Provantis
#'
#' @return SEND Converted Domain as data frame
#' @export
#'
#' @examples
#'  domainName<-'CV'
#'  domainData<-loadDomain(domainName)
#'  SEND<-convertMA(domainName, domainData)
#'
convertCV<-function(domainData) {
  # stopifnot(is.character(domainName), length(domainName) ==1)
  # SEND_names <-unlist(dictionary %>% dplyr::filter(`Domain Prefix`==domainName) %>% dplyr::select(`Variable Name`))

  out_data<-tibble::as_tibble(domainData[[1]])
  # load SEND names from Standard
  SEND_names<-rdSendig('CV')

  names(out_data)[1]<-names(SEND_names[1]) # STUDYID
  names(out_data)[2]<-names(SEND_names[3]) # USUBJID
  # names(out_data)[3]<-SEND_names[[6]] # CVTEST
  names(out_data)[4]<-names(SEND_names[8]) # CVTEST
  names(out_data)[5]<-names(SEND_names[10]) # CVORRES
  names(out_data)[6]<-names(SEND_names[11]) # CVORRESU
  names(out_data)[7]<-names(SEND_names[12]) # CVSTRESC
  names(out_data)[8]<-names(SEND_names[24]) # CVDTC
  names(out_data)[9]<-names(SEND_names[26]) # CVDY
  
  out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
  
  out_data<-out_data %>% tibble::add_column(DOMAIN='CV',.before="USUBJID") # add Domain column
  out_data<-out_data %>% tibble::add_column(CVNOMDY="",.after="CVDY") # add CVNOMDY column
  out_data<-out_data %>% tibble::add_column(CVSTRESN="",.after="CVSTRESC") # add CVSTRESN column
  out_data<-out_data %>% tibble::add_column(CVSTRESU="",.after="CVSTRESN") # add CVSTRESU column
  
  
  # copy values from CVORRES to CVSTRESN
  out_data$CVSTRESN<-as.double(out_data$CVORRES)
  
  # copy values from CVORRESU to CVSTRESU
  out_data$CVSTRESU<-out_data$CVORRESU
  
  # copy values from CVNOMDY to CVDY
  out_data$CVNOMDY<-out_data$CVDY
  
  # Format CVDTC as POSIXct Date
  out_data$CVDTC<- format(as.POSIXct(out_data$CVDTC,format='%Y/%m/%d %H:%M:%S'))
  
  # align names according to standard 
  out_data<-out_data%>%
    dplyr::select(dplyr::any_of(names(SEND_names)))
  
  return(out_data)
  
}