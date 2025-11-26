#' convert_BASF_EX
#'
#' @param domainName A character vector that contains the acronym for the domain
#' @param domainData The data frame containing the export of Provantis
#'
#' @return SEND Converted Domain as data frame
#' @export
#'
#' @examples
#'  domainName<-'EX'
#'  domainData<-loadDomain(domainName)
#'  SEND<-convertMA(domainName, domainData)
#'
convert_BASF_EX<-function(domainName, domainData) {
  stopifnot(is.character(domainName), length(domainName) ==1)
  SEND_names<-rdSendig('EX')
  out_data<-tibble::as_tibble(domainData)
  
  names(out_data)[1]<-names(SEND_names[1]) # STUDYID
  names(out_data)[5]<-names(SEND_names[3]) # USUBJID
  names(out_data)[9]<-names(SEND_names[26]) # EXSTDY
  names(out_data)[10]<-names(SEND_names[21]) # RPHASE
  names(out_data)[11]<-names(SEND_names[28]) # EXRPSTDY
  names(out_data)[12]<-names(SEND_names[24]) # EXSTDTC
  names(out_data)[17]<-names(SEND_names[18]) # EXVAMT
  names(out_data)[18]<-names(SEND_names[19]) # EXVAMTU
  

  out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
  out_data<-out_data %>% tibble::add_column(DOMAIN='EX',.before="USUBJID") # add Domain column
  
  # remove unused columns
  out_data<-out_data %>% dplyr::select(any_of(names(SEND_names)))
  
  # set correct date fomat
  Sys.setlocale("LC_ALL", "English")
  out_data$EXSTDTC<- format(as.POSIXct(out_data$EXSTDTC,format="%d-%b-%Y %H:%M"))
  Sys.setlocale("LC_ALL", "de_DE.UTF-8")
  
  # toupper all relevant cols
  out_data<-out_data %>% dplyr::mutate_all(.funs=toupper)
  
  return(out_data)
}