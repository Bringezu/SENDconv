#' convert_BASF_MI
#'
#' @param domainName A character vector that contains the acronym for the domain
#' @param domainData The data frame containing the export of Provantis
#'
#' @return SEND Converted Domain as data frame
#' @export
#'
#' @examples
#'  domainName<-'MI'
#'  domainData<-loadDomain(domainName)
#'  SEND<-convertMA(domainName, domainData)
#'
convert_BASF_MI<-function(domainName, domainData) {
  stopifnot(is.character(domainName), length(domainName) ==1)
  SEND_names<-rdSendig('MI')
  out_data<-tibble::as_tibble(domainData)
  
  names(out_data)[1]<-names(SEND_names[1]) # STUDYID
  names(out_data)[5]<-names(SEND_names[3]) # USUBJID
  names(out_data)[8]<-names(SEND_names[32]) # MIDY
  names(out_data)[9]<-names(SEND_names[31]) # MIDTC
  names(out_data)[15]<-names(SEND_names[20]) # MISPEC
  names(out_data)[19]<-names(SEND_names[18]) # MIREASND
  names(out_data)[21]<-names(SEND_names[12]) # MIORRES
  names(out_data)[26]<-names(SEND_names[28]) # MIRESCAT
  names(out_data)[27]<-names(SEND_names[24]) # MILAT
  names(out_data)[29]<-names(SEND_names[16]) # MIDIST
  
  
  
  out_data<-out_data %>% tibble::add_column(DOMAIN='MI',.before="USUBJID") # add Domain column
  
  out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
  
  out_data<-out_data %>% dplyr::mutate(MIORRES = ifelse(`No lesions visible)` == 'Yes', 'UNREMARKABLE' , MIORRES))
  
  
  # remove unused columns
  out_data<-out_data %>% dplyr::select(any_of(names(SEND_names)))
  
  
  # set correct date fomat
  Sys.setlocale("LC_ALL", "English")
  out_data$MIDTC<- format(as.POSIXct(out_data$MIDTC,format="%d-%b-%Y %H:%M"))
  Sys.setlocale("LC_ALL", "de_DE.UTF-8")
  
  
  # toupper all relevant cols
  out_data<-out_data %>% dplyr::mutate_all(.funs=toupper)

  # 
  return(out_data)
}