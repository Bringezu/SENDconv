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
#'  SEND<-convertFW(domainData)
#'
convertFW<-function(domainData) {
  
  # load SEND names from Standard
  SEND_names<-rdSendig('FW')
  out_data<-tibble::as_tibble(domainData[[1]])

  
  names(out_data)[1]<-names(SEND_names)[1] # STUDYID
  names(out_data)[2]<-names(SEND_names)[4] # POOLID
  names(out_data)[3]<-names(SEND_names)[8] # FWTEST
  names(out_data)[4]<-names(SEND_names)[7] # FWTESTCD
  names(out_data)[5]<-names(SEND_names)[18] # FWDTC
  names(out_data)[6]<-names(SEND_names)[20] # FWDY
  names(out_data)[7]<-names(SEND_names)[9] # FWORRES
  names(out_data)[8]<-names(SEND_names)[10] # FWORRESU
  
  
  # out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
  
  out_data<-out_data %>% tibble::add_column(DOMAIN='FW',.before="POOLID") # add Domain column
  
  out_data<-out_data %>% dplyr::mutate(FWORRES=ifelse(is.na(FWORRES), 'Food Residue',FWORRES ))
  out_data<-out_data %>% dplyr::mutate(FWORRESU=ifelse(FWORRESU=="", 'Units (Residue)', FWORRESU))
  
  out_data<-out_data %>% dplyr::mutate(FWSTRESN = suppressWarnings(as.numeric(out_data$FWORRES), FWSTRESU = FWORRESU))
  out_data<-out_data %>% dplyr::mutate(FWSTRESC = as.character(out_data$FWORRES))
  
 
  # Set Date Fomat
  out_data$FWDTC<- format(as.POSIXct(out_data$FWDTC,format='%Y/%m/%d %H:%M:%S'))
  
  
  # align names according to standard 
  out_data<-out_data%>%
    dplyr::select(dplyr::any_of(names(SEND_names)))
  
  return(out_data)
  
}