#' convert_BASF_DM
#' This module converts the Provantis Export for Demographics to a SEND like structure leading to a DM Domain.
#' @param domainName A character vector that contains the acronym for the domain
#' @param domainData The data frame containing the csv export of Provantis
#'
#' @return SEND Converted Domain as data frame
#' @export
#' @examples
#'  SEND<-convertDM(domainData)
#'
convert_BASF_DM<-function(domainData) {
  
  # load SEND names from Standard
  SEND_names<-rdSendig('DM')
  out_data<-tibble::as_tibble(domainData)
  
  names(out_data)[1]<-names(SEND_names[1]) # STUDYID
  names(out_data)[5]<-names(SEND_names[3]) # USUBJID
  names(out_data)[7]<-names(SEND_names[14]) # SEX
  names(out_data)[2]<-names(SEND_names[18]) # ARMCD
  names(out_data)[4]<-names(SEND_names[19]) # ARM
  names(out_data)[11]<-names(SEND_names[10]) # BRTHDTC
  names(out_data)[10]<-names(SEND_names[5]) # RFSTDTC
  names(out_data)[13] <-names(SEND_names[6])# RFENDTC
  names(out_data)[15] <-names(SEND_names[7]) # RFXSTDTC
  names(out_data)[17] <-names(SEND_names[8])# RFXENDTC
  names(out_data)[8]<-names(SEND_names[15]) # SPECIES
  names(out_data)[9]<-names(SEND_names[16]) # STRAIN
  
  
  out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
  # browser()
  
  out_data<-out_data %>% tibble::add_column(DOMAIN='DM',.before="USUBJID") # add Domain column
  
  l<-Sys.getlocale()
  Sys.setlocale("LC_ALL", "English")
  date_format <- "%d-%b-%Y %H:%M"
  # # Convert Dates
  out_data$BRTHDTC<- format(as.POSIXct(out_data$BRTHDTC,format="%d-%b-%Y %H:%M"))
  out_data$RFXSTDTC<- format(as.POSIXct(out_data$RFXSTDTC,format="%d-%b-%Y %H:%M"))
  out_data$RFXENDTC<- format(as.POSIXct(out_data$RFXENDTC,format="%d-%b-%Y %H:%M"))
  out_data$RFENDTC<- format(as.POSIXct(out_data$RFENDTC,format="%d-%b-%Y %H:%M"))
  out_data$RFSTDTC<- format(as.POSIXct(out_data$RFSTDTC,format="%d-%b-%Y %H:%M"))
  # 
  Sys.setlocale("LC_ALL", "de_DE.UTF-8")
  out_data<-out_data%>%dplyr::mutate_if(is.character, toupper)
  out_data<-out_data %>% dplyr::mutate(AGE=as.integer(as.Date(RFSTDTC)-as.Date(BRTHDTC)))
  out_data<-out_data %>% dplyr::mutate(AGEU='DAYS')
  
  # align names according to standard 
  out_data<-out_data%>%
    dplyr::select(dplyr::any_of(names(SEND_names)))
  out_data$AGETXT<-'NULL'
  out_data$SBSTRAIN<-'NULL'
  out_data$SITEID<-'NULL'
  out_data$SETCD<-'NULL'
  
  return(out_data)
  
}