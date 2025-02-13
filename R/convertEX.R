#' convertEX
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
#'  SEND<-convertEX(domainData)
#'
convertEX<-function(domainData) {

  
  # load SEND names from Standard
  SEND_names<-rdSendig('EX')
  out_data<-tibble::as_tibble(domainData[[1]])
  
  out_data<-tibble::as_tibble(domainData[[1]])
  names(out_data)[1]<-names(SEND_names[1]) # STUDYID
  names(out_data)[2]<-names(SEND_names[3]) # USUBJID
  names(out_data)[3]<-names(SEND_names[9]) # EXDOSTXT
  names(out_data)[4]<-names(SEND_names[21]) # EXSTDTC
  names(out_data)[5]<-names(SEND_names[23]) # EXSTDY
  # names(out_data)[6]<-SEND_names[[13]] # TIME-sLOT UNCHANED 
  names(out_data)[7]<-names(SEND_names[10]) # EXDOSU
  names(out_data)[9]<-names(SEND_names[8]) # EXDOSE
  names(out_data)[13]<-names(SEND_names[7]) # EXTRT
  names(out_data)[14]<-names(SEND_names[18]) # EXVAMT
  names(out_data)[15]<-names(SEND_names[13]) # EXROUTE
  names(out_data)[16]<-names(SEND_names[19]) # EXVAMTU
  names(out_data)[18]<-names(SEND_names[17]) # EXTRTV
  
  out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
    out_data<-out_data %>% tibble::add_column(DOMAIN='EX',.before="USUBJID") # add Domain column
    # remove unused columns
  out_data<-out_data %>% dplyr::select(-c('Time Slot', 
                                   'Calculated Dose',
                                   'Dose Body Weight', 
                                   'Body Weight Unit', 
                                   'Group Number', 
                                   'Dose Colour'))
  
  out_data$EXTRTV<-toupper(iconv(out_data$EXTRTV, "LATIN2", "UTF-8"))
  out_data$EXDOSTXT<-toupper(iconv(out_data$EXDOSTXT, "LATIN2", "UTF-8"))
  out_data$EXROUTE<-toupper(iconv(out_data$EXROUTE, "LATIN2", "UTF-8"))
  out_data$EXDOSU<-toupper(iconv(out_data$EXDOSU, "LATIN2", "UTF-8"))
  out_data$EXVAMTU<-toupper(iconv(out_data$EXVAMTU, "LATIN2", "UTF-8"))
  
  # Set Date Fomat
  out_data$EXSTDTC<- format(as.POSIXct(out_data$EXSTDTC,format='%Y/%m/%d %H:%M:%S'))
  
  
  # align names according to standard 
  out_data<-out_data%>%
    dplyr::select(dplyr::any_of(names(SEND_names)))
  return(out_data)
}