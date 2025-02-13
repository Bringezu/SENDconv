#' convertMA
#'
#' @param domainName A character vector that contains the acronym for the domain
#' @param domainData The data frame containing the export of Provantis
#'
#' @return SEND Converted Domain as dataframe
#' @export
#'
#' @examples
#'  domainName<-'MA'
#'  SEND<-convertMA(domainData)
#'
convertMA<-function(domainData) {
  
  # load SEND names from Standard
  SEND_names<-rdSendig('MA')
  out_data<-tibble::as_tibble(domainData[[1]])
  
  
  names(out_data)[1]<-names(SEND_names[1]) # STUDYID
  names(out_data)[2]<-names(SEND_names[3]) # USUBJID
  names(out_data)[3]<-names(SEND_names[17]) # MASPEC
  names(out_data)[4]<-names(SEND_names[14]) # MASTAT
  names(out_data)[6]<-names(SEND_names[12]) # MAORRES
  # names(out_data)76]<-SEND_names[[]] # NVL--> MAORRES
  names(out_data)[8]<-names(SEND_names[13]) # MASTRESC
  # names(out_data)[9]<-SEND_names[[18]] # QUALIFIER --> MAANTREG
  names(out_data)[10]<-names(SEND_names[18]) # MAANTREG
  names(out_data)[11]<-names(SEND_names[22]) # Sub-Locator --> MADIR 
  names(out_data)[12]<-names(SEND_names[21]) # MALAT
  # names(out_data)[13]<-SEND_names[[18]] # Distribution --> MAANTREG
  names(out_data)[14]<-names(SEND_names[25]) # MASEV
  names(out_data)[18]<-names(SEND_names[27]) # MADTC
  names(out_data)[19]<-names(SEND_names[28]) # MADY
  
  out_data<-out_data %>% tibble::add_column(DOMAIN='MA',.before="USUBJID") # add Domain column
  
  out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
  out_data<-out_data %>% dplyr::mutate(MAORRES = ifelse(NVL == 'No Visible Lesions', 'NO VISIBLE LESIONS' , MAORRES))
  out_data<-out_data %>% dplyr::mutate(MAORRES = ifelse(MASTAT == 'Not Examined', 'NOT EXAMINED' , MAORRES))
  out_data<-out_data %>% dplyr::mutate(MAANTREG = ifelse(Distribution != '' & MAANTREG !='', paste0(MAANTREG,', ',Distribution), MAANTREG))
  out_data<-out_data %>% dplyr::mutate(MAANTREG = ifelse(Distribution != '' & MAANTREG =='', paste0(Distribution), MAANTREG))
  out_data<-out_data %>% dplyr::mutate(MAANTREG = ifelse(Qualifier != '', paste0(MAANTREG,', ',Qualifier), MAANTREG))
  
  out_data<-out_data %>% dplyr::mutate(MALAT = ifelse(MADIR=='left', MADIR, MALAT))
  out_data<-out_data %>% dplyr::mutate(MADIR = replace(MADIR, MADIR == "left", ""))
  
  
  out_data<-out_data %>% dplyr::mutate(MALAT = ifelse(MADIR=='right', MADIR, MALAT))
  out_data<-out_data %>% dplyr::mutate(MADIR = replace(MADIR, MADIR == "right", ""))
  out_data<-out_data %>% dplyr::mutate(MASTRESC = ifelse(MAORRES == 'NO VISIBLE LESIONS', 'UNREMARKABLE', MASTRESC))
  
  # copy values from CVNOMDY to CVDY
  out_data$MANOMDY<-out_data$MADY
  
  # toupper all relevant cols
  out_data$MAORRES<-toupper(out_data$MAORRES)
  out_data$MASPEC<-toupper(out_data$MASPEC)
  out_data$MASTRESC<-toupper(out_data$MASTRESC)
  out_data$MALAT<-toupper(out_data$MALAT)
  out_data$MASEV<-toupper(out_data$MASEV)
  out_data$MASTAT<-toupper(out_data$MASTAT)
  out_data$MAANTREG<-toupper(out_data$MAANTREG)
  out_data$MADIR<-toupper(out_data$MADIR)
  
  
  
  # Set Date Fomat
  out_data$MADTC<- format(as.POSIXct(out_data$MADTC,format='%Y/%m/%d %H:%M:%S'))
  
  
  # align names according to standard 
  out_data<-out_data%>%
    dplyr::select(dplyr::any_of(names(SEND_names)))
  
  return(out_data)
}