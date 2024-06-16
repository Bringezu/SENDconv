#' convertMI
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
convertMI<-function(domainName, domainData) {
  stopifnot(is.character(domainName), length(domainName) ==1)
  SEND_names <-unlist(dictionary %>% dplyr::filter(`Domain Prefix`==domainName) %>% dplyr::select(`Variable Name`))
  out_data<-tibble::as_tibble(domainData[[1]])
  names(out_data)[1]<-SEND_names[[1]] # STUDYID
  names(out_data)[2]<-SEND_names[[3]] # USUBJID
  names(out_data)[3]<-SEND_names[[12]] # MIORRES
  names(out_data)[4]<-SEND_names[[20]] # MISPEC
  names(out_data)[5]<-SEND_names[[17]] # MISTAT
  names(out_data)[8]<-SEND_names[[13]] # Morphology --> MISTRESC
  names(out_data)[9]<-SEND_names[[21]] # Locator --> MIANTREG
  names(out_data)[10]<-SEND_names[[25]] # sub-locator --> MIDIR
  names(out_data)[13]<-SEND_names[[24]] # Symmetry -> MILAT
  names(out_data)[14]<-SEND_names[[16]] # MIDISTR
  # names(out_data)[15]<-SEND_names[[16]] # Qualifier --> ??
  names(out_data)[16]<-SEND_names[[28]] # MISEV
  names(out_data)[17]<-SEND_names[[30]] # MIDTC
  names(out_data)[18]<-SEND_names[[31]] # MIDY
  
  out_data<-out_data %>% tibble::add_column(DOMAIN='MI',.before="USUBJID") # add Domain column
  
  out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
  
  out_data<-out_data %>% dplyr::mutate(MIORRES = ifelse(NVL == 'No Visible Lesions', 'NO VISIBLE LESIONS' , MIORRES))
  out_data<-out_data %>% dplyr::mutate(MIORRES = ifelse(MISTAT == 'Not Examined', 'NOT EXAMINED' , MIORRES))
  out_data<-out_data %>% dplyr::mutate(MIANTREG = ifelse(MIDIR != '' & MIANTREG !='', paste0(MIANTREG,', ',MIDIR), MIANTREG))
  out_data<-out_data %>% dplyr::mutate(MIANTREG = ifelse(Qualifier != '' & MIANTREG !='', paste0(MIANTREG,', ',Qualifier), MIANTREG))
  out_data<-out_data %>% dplyr::mutate(MIANTREG = ifelse(Qualifier != '' & MIANTREG =='', paste0(Qualifier), MIANTREG))
  out_data<-out_data %>% dplyr::mutate(MILAT = ifelse(MIDIR == 'left', paste0(MIDIR), MILAT))
  out_data<-out_data %>% dplyr::mutate(MIDIR = ifelse(MIDIR == 'left', '', MIDIR))
  out_data<-out_data %>% dplyr::mutate(MILAT = ifelse(MIDIR == 'right', paste0(MIDIR), MILAT))
  out_data<-out_data %>% dplyr::mutate(MIDIR = ifelse(MIDIR == 'right', '', MIDIR))
  
  # remove unused columns
  out_data<-out_data %>% dplyr::select(-c('NVL', 'Tisssue NE Reason', 'Qualifier','Cause of Death','MPF'))
  
  # copy values from MINOMDY to MIDY
  out_data$MINOMDY<-out_data$MIDY
  
  # toupper all relevant cols
  out_data$MIORRES<-toupper(out_data$MIORRES)
  out_data$MIDISTR<-toupper(out_data$MIDISTR)
  out_data$MISPEC<-toupper(out_data$MISPEC)
  out_data$MISTRESC<-toupper(out_data$MISTRESC)
  out_data$MIANTREG<-toupper(out_data$MIANTREG)
  out_data$MILAT<-toupper(out_data$MILAT)
  out_data$MISEV<-toupper(out_data$MISEV)
  out_data$MISTAT<-toupper(out_data$MISTAT)
  out_data$MIDIR<-toupper(out_data$MIDIR)
  out_data$MIDTC<- format(as.POSIXct(out_data$MIDTC,format='%Y/%m/%d %H:%M:%S'))
  
  return(out_data)
}