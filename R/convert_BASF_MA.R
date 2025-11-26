#' convert_BASF_MA
#'
#' @param domainName A character vector that contains the acronym for the domain
#' @param domainData The data frame containing the export of Provantis
#'
#' @return SEND Converted Domain as dataframe
#' @export
#'
#' @examples
#'  domainName<-'MA'
#'  domainData<-loadDomain(domainName)
#'  SEND<-convertMA(domainName, domainData)
#'
convert_BASF_MA<-function(domainName, domainData) {
  stopifnot(is.character(domainName), length(domainName) ==1)
  SEND_names<-rdSendig('MA')
  out_data<-tibble::as_tibble(domainData)
  
  names(out_data)[1]<-names(SEND_names[1]) # STUDYID
  names(out_data)[5]<-names(SEND_names[3]) # USUBJID
  names(out_data)[8]<-names(SEND_names[30]) # MADY
  names(out_data)[9]<-names(SEND_names[29]) # MADTC
  names(out_data)[15]<-names(SEND_names[26]) # MADY
  names(out_data)[21]<-names(SEND_names[12]) # MAORRES
  names(out_data)[22]<-names(SEND_names[18]) # MAANTREG
  names(out_data)[24]<-names(SEND_names[25]) # MASEV
  names(out_data)[25]<-names(SEND_names[21]) # MALAT
  
  
  out_data<-out_data %>% tibble::add_column(DOMAIN='MA',.before="USUBJID") # add Domain column
  
  out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
  out_data<-out_data %>% dplyr::mutate(MAORRES  = ifelse(`No lesions visible` == 'Yes', 'UNREMARKABLE' , MAORRES))
  
  
  # out_data<-out_data %>% dplyr::mutate(MAORRES = ifelse(MASTAT == 'Not Examined', 'NOT EXAMINED' , MAORRES))
  out_data<-out_data %>% dplyr::mutate(MAANTREG = ifelse(!is.na(Distribution), paste0(MAANTREG,', ',Distribution), MAANTREG))
  out_data<-out_data %>% dplyr::mutate(MAANTREG = ifelse(!is.na(Size), paste0(MAANTREG,', ',Size), MAANTREG))
  out_data<-out_data %>% dplyr::mutate(MAANTREG = ifelse(!is.na(Qualifiers), paste0(MAANTREG,', ',Qualifiers), MAANTREG))
  # out_data<-out_data %>% dplyr::mutate(MAANTREG = ifelse(Qualifier != '', paste0(MAANTREG,', ',Qualifier), MAANTREG))
  # 
  # out_data<-out_data %>% dplyr::mutate(MALAT = ifelse(MADIR=='left', MADIR, MALAT))
  # out_data<-out_data %>% dplyr::mutate(MADIR = replace(MADIR, MADIR == "left", ""))
  
  
  
  # 
  # out_data<-out_data %>% dplyr::mutate(MALAT = ifelse(MADIR=='right', MADIR, MALAT))
  # out_data<-out_data %>% dplyr::mutate(MADIR = replace(MADIR, MADIR == "right", ""))
  # 

  # modify MASTRESC
  out_data<- out_data %>% dplyr::mutate(MASTRESC=as.character(MAORRES))
  
  
  # toupper all relevant cols
  out_data<-out_data %>% dplyr::mutate_all(.funs=toupper)
 
  # remove unused columns
  out_data<-out_data %>% dplyr::select(any_of(names(SEND_names)))
  
  # set correct date fomat
  Sys.setlocale("LC_ALL", "English")
  out_data$MADTC<- format(as.POSIXct(out_data$MADTC,format="%d-%b-%Y %H:%M"))
  Sys.setlocale("LC_ALL", "de_DE.UTF-8")
  
  return(out_data)
}