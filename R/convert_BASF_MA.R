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
  SEND_names <-unlist(dictionary %>% dplyr::filter(`Domain Prefix`==domainName) %>% dplyr::select(`Variable Name`))
  out_data<-tibble::as_tibble(domainData[[1]])
  names(out_data)[1]<-SEND_names[[1]] # STUDYID
  names(out_data)[6]<-SEND_names[[3]] # USUBJID
  names(out_data)[15]<-SEND_names[[17]] # MASPEC
    names(out_data)[21]<-SEND_names[[12]] # MAORRES
  # names(out_data)[23]<-SEND_names[[]] # MAANTREG
  names(out_data)[27]<-SEND_names[[22]] # Sub-Locator --> MADIR 
  names(out_data)[22]<-SEND_names[[21]] # MALAT
  # names(out_data)[13]<-SEND_names[[18]] # Distribution --> MAANTREG
  names(out_data)[24]<-SEND_names[[25]] # MASEV
  names(out_data)[15]<-SEND_names[[27]] # MADTC
  names(out_data)[8]<-SEND_names[[28]] # MADY
  
  out_data<-out_data %>% tibble::add_column(DOMAIN='MA',.before="USUBJID") # add Domain column
  
  out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
  out_data<-out_data %>% dplyr::mutate(MAORRES = ifelse(`No lesions visible` == 'Yes', 'UNREMARKABLE' , MAORRES))
  
  
  # out_data<-out_data %>% dplyr::mutate(MAORRES = ifelse(MASTAT == 'Not Examined', 'NOT EXAMINED' , MAORRES))
  # out_data<-out_data %>% dplyr::mutate(MAANTREG = ifelse(Distribution != '' & MAANTREG !='', paste0(MAANTREG,', ',Distribution), MAANTREG))
  # out_data<-out_data %>% dplyr::mutate(MAANTREG = ifelse(Distribution != '' & MAANTREG =='', paste0(Distribution), MAANTREG))
  # out_data<-out_data %>% dplyr::mutate(MAANTREG = ifelse(Qualifier != '', paste0(MAANTREG,', ',Qualifier), MAANTREG))
  # 
  # out_data<-out_data %>% dplyr::mutate(MALAT = ifelse(MADIR=='left', MADIR, MALAT))
  # out_data<-out_data %>% dplyr::mutate(MADIR = replace(MADIR, MADIR == "left", ""))
  
  
  
  # 
  # out_data<-out_data %>% dplyr::mutate(MALAT = ifelse(MADIR=='right', MADIR, MALAT))
  # out_data<-out_data %>% dplyr::mutate(MADIR = replace(MADIR, MADIR == "right", ""))
  # 
  # remove unused columns
  out_data<-out_data %>% dplyr::select(any_of(unname(SEND_names)))
  
  
  # toupper all relevant cols
  out_data<-out_data %>% dplyr::mutate_all(.funs=toupper)
  # Set Date Fomat
  #out_data$MADTC<- format(as.POSIXct(out_data$MADTC,format='%Y/%m/%d %H:%M:%S'))
  
  return(out_data)
}