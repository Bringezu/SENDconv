#' convert_BASF_FX
#' @param domainName A character vector that contains the acronym for the domain
#' @param domainData The dataframe containing the export of Provantis
#'
#' @return SEND Converted Domain as dataframe, use BASF Provantis Export files
#' @export
#'
#' @examples
#'  domainName<-'Fx'
#'  domainData<-loadDomain(domainName)
#'  SEND<-convert_BASF_FX(domainName, domainData)
#'  

convert_BASF_FX<-function(domainName, domainData) {
  # load SEND names from Standard
  SEND_names<-rdSendig('FX')
  out_data<-tibble::as_tibble(domainData)
  
  names(out_data)[1]<-names(SEND_names[1]) # STUDYID
  names(out_data)[5]<-names(SEND_names[3]) # USUBJID
  names(out_data)[10]<-names(SEND_names[4]) # FETUSID
  names(out_data)[13]<-names(SEND_names[24]) # FXDTC
  names(out_data)[14]<-names(SEND_names[9]) # FXTEST
  names(out_data)[16]<-names(SEND_names[18]) # FXLOC
  names(out_data)[18]<-names(SEND_names[10]) # FXORRES
  names(out_data)[19]<-names(SEND_names[12]) # FXRESCAT
  
  
  out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
  
  
  out_data<-out_data %>% tibble::add_column(DOMAIN='FX',.before="USUBJID") # add Domain column
  out_data<-out_data %>% tibble::add_column(FXTESTCD="",.before="FXTEST") # add FXTESTCD column
  out_data<-out_data %>% tibble::add_column(FXSTRESC="",.after="FXORRES") # add FXSTRESC column
  

  # AG Distance: no numerical data. 
  # Numerical values are populated in FM Domain
  out_data<-out_data %>% dplyr::filter(FXTEST!='AG Distance') 
  
  out_data<-out_data %>% dplyr::mutate(FXORRES=ifelse(grepl('Yes', `Is NAD`, ignore.case = T), 'NAD', FXORRES))
  out_data<-out_data %>% dplyr::mutate(FXSTRESC=ifelse(grepl('Yes', `Is NAD`, ignore.case = T), 'UNREMARKABLE', FXSTRESC))
  out_data<-out_data %>% dplyr::filter(FXORRES!='')
  
 
  # Set BWTESTCD according to BWTEST information
  # out_data<-out_data %>% dplyr::mutate(FXTESTCD = ifelse(FXTEST == 'Fetal Weigth', 'FWT' , FXTESTCD))
  # out_data<-out_data %>% dplyr::mutate(FXTESTCD = ifelse(FXTEST == 'Anogenital Distance', 'AGD' , FXTESTCD))
  # out_data<-out_data %>% dplyr::mutate(FXTESTCD = ifelse(FXTEST == 'Placental Weight', 'PWT' , FXTESTCD))
  # 
  
  # set correct date fomat
  Sys.setlocale("LC_ALL", "English")
  out_data$FXDTC<- format(as.POSIXct(out_data$FXDTC,format="%d-%b-%Y %H:%M"))
  Sys.setlocale("LC_ALL", "de_DE.UTF-8")
  
  # remove unused columns
  out_data<-out_data %>% dplyr::select(any_of(names(SEND_names)))
  
  
  ##########################
  return(out_data)
  
}

