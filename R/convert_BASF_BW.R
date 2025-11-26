#' convert_BASF_BW
#'
#' @param domainName A character vector that contains the acronym for the domain
#' @param domainData The dataframe containing the export of Provantis
#'
#' @return SEND Converted Domain as dataframe, use BASF Provantis Export files
#' @export
#'
#' @examples
#'  domainName<-'BW'
#'  domainData<-loadDomain(domainName)
#'  SEND<-convertBW(domainName, domainData)
#'
convert_BASF_BW<-function(domainName, domainData) {
  stopifnot(is.character(domainName), length(domainName) ==1)
  SEND_names<-rdSendig('BW')
  out_data<-domainData
  names(out_data)[1]<-names(SEND_names)[1] # STUDYID
  names(out_data)[5]<-names(SEND_names)[3] # USUBJID
  names(out_data)[15]<-names(SEND_names)[6] # BWTEST
  names(out_data)[18]<-names(SEND_names)[7] # BWORRES
  names(out_data)[19]<-names(SEND_names)[8] # BWORRESU
  names(out_data)[13]<-names(SEND_names)[22] # BWDTC
  names(out_data)[12]<-names(SEND_names)[23] # BWDY
  names(out_data)[10]<-names(SEND_names)[20] # RPHASE
  names(out_data)[11]<-names(SEND_names)[26] # BWRPDY

  # browser()
  out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
  out_data<-out_data %>% tibble::add_column(DOMAIN='BW',.before="USUBJID") # add Domain column
  out_data<-out_data %>% tibble::add_column(BWTESTCD="",.before="BWTEST") # add BWTESTCD column
  # out_data<-out_data %>% tibble::add_column(BWNOMDY="",.after="BWDY") # add BWNOMDY column
  out_data<-out_data %>% tibble::add_column(BWSTRESN=0.0,.after="BWORRESU") # add BWSTRESN column
  out_data<-out_data %>% tibble::add_column(BWSTRESU="",.after="BWSTRESN") # add BWSTRESU column
  out_data<-out_data %>% tibble::add_column(BWSTRESC="",.after="BWSTRESU") # add BWSTRESC column
  
  # Correct BWTEST
  out_data<-out_data %>% dplyr::mutate(BWTEST = replace(BWTEST, BWTEST == 'Bodyweights', 'Body Weight'))
  out_data<-out_data %>% dplyr::mutate(BWTEST = replace(BWTEST, BWTEST == 'Terminal Bodyweight', 'Terminal Body Weight'))
  
  # Set BWTESTCD according to BWTEST information
  out_data<-out_data %>% dplyr::mutate(BWTESTCD = ifelse(BWTEST == 'Body Weight', 'BW' , 'TERMBW'))
  
  # copy values from BWORRES to BWORRESN
  out_data$BWSTRESN<-as.numeric(out_data$BWORRES)
  out_data$BWSTRESC<-as.character(out_data$BWORRES)
  
  # copy values from BWORRESU to BWSTRESU
  out_data$BWSTRESU<-out_data$BWORRESU
  
  # copy values from BWNOMDY to BWDY
  # out_data$BWNOMDY<-out_data$BWDY
  
  # toupper all relevant cols
  out_data<-out_data %>% dplyr::mutate_all(.funs=toupper)
  
  # set correct date fomat
  Sys.setlocale("LC_ALL", "English")
  out_data$BWDTC<- format(as.POSIXct(out_data$BWDTC,format="%d-%b-%Y %H:%M"))
  Sys.setlocale("LC_ALL", "de_DE.UTF-8")
  
  # remove unused columns
  out_data<-out_data %>% dplyr::select(any_of(names(SEND_names)))
  return(out_data)
  
  
}
