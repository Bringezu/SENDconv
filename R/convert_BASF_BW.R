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
convert_BSAF_BW<-function(domainName, domainData) {
  stopifnot(is.character(domainName), length(domainName) ==1)
  SEND_names <-unlist(dictionary %>% dplyr::filter(`Domain Prefix`==domainName) %>% dplyr::select(`Variable Name`))
  out_data<-tibble::as_tibble(domainData[[1]])
  names(out_data)[1]<-SEND_names[[1]] # STUDYID
  names(out_data)[6]<-SEND_names[[3]] # USUBJID
  names(out_data)[15]<-SEND_names[[6]] # BWTEST
  names(out_data)[18]<-SEND_names[[7]] # BWORRES
  names(out_data)[19]<-SEND_names[[8]] # BWORRESU
  names(out_data)[13]<-SEND_names[[20]] # BWDTC
  names(out_data)[12]<-SEND_names[[21]] # BWDY
  names(out_data)[11]<-SEND_names[[22]] # BWNOMDY
  
  out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
  out_data<-out_data %>% tibble::add_column(DOMAIN='BW',.before="USUBJID") # add Domain column
  out_data<-out_data %>% tibble::add_column(BWTESTCD="",.before="BWTEST") # add BWTESTCD column
  # out_data<-out_data %>% tibble::add_column(BWNOMDY="",.after="BWDY") # add BWNOMDY column
  out_data<-out_data %>% tibble::add_column(BWSTRESN="",.after="BWORRESU") # add BWSTRESN column
  out_data<-out_data %>% tibble::add_column(BWSTRESU="",.after="BWSTRESN") # add BWSTRESU column
  out_data<-out_data %>% tibble::add_column(BWSTRESC="",.after="BWSTRESU") # add BWSTRESC column
  
  # Correct BWTEST
  out_data<-out_data %>% dplyr::mutate(BWTEST = replace(BWTEST, BWTEST == 'Bodyweights', 'Body Weight'))
  out_data<-out_data %>% dplyr::mutate(BWTEST = replace(BWTEST, BWTEST == 'Terminal Bodyweight', 'Terminal Body Weight'))
  
  # Set BWTESTCD according to BWTEST information
  out_data<-out_data %>% dplyr::mutate(BWTESTCD = ifelse(BWTEST == 'Body Weight', 'BW' , 'TERMBW'))
  
  # copy values from BWORRES to BWORRESN
  out_data$BWSTRESN<-as.double(out_data$BWORRES)
  out_data$BWSTRESC<-as.character(out_data$BWORRES)
  
  # copy values from BWORRESU to BWSTRESU
  out_data$BWSTRESU<-out_data$BWORRESU
  
  # copy values from BWNOMDY to BWDY
  # out_data$BWNOMDY<-out_data$BWDY
  
  # set correct date fomat
  # out_data$BWDTC<- format(as.POSIXct(out_data$BWDTC,format='%d-%m-%Y %H:%M'))
  
  # remove unused columns
  out_data<-out_data %>% dplyr::select(any_of(unname(SEND_names)))
  return(out_data)
  
  
}
