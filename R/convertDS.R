#' convertDS
#' This module converts the Provantis Export for Dispostion Data to a SEND like structure leading to a DS Domain.
#' @param domainName A character vector that contains the acronym for the domain
#' @param domainData The data frame containing the export of Provantis
#'
#' @return SEND Converted Domain as data frame
#' @export
#'
#' @examples
#'  domainName<-'DS'
#'  domainData<-loadDomain(domainName)
#'  SEND<-convertMA(domainName, domainData)
#'
convertDS<-function(domainName, domainData) {
  stopifnot(is.character(domainName), length(domainName) ==1)
  SEND_names <-unlist(dictionary %>% dplyr::filter(`Domain Prefix`==domainName) %>% dplyr::select(`Variable Name`))
  out_data<-tibble::as_tibble(domainData[[1]])
  names(out_data)[1]<-SEND_names[[1]] # STUDYID
  names(out_data)[2]<-SEND_names[[3]] # USUBJID
  names(out_data)[4]<-SEND_names[[10]] # DSSTDY
  names(out_data)[5]<-SEND_names[[9]] # DSSTDTC
  names(out_data)[6]<-SEND_names[[5]] # DSTERM
  names(out_data)[7]<-SEND_names[[6]] # DSDECOD
  
  out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
  
  out_data<-out_data %>% tibble::add_column(DOMAIN='DS',.before="USUBJID") # add Domain column
  out_data<-out_data %>% tibble::add_column(DSNOMDY="",.after="DSSTDY") # add DSNOMDY column
  out_data<-out_data %>% tibble::add_column(DSUSCHFL="",.after="DSNOMDY") # add DSUSCHFL column
  
  
  # copy values from DSSTDY to DSNOMDY
  out_data$DSNOMDY<-out_data$DSSTDY
  
  out_data$DSUSCHFL<-""
  out_data<-out_data %>% dplyr::mutate(DSUSCHFL = replace(DSUSCHFL, !grepl('RECOVERY|Recovery|Scheduled|SCHEDULED', DSTERM), 'Y'))
  out_data<-out_data %>% dplyr::mutate(DSUSCHFL = replace(DSUSCHFL,DSSTDTC=="", ''))
  
  return(out_data)
  
}