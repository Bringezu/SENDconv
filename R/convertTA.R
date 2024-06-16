#' convertTA
#' This module converts the Provantis Export for Test Amount to a SEND like structure leading to a TA Domain.
#' @param domainName A character vector that contains the acronym for the domain
#' @param domainData The data frame containing the export of Provantis
#'
#' @return SEND Converted Domain as data frame
#' @export
#'
#' @examples
#'  domainName<-'TA'
#'  domainData<-loadDomain(domainName)
#'  SEND<-convertMA(domainName, domainData)
#'
convertTA<-function(domainName, domainData) {
  stopifnot(is.character(domainName), length(domainName) ==1)
  SEND_names <-unlist(dictionary %>% dplyr::filter(`Domain Prefix`==domainName) %>% dplyr::select(`Variable Name`))
  out_data<-tibble::as_tibble(domainData[[1]])
  names(out_data)[1]<-SEND_names[[1]] # STUDYID
  names(out_data)[2]<-SEND_names[[3]] # Group Number --> ARMCD 
  names(out_data)[3]<-SEND_names[[10]] # Group Name  --> EPOCH 
  names(out_data)[4]<-SEND_names[[4]] # Group Type --> ARM
 
  out_data<-out_data %>% tibble::add_column(DOMAIN='TA',.before="ARMCD") # add Domain column
  
  if (full) {
    names(out_data)<-toupper(names(out_data))
    return(out_data)
  } else {
    out_data<-out_data %>% dplyr::select(-c('Compound',
                                     'Test Material Amount',
                                     'Route of Administration',
                                     'Administration Route Units',
                                     'Dose Colour',
                                     'Vehicle',
                                     'Species',
                                     'Strain'))
    
    return(out_data)
  } 
}