#' convertTA
#' This module converts the Provantis Export for Test Amount to a SEND like structure leading to a TA Domain.
#' @param domainName A character vector that contains the acronym for the domain
#' @param domainData The data frame containing the export of Provantis
#'
#' @return SEND Converted Domain as data frame
#' @export
#'
#' @examples
#'  SEND<-convertTA( domainData)
#'
convertTA<-function(domainData) {
  
  # load SEND names from Standard
  SEND_names<-rdSendig('TA')
  out_data<-tibble::as_tibble(domainData[[1]])
  
  names(out_data)[1]<-names(SEND_names[1]) # STUDYID
  names(out_data)[2]<-names(SEND_names[3]) # Group Number --> ARMCD 
  names(out_data)[3]<-names(SEND_names[10]) # Group Name  --> EPOCH 
  names(out_data)[4]<-names(SEND_names[4]) # Group Type --> ARM
 
  out_data<-out_data %>% tibble::add_column(DOMAIN='TA',.before="ARMCD") # add Domain column
  
  
  # align names according to standard 
  out_data<-out_data%>%
    dplyr::select(dplyr::any_of(names(SEND_names)))
  
  return(out_data)
}