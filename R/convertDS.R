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
#'  SEND<-convertDS(domainData)
#'
convertDS<-function(domainData) {

  # load SEND names from Standard
  SEND_names<-rdSendig('DS')
  out_data<-tibble::as_tibble(domainData[[1]])
  
  names(out_data)[1]<-names(SEND_names[1]) # STUDYID
  names(out_data)[2]<-names(SEND_names[3]) # USUBJID
  names(out_data)[4]<-names(SEND_names[10]) # DSSTDY
  names(out_data)[5]<-names(SEND_names[9]) # DSSTDTC
  names(out_data)[6]<-names(SEND_names[5]) # DSTERM
  names(out_data)[7]<-names(SEND_names[6]) # DSDECOD
  
  out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
  
  out_data<-out_data %>% tibble::add_column(DOMAIN='DS',.before="USUBJID") # add Domain column
  out_data<-out_data %>% tibble::add_column(DSNOMDY="",.after="DSSTDY") # add DSNOMDY column
  out_data<-out_data %>% tibble::add_column(DSUSCHFL="",.after="DSNOMDY") # add DSUSCHFL column
  
  out_data$DSSTDTC<- format(as.POSIXct(out_data$DSSTDTC,format='%Y/%m/%d %H:%M:%S'))
  
  # copy values from DSSTDY to DSNOMDY
  out_data$DSNOMDY<-out_data$DSSTDY
  
  out_data$DSUSCHFL<-""
  out_data<-out_data %>% dplyr::mutate(DSUSCHFL = replace(DSUSCHFL, !grepl('RECOVERY|Recovery|Scheduled|SCHEDULED', DSTERM), 'Y'))
  out_data<-out_data %>% dplyr::mutate(DSUSCHFL = replace(DSUSCHFL,DSSTDTC=="", ''))
  
    # align names according to standard 
  out_data<-out_data%>%
    dplyr::select(dplyr::any_of(names(SEND_names)))
  return(out_data)
  
}