#' convertTS
#'
#' @param domainData The dataframe containing the export of Provantis
#'
#' @return SEND Converted Domain as dataframe
#' @export
#'
#' @examples
#'  domainName<-'TS'
#'  domainData<-loadDomain(domainName)
#'  SEND<-convertTS(domainData)
#'
convertTS<-function(domainData) {
  # stopifnot(is.character(domainName), length(domainName) ==1)
  
  # load SEND names from Standard
  SEND_names<-rdSendig('TS')
  # SEND_names <-unlist(dictionary %>% dplyr::filter(`Domain Prefix`==domainName) %>% dplyr::select(`Variable Name`))
  
  out_data<-tibble::as_tibble(domainData[[1]])
  # need homogeneous columns types for pivot_longer
  out_data<-out_data%>%dplyr::mutate(SPREFID=`Study Number`,  'Sponsoring Organisation'='Merck') %>%
    mutate_all(as.character) %>%
    tidyr::pivot_longer(!`Study Number`,names_to="TSPARM", values_to = "TSVAL")
  
  out_data$TSPARMCD<-NA
  out_data <- out_data %>% dplyr::mutate(TSPARMCD=ifelse(TSPARM=='Study Title', 'STITLE', TSPARMCD))
  out_data <- out_data %>% dplyr::mutate(TSPARMCD=ifelse(TSPARM=='Species', 'SPECIES', TSPARMCD))
  out_data <- out_data %>% dplyr::mutate(TSPARMCD=ifelse(TSPARM=='Strain', 'STRAIN', TSPARMCD))
  out_data <- out_data %>% dplyr::mutate(TSPARMCD=ifelse(TSPARM=='GLP Study', 'GLPFL', TSPARMCD))
  out_data <- out_data %>% dplyr::mutate(TSPARMCD=ifelse(TSPARM=='Start Date', 'RFSTDTC', TSPARMCD))
  out_data <- out_data %>% dplyr::mutate(TSPARMCD=ifelse(TSPARM=='End Date', 'RFENDTC', TSPARMCD))
  out_data <- out_data %>% dplyr::mutate(TSPARMCD=ifelse(TSPARM=='Live Phase', 'DOSDUR', TSPARMCD))
  out_data <- out_data %>% dplyr::mutate(TSPARMCD=ifelse(TSPARM=='Live Phase Units', 'DOSDURU', TSPARMCD))
  out_data <- out_data %>% dplyr::mutate(TSPARMCD=ifelse(TSPARM=='Supplier', 'DOSDURU', TSPARMCD))
  out_data <- out_data %>% dplyr::mutate(TSPARMCD=ifelse(TSPARM=='Diet Type', 'DIET', TSPARMCD))
  out_data <- out_data %>% dplyr::mutate(TSPARMCD=ifelse(TSPARM=='Sponsoring Organisation', 'SSPONSOR', TSPARMCD))
  out_data <- out_data %>% dplyr::mutate(TSPARMCD=ifelse(TSPARM=='Supplier', 'SPLRNAM', TSPARMCD))
  out_data <- out_data %>% dplyr::mutate(TSPARMCD=ifelse(TSPARM=='Study Type', 'STYP', TSPARMCD))
  out_data <- out_data %>% dplyr::mutate(TSPARMCD=ifelse(TSPARM=='Study Status', 'SSTATUS', TSPARMCD))
  out_data <- out_data %>% dplyr::mutate(TSPARMCD=ifelse(TSPARM=='Study Director', 'SDIR', TSPARMCD))
  out_data <- out_data %>% dplyr::mutate(TSPARMCD=ifelse(TSPARM=='Account Code', 'SCODE', TSPARMCD))
  
  out_data <- out_data %>% dplyr::mutate(TSVAL=ifelse(TSPARMCD=='RFSTDTC',format(as.POSIXct(TSVAL,format='%Y/%m/%d %H:%M:%S')) , TSVAL))
  out_data <- out_data %>% dplyr::mutate(TSVAL=ifelse(TSPARMCD=='RFENDTC',format(as.POSIXct(TSVAL,format='%Y/%m/%d %H:%M:%S')) , TSVAL))
  
  out_data$DOMAIN<-'TS'
  out_data$TSSEQ<-NULL
  out_data$TSTGRPID<-NULL
  out_data$TSVALNF<-NULL
  
  
  # align names according to standard 
  out_data<-out_data%>%
    dplyr::select(dplyr::any_of(names(SEND_names)))
  
  
  return(out_data)
}
