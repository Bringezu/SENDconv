#' convert_BASF_TS
#'
#' @param domainData The dataframe containing the export of Provantis
#'
#' @return SEND Converted Domain as dataframe
#' @export
#'
#' @examples
#'  domainName<-'TS'
#'  domainData<-loadDomain(domainName)
#'  SEND<-convert_BASF_TS(domainData)
#'
convert_BASF_TS<-function(domainData) {
  # stopifnot(is.character(domainName), length(domainName) ==1)
  
  # load SEND names from Standard
  SEND_names<-rdSendig('TS')
  # SEND_names <-unlist(dictionary %>% dplyr::filter(`Domain Prefix`==domainName) %>% dplyr::select(`Variable Name`))
  
  out_data<-domainData %>% 
    dplyr::mutate(across(everything(), as.character)) %>%
    tidyr::pivot_longer(!`STUDYID`,names_to="TSPARMCD", values_to = "TSVAL",values_ptypes = list(TSVAL=character()))
   
  ts_terms<-data.table::fread('c:/temp/Data Lake to be processed/BASF/BASF_all/TRIAL SUMMARY/ts_terms.txt')
  out_data<-out_data %>% left_join(ts_terms, by="TSPARMCD") %>% filter(!is.na(TSVAL))
  
  out_data <- out_data %>% dplyr::mutate(TSPARMCD=ifelse(TSPARMCD=='STRAIN', 'STRAIN_orig', TSPARMCD))
  out_data <- out_data %>% dplyr::mutate(TSPARMCD=ifelse(TSPARMCD=='STRAIN_standardised', 'STRAIN', TSPARMCD))
  out_data <- out_data %>% dplyr::mutate(TSPARM=ifelse(TSPARMCD=='STRAIN', 'Strain/Substrain', TSPARM))
  out_data <- out_data %>% dplyr::mutate(TSPARMCD=ifelse(TSPARMCD=='COMPANY', 'VICT3RP', TSPARMCD))
  out_data <- out_data %>% dplyr::filter(TSPARMCD!='START_YEAR')
  out_data <- out_data %>% dplyr::mutate(TSPARM=ifelse(TSPARMCD=='RFSTDTC', 'Study Start', TSPARM))
  out_data <- out_data %>% dplyr::mutate(TSPARM=ifelse(TSPARMCD=='RFENDTC', 'Study End', TSPARM))
  out_data <- out_data %>% dplyr::mutate(TSPARMCD=ifelse(TSPARMCD=='ROUTE', 'ROUTE_orig', TSPARMCD))
  out_data <- out_data %>% dplyr::mutate(TSPARMCD=ifelse(TSPARMCD=='ROUTE_standardised', 'ROUTE', TSPARMCD))
  out_data <- out_data %>% dplyr::mutate(TSPARMCD=ifelse(TSPARMCD=='AGEU', 'AGEU_orig', TSPARMCD))
  out_data <- out_data %>% dplyr::mutate(TSPARMCD=ifelse(TSPARMCD=='AGEU_neu', 'AGEU', TSPARMCD))
  out_data <- out_data %>% dplyr::mutate(TSPARMCD=ifelse(TSPARMCD=='HUMIDTU', 'HUMIDTU_orig', TSPARMCD))
  out_data <- out_data %>% dplyr::mutate(TSPARMCD=ifelse(TSPARMCD=='HUMIDTU_neu', 'HUMIDTU', TSPARMCD))
  out_data <- out_data %>% dplyr::mutate(TSPARMCD=ifelse(TSPARMCD=='TSFNAM', 'TSTFNAM', TSPARMCD))
  
  out_data <- out_data %>% dplyr::mutate(TSPARM=ifelse(TSPARMCD=='VICT3RP', 'VICT3R Partner', TSPARM))
  out_data <- out_data %>% dplyr::mutate(TSPARM=ifelse(TSPARMCD=='ROUTE', 'ROUTE OF ADMINISTRATION', TSPARM))
  out_data <- out_data %>% dplyr::mutate(TSPARM=ifelse(TSPARMCD=='AGEU', 'AGE UNIT', TSPARM))
  out_data <- out_data %>% dplyr::mutate(TSPARM=ifelse(TSPARMCD=='HUMIDTU', 'HUMIDITY UNIT', TSPARM))
  out_data <- out_data %>% dplyr::mutate(TSPARM=ifelse(TSPARMCD=='TSTFNAM', 'TEST FACILITY NAME', TSPARM))
  out_data <- out_data %>% dplyr::mutate(TSPARM=ifelse(TSPARMCD=='MANESTH', 'METHOD OF ANESTHESIA FOR BLOOD WITHDRAW', TSPARM))
  
  out_data$DOMAIN<-'TS'
  out_data$TSSEQ<-NULL
  out_data$TSTGRPID<-NULL
  out_data$TSVALNF<-NULL
  
  
  # align names according to standard 
  out_data<-out_data%>%
    dplyr::select(dplyr::any_of(names(SEND_names)))
  
  
  return(out_data)
}
