#' convertBW
#'
#' @param domainData The dataframe containing the export of Provantis
#'
#' @return SEND Converted Domain as dataframe
#' @export
#'
#' @examples
#'  domainName<-'BW'
#'  domainData<-loadDomain(domainName)
#'  SEND<-convertBW(domainData)
#'
convertBW<-function(domainData) {
  # stopifnot(is.character(domainName), length(domainName) ==1)

  # load SEND names from Standard
  SEND_names<-rdSendig('BW')
  # SEND_names <-unlist(dictionary %>% dplyr::filter(`Domain Prefix`==domainName) %>% dplyr::select(`Variable Name`))

  out_data<-tibble::as_tibble(domainData[[1]])
  names(out_data)[1]<-names(SEND_names)[1] # STUDYID
  names(out_data)[2]<-names(SEND_names)[3] # USUBJID
  names(out_data)[4]<-names(SEND_names)[6] # BWTEST
  names(out_data)[5]<-names(SEND_names)[7] # BWORRES
  names(out_data)[6]<-names(SEND_names)[8] # BWORRESU
  names(out_data)[7]<-names(SEND_names)[9] # BWSTRESC
  names(out_data)[8]<-names(SEND_names)[20] # BWDTC
  names(out_data)[9]<-names(SEND_names)[21] # BWDY

  out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
  out_data<-out_data %>% tibble::add_column(DOMAIN='BW',.before="USUBJID") # add Domain column
  out_data<-out_data %>% tibble::add_column(BWTESTCD="",.before="BWTEST") # add BWTESTCD column
  out_data<-out_data %>% tibble::add_column(BWNOMDY="",.after="BWDY") # add BWNOMDY column
  out_data<-out_data %>% tibble::add_column(BWSTRESN="",.after="BWSTRESC") # add BWSTRESN column
  out_data<-out_data %>% tibble::add_column(BWSTRESU="",.after="BWSTRESN") # add BWSTRESU column
  # Correct BWTEST
  out_data<-out_data %>% dplyr::mutate(BWTEST = replace(BWTEST, BWTEST == 'Bodyweights', 'Body Weight'))
  out_data<-out_data %>% dplyr::mutate(BWTEST = replace(BWTEST, BWTEST == 'Terminal Bodyweight', 'Terminal Body Weight'))

  # Set BWTESTCD according to BWTEST information
  out_data<-out_data %>% dplyr::mutate(BWTESTCD = ifelse(BWTEST == 'Body Weight', 'BW' , 'TERMBW'))

  # copy values from BWORRES to BWORRESN
  out_data$BWSTRESN<-as.numeric(out_data$BWORRES)
  out_data$BWSTRESC<-as.character(out_data$BWORRES)

  # align names according to standard 
    out_data<-out_data%>%
    dplyr::select(dplyr::any_of(names(SEND_names)))
 
  # copy values from BWORRESU to BWSTRESU
  out_data$BWSTRESU<-out_data$BWORRESU

  # copy values from BWNOMDY to BWDY
  out_data$BWNOMDY<-out_data$BWDY

  # set correct date fomat
  out_data$BWDTC<- format(as.POSIXct(out_data$BWDTC,format='%Y/%m/%d %H:%M:%S'))

  names(out_data)<- toupper(names(out_data)) ## make colnames toupper case
  
  return(out_data)
}
