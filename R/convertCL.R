#' convertCL
#'
#' @param domainName A character vector that contains the acronym for the domain
#' @param domainData The data frame containing the export of Provantis
#'
#' @return SEND Converted Domain as data frame
#' @export
#'
#' @examples
#'  domainName<-'CL'
#'  domainData<-loadDomain(domainName)
#'  SEND<-convertCL(domainName, domainData)
#'
convertCL<-function(domainData) {
  
  
  SEND_names<-rdSendig('CL')
  
  ## Step 1: Associate data with SEND Names for CL 
  out_data<-tibble::as_tibble(domainData[[1]])
  names(out_data)[1]<-names(SEND_names)[1] # STUDYID
  names(out_data)[2]<-names(SEND_names)[3] # USUBJID
  names(out_data)[3]<-names(SEND_names)[10] # CLTEST
  names(out_data)[5]<-names(SEND_names)[14] # CLORRES
  names(out_data)[6]<-names(SEND_names)[19] # CLLOC
  names(out_data)[7]<-names(SEND_names[21]) # CLSEV
  names(out_data)[8]<-names(SEND_names[11]) # CLCAT
  names(out_data)[10]<-names(SEND_names[26]) # CLDTC
  names(out_data)[11]<-names(SEND_names[28]) # CLDY
  names(out_data)[12]<-names(SEND_names[32]) # CLTP  

  # remove empty cells
  out_data[] <- lapply(out_data, SendConv::make.true.NA)
  
  ## Step 2a: merge NAD (column 4) with Symptom (column 5) into CLORRES 
  out_data<- out_data %>% dplyr::mutate(CLORRES = ifelse(NAD))
  
  
  ## Step 2b: merge Modifier (column 4) with Symptom (column 5) into CLORRES 
  
  out_data <- out_data %>% dplyr::mutate(CLLOC = paste0(CLLOC, Modifier))

  out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
  
  out_data<-out_data %>% tibble::add_column(DOMAIN='CL',.before="USUBJID") # add Domain column
  out_data<-out_data %>% tibble::add_column(CLTESTCD="",.before="CLTEST") # add CLTESTCD column
  out_data<-out_data %>% tibble::add_column(CLNOMDY="",.after="CLDY") # add CLNOMDY column
  
  out_data <- out_data %>% dplyr::mutate(CLORRES = ifelse(CLCAT=='Color', paste(c(CLORRES, Modifier, collapse = ", "), '')))
  
  out_data$CLSEV<-gsub('\\.$', '', out_data$CLSEV)
  
  `%ni%` <- Negate(`%in%`)
  
  out_data <- out_data %>% dplyr::mutate(CLORRES = ifelse(CLSEV %ni% c('slight','moderate','severe'),paste0(CLORRES,', ',CLSEV), CLORRES))
  out_data <- out_data %>% dplyr::mutate(CLSEV = ifelse(CLSEV %ni% c('slight','moderate','severe'),'', CLSEV))
  
  
  out_data$CLORRES<-gsub(',$', '', out_data$CLORRES)
  out_data <- out_data %>% dplyr::mutate(CLSEV = ifelse(CLCAT=='Severity',Modifier, ''))
  out_data <- out_data %>% dplyr::mutate(CLLOC = ifelse(CLCAT=='Laterality',Modifier, ''))
  
  out_data$CLSEV<-gsub('slight\\.', 'SLIGHT', out_data$CLSEV)
  out_data$CLSEV<-gsub('medium\\.', 'MEDIUM', out_data$CLSEV)
  out_data$CLSEV<-gsub('moderate\\.', 'MODERATE', out_data$CLSEV)
  
  
  # out_data$CLORRES[which(out_data$CLCAT=='Color')] <- paste(c( out_data$CLORRES[which(out_data$CLCAT=='Color')],  out_data$Modifier[which(out_data$CLCAT=='Color')]), collapse=", ")
  out_data<-out_data %>% dplyr::mutate(CLLOC = toupper(CLLOC))
  
  # Set result as NORMAL when nothing is reported
  out_data<-out_data %>%dplyr:: mutate(CLORRES = replace(CLORRES, CLORRES == '', 'NORMAL'))
  
  names(out_data)<-toupper(names(out_data))
  
  # copy values from CLORRES to CLORRESC
  out_data$CLORRESC<-as.character(out_data$CLORRES)
  
  # copy values from LBNOMDY to BWDY
  out_data$CLNOMDY<-out_data$CLDY
  
  # Remove time and format as Date
  # out_data$CLDTC<-as.Date(gsub("/","-",out_data$CLDTC))
  # out_data$CLDTC<-strptime(out_data$CLDTC,format='%Y/%m/%d %H:%M:%S')
  out_data$CLDTC<- format(as.POSIXct(out_data$CLDTC,format='%Y/%m/%d %H:%M:%S'))
  
  # align names according to standard 
  out_data<-out_data%>%
    dplyr::select(dplyr::any_of(names(SEND_names)))
  return(out_data)
  
}