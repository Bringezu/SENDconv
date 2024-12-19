#' convert_BASF_CL
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
convert_BASF_CL<-function(domainName, domainData) {
  stopifnot(is.character(domainName), length(domainName) ==1)
  SEND_names <-unlist(dictionary %>% dplyr::filter(`Domain Prefix`==domainName) %>% dplyr::select(`Variable Name`))
  out_data<-tibble::as_tibble(domainData[[1]])
  
  names(out_data)[1]<-SEND_names[[1]] # STUDYID
  names(out_data)[6]<-SEND_names[[3]] # USUBJID
  names(out_data)[15]<-SEND_names[[10]] # CLTEST
  names(out_data)[17]<-SEND_names[[14]] # CLORRES
  # names(out_data)[6]<-SEND_names[[19]] # CLLOC
  #  names(out_data)[7]<-'CLDISTR' # CLDISTR
  #names(out_data)[15]<-SEND_names[[11]] # CLCAT
  # names(out_data)[9]<-SEND_names[[11]] # CLCAT
  # names(out_data)[7]<-SEND_names[[21]] # CLSEV
  names(out_data)[13]<-SEND_names[[26]] # CLDTC
  names(out_data)[11]<-SEND_names[[28]] # CLDY
  # names(out_data)[12]<-SEND_names[[32]] # CLTPT
  
  out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
  
  out_data<-out_data %>% tibble::add_column(DOMAIN='CL',.before="USUBJID") # add Domain column
  out_data<-out_data %>% tibble::add_column(CLTESTCD="",.before="CLTEST") # add CLTESTCD column
  out_data<-out_data %>% tibble::add_column(CLNOMDY="",.after="CLDY") # add CLNOMDY column
  # 
  # cindex<-which(out_data$CLCAT=='Color')
  # for (i in seq_along(cindex)) {
  #   out_data$CLORRES[cindex[i]]<-paste(c(out_data$CLORRES[cindex[i]], out_data$Modifier[cindex[i]]), collapse = ", ")
  # }
  
  # out_data$CLSEV<-gsub('\\.$', '', out_data$CLSEV)
  
  # `%ni%` <- Negate(`%in%`)
  # 
  # cindex<-which(out_data$CLSEV %ni% c('slight','moderate','severe'))
  # for (i in seq_along(cindex)) {
  #   out_data$CLORRES[cindex[i]]<-paste0(out_data$CLORRES[cindex[i]],',',out_data$CLSEV[cindex[i]])
  #   out_data$CLSEV[cindex[i]]<-''
  # }
  
  #  out_data$CLORRES<-gsub(',$', '', out_data$CLORRES)
  
  # cindex<-which(out_data$CLCAT=='Severity')
  # for (i in seq_along(cindex)) {
  #   out_data$CLSEV[cindex[i]]<-out_data$Modifier[cindex[i]]
  # }
  
  # # out_data$CLLOC<-''
  # cindex<-which(out_data$CLCAT=='Laterality')
  # for (i in seq_along(cindex)) {out_data$CLLOC[cindex[i]]<-out_data$Modifier[cindex[i]]}
  # out_data$CLLOC[out_data$CLLOC == 'lighter'] <- ''
  # 
  # out_data$CLSEV<-gsub('slight\\.', 'SLIGHT', out_data$CLSEV)
  # out_data$CLSEV<-gsub('medium\\.', 'MEDIUM', out_data$CLSEV)
  # out_data$CLSEV<-gsub('moderate\\.', 'MODERATE', out_data$CLSEV)
  # 
  
  # out_data$CLORRES[which(out_data$CLCAT=='Color')] <- paste(c( out_data$CLORRES[which(out_data$CLCAT=='Color')],  out_data$Modifier[which(out_data$CLCAT=='Color')]), collapse=", ")
  #out_data<-out_data %>% dplyr::mutate(CLCAT = toupper(CLCAT))
  
  # Set result as NORMAL when nothing is reported
  #out_data<-out_data %>%dplyr:: mutate(CLORRES = replace(CLORRES, CLORRES == '', 'NORMAL'))
  
  
  out_data$CLORRES <- toupper(out_data$CLORRES)
  out_data$CLTEST <- toupper(out_data$CLTEST)
  # out_data$CLTPT <- toupper(out_data$CLTPT)
  # out_data$CLSEV<-toupper(out_data$CLSEV)
  
  # copy values from CLORRES to CLORRESC
  out_data$CLORRESC<-out_data$CLORRES
  
  # copy values from LBNOMDY to BWDY
  out_data$CLNOMDY<-out_data$CLDY
  
  # Remove time and format as Date
  # out_data$CLDTC<-as.Date(gsub("/","-",out_data$CLDTC))
  # out_data$CLDTC<-strptime(out_data$CLDTC,format='%Y/%m/%d %H:%M:%S')
#   out_data$CLDTC<- format(as.POSIXct(out_data$CLDTC,format='%Y/%m/%d %H:%M:%S'))
  
  
  # out_data$CLCAT<-''
  
  ## remove columns
  # return conversion result
  out_data<-out_data %>% dplyr::select(any_of(unname(SEND_names)))
  
  return(out_data)
  
}