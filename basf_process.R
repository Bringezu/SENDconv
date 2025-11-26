read_and_melt<-function(dir=dir, pattern=pattern, dec=dec, sep=sep){
  files <- list.files(dir,pattern=pattern,ignore.case = T, full.names=TRUE) %>%
    file.info() %>%
    tibble::rownames_to_column()
  if (nrow(files)>0) {df <- lapply(files$rowname, data.table::fread, dec=dec, sep=sep)} else(df<-NULL)

  for seq_along(i in df){
    df
  }
}


dir<-'C:/Temp/Data Lake to be processed/BASF/BASF_all/ANIMAL_DATA/'
sep<-','
dec<-'.'
df<-loadDomain('DM', dir=dir, pattern=pattern, dec=dec, sep=sep)
df<-loadDomain('DM', dir=dir, pattern='ANIMAL, dec=dec, sep=sep)
''
'
df<-loadDomain('DM', dir=dir, pattern='ANIMAL', dec=dec, sep=sep)
loadDomain <- function(domain, dir=dir, pattern=pattern, dec=dec, sep=sep){
if (!methods::hasArg(domain)) {return('please provide a Domain acronyme, e.g. BW for Bodyweight')}
# If no dir was provided, select a  directory
if (!methods::hasArg(dir)) {dir<- dirname(file.choose())} #
# Define a default pattern
if (!methods::hasArg(pattern)) {pattern<- paste0('SEND_',domain,'-.*.csv')}
# Define a default pattern
if (!methods::hasArg(dec)) {dec<- '.'}
stopifnot(is.character(domain), length(domain) ==1)
# pattern<-paste0('^SEND_',domain,"-.*.csv")
# pattern<-paste0('^',domain,"_BASF_.*.CSV")
# files <- list.files(dir, pattern=pattern,ignore.case = T, full.names=TRUE)
# filter files per date (only files not older than 7 days)
md<-1400
files <- list.files(dir,pattern=pattern,ignore.case = T, full.names=TRUE) %>%
file.info() %>%
tibble::rownames_to_column()
if (nrow(files)>0) {df <- lapply(files$rowname, data.table::fread, dec=dec, sep=sep)} else(df<-NULL)
return(rbind(df))
}
df<-loadDomain('DM', dir=dir, pattern='ANIMAL', dec=dec, sep=sep)
do.call("rbind",df)
dd<-do.call("rbind",df)
View(dd)
load_all()
rm(list = c("loadDomain"))
load_all()
library(tidyverse)
sendig<-dict$SENDIG_DART_11_VICT3R_21
s<-sendig %>% filter(DomainPrefix=='DM')
s
names(s)
View(s)
s<-loadSENDIG(domain = 'DM')
names(dd)
names(dd)[1]<-'STUDYID'
dd<-dd %>% mutate(USUBJID=paste0(STUDYID, `Animal #`))
dd<-dd %>% relocate(USUBJID, .after = STUDYID)
dd<-dd %>% mutate(USUBJID=paste0(STUDYID,'-', `Animal #`))
dd<-dd %>% mutate(SEX=ifelse(Sex=='Female', 'F', 'M'))
dd<-dd %>% relocate(SEX, .after = USUBJID)
dd<-dd %>% mutate(BRTHDTC=as.POSIXct(`Birth date`)) %>% relocate(.after='SEX')
dd<-dd %>% mutate(BRTHDTC=as.Date(`Birth date`)) %>% relocate(.after='SEX')
dd<-dd %>% mutate(BRTHDTC=as.POSIXct(`Birth date`, '%d-b-Y %h:%s')) %>% relocate(.after='SEX')
dd<-dd %>% mutate(BRTHDTC=as.POSIXct(`Birth date`, '%d-%b-%Y %h:%s')) %>% relocate(.after='SEX')
rlang::last_trace()
strptime(dd$`Birth date`)
dd<-dd %>% mutate(BRTHDTC=as.POSIXct(`Birth date`, '%d-%b-%Y %H:%s')) %>% relocate(.after='SEX')
dd<-dd %>% mutate(BRTHDTC=as.POSIXct(`Birth date`, '%d-%b-%Y %H:%S')) %>% relocate(.after='SEX')
dd<-dd %>% mutate(BRTHDTC=as.POSIXct(`Birth date`, '%d-%b-%Y %R')) %>% relocate(.after='SEX')
dd<-dd %>% mutate(BRTHDTC=as.POSIXct(`Birth date`, '%d-%b-%Y %H:%M')) %>% relocate(.after='SEX')
dd<-dd %>% mutate(BRTHDTC=strptime(`Birth date`, '%d-%b-%Y %H:%M')) %>% relocate(.after='SEX')
View(dd)
dd<-do.call("rbind",df)
View(dd)
names(dd)[1]<-'STUDYID'
dd<-dd %>% mutate(USUBJID=paste0(STUDYID, '-', `Animal #`))
dd<-dd %>% relocate(USUBJID, .after = STUDYID)
dd<-dd %>% mutate(SEX=ifelse(Sex=='Female', 'F', 'M'))
dd<-dd %>% relocate(SEX, .after = USUBJID)
strptime(dd$`Birth date`, '%d-%b-%Y %H:%M:%S')
date_format <- "%d-%b-%Y %H:%M"
strptime(dd$`Birth date`, format = date_format)
as.POSIXct(dd$`Birth date`, format = date_format)
as.POSIXct(dd$`Birth date`, format = date_format)
date_format
dd<-dd %>% mutate(BRTHDTC=`Birth date`) %>% relocate(.after = SEX)
dd<-dd %>% relocate(.after = SEX)
dd<-dd %>% relocate(.after = 'SEX')
dd<-dd %>% relocate(BRTHDTC, .after = 'SEX')
dd<-dd %>% mutate(BRTHDTC=as.POSIXct(BRTHDTC, date_format))
dd<-dd %>% mutate(BRTHDTC=as.POSIXct(BRTHDTC, format=date_format))
dd<-dd %>% mutate(RFSTDTC=as.POSIXct(`Start date`, format=date_format))
dd<-dd %>% relocate(RFSTDTC, .after = 'BRTHDTC')
dd<-dd %>% mutate(RFXSTDTC=as.POSIXct(`1st dose date`, format=date_format))
dd<-dd %>% relocate(RFSTDTC, .after = 'RFSTDTC')
dd<-dd %>% relocate(RFXSTDTC, .after = 'RFSTDTC')
dd<-dd %>% mutate(RFXENDTC=as.POSIXct(`Last dose date`, format=date_format))
dd<-dd %>% relocate(RFXENDTC, .after = 'RFXSTDTC')
dd<-dd %>% mutate(RFENDTC=as.POSIXct(`Date of death`, format=date_format))
dd<-dd %>% relocate(RFENDTC, .after = 'RFXENDTC')
dd %>% dplyr::select(dplyr::any_of(names(s)))
dd<-dd %>% dplyr::select(dplyr::any_of(names(s)))
dd
library(data.table)
fwrite(dd, 'c:/Temp/Data Lake to be processed/BASF/dm.txt', sep=';', quote = T)
s
dd<-dd %>% mutate(AGE=as.Date(BRTHDTC)-as.Date(RFSTDTC))
View(dd)
dd<-dd %>% mutate(AGE=as.Date(RFSTDTC)-as.Date(BRTHDTC))
dd<-dd %>% mutate(AGEU='DAYS')
fwrite(dd, 'c:/Temp/Data Lake to be processed/BASF/dm.txt', sep=';', quote = T)


################# Batch Start ##############

sep<-','
dec<-'.'
outdir<-'c:/temp/Data Lake to be processed/BASF/BASF_all/output'

dir<-"C:/Temp/Data Lake to be processed/BASF/BASF_all/ANIMAL_DATA/"
df<-loadDomain('DM', dir=dir, sep=sep, dec=dec, pattern = 'ANIMAL')
dd<-convert_BASF_DM('DM', df)
exportDomain(dd, 'txt', outdir)

dir<-"C:/Temp/Data Lake to be processed/BASF/BASF_all/ANIMAL_BODYWEIGHTS/"
df<-loadDomain('BW', dir=dir, pattern='BODY', dec=dec, sep=sep)
dd<-convert_BASF_BW('BW', df)
exportDomain(dd, 'txt', outdir)

dir<-"C:/Temp/Data Lake to be processed/BASF/BASF_all/CLINICAL OBSERVATIONS/"
df<-loadDomain('CL', dir=dir, pattern='CLINICAL', dec=dec, sep=sep)
dd<-convert_BASF_CL('CL', df)
exportDomain(dd, 'txt', outdir)

dir<-"C:/Temp/Data Lake to be processed/BASF/BASF_all/GROSS PATHOLOGY/"
df<-loadDomain('MA', dir=dir, pattern='GROSS', dec=dec, sep=sep)
dd<-convert_BASF_MA('MA', df)
exportDomain(dd, 'txt', outdir)

dir<-"C:/Temp/Data Lake to be processed/BASF/BASF_all/ACTUAL DOSING/"
df<-loadDomain('EX', dir=dir, pattern='ACTUAL', dec=dec, sep=sep)
dd<-convert_BASF_EX('EX', df)
exportDomain(dd, 'txt', outdir)

dir<-"C:/Temp/Data Lake to be processed/BASF/BASF_all/ANIMAL MEASUREMENTS/"
df<-loadDomain('LB', dir=dir, pattern='ANIM', dec=dec, sep=sep)
dd<-convert_BASF_OM('OM', df)
exportDomain(dd, 'txt', outdir)



dir<-"C:/Temp/Data Lake to be processed/BASF/BASF_all/HISTOPATHOLOGY/"
df<-loadDomain('MI', dir=dir, pattern='HISTO', dec=dec, sep=sep)
dd<-convert_BASF_MI('MI', df)
exportDomain(dd, 'txt', outdir)

dir<-"C:/Temp/Data Lake to be processed/BASF/BASF_all/FETAL_PATHOLOGY/"
df<-loadDomain('BW', dir=dir, pattern='FETAL', dec=dec, sep=sep)
dd<-convert_BASF_FX('FX', df)
exportDomain(dd, 'txt', outdir)

dir<-"C:/Temp/Data Lake to be processed/BASF/BASF_all/FETAL_MEASUREMENTS/"
df<-loadDomain('BW', dir=dir, pattern='FETAL', dec=dec, sep=sep)
dd<-convert_BASF_FM('FM', df)
exportDomain(dd, 'txt', outdir)

dir<-'C:/Temp/Data Lake to be processed/BASF/BASF_all/C-SECTION'
df<-loadDomain('PY', dir=dir, pattern='SECTION', dec=dec, sep=sep)
dd<-convert_BASF_PY('PY', df)
exportDomain(dd, 'txt', outdir)

