# source(paste0("C:/temp/portableApps/Documents/___study/convert_utils.r"))
source(paste0("C:/Temp/Shiny Apps/SEND Converter/convert_utils.r"))
options(warn=-1)
domains<-c('BW','CL', 'CV', 'DS', 'DM', 'EG','FC','LB','MA','MI','OM', 'EX')
#domains<-c('EX')
for (i in domains){
  i_data<-load_domain(i)
  i_data_c<-convert_domain(i_data,as.character(i))
  export_domain(i_data_c, i, 'csv')
}
