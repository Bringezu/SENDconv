#' SendConv
#'
#' @param domains character list of domains to be processed
#' @param indir directory for input files
#' @param outdir directory for outout files 
#'
#'
#' @examples
#' domains<-c('BW', 'LB')
#' indir<-'c:/temp/mquest_send/Provantis/DA/ARCH'
#' outdir<-'c:/temp/mquest_send/Provantis/DA/ARCH/output'
#' SendConf(domains, indir, outdir)
SendConv<-function(domains, indir, outdir){
  stopifnot(is.character(domains), 'Please provide a list of domains to be processed!')
  stopifnot(file.exists(indir), 'Please select a directory name to read the input!')
  stopifnot(file.exists(outdir), 'Please select a directory name to store the output!')
  
  for (i in domains){
    d<-loadDomain(domains[i], indir)
    
    switch(domains[i], 
           'BW'={
             cd<-convertBW('BW', d)
            },
           'CL'={
             
             
           },
           'CV'={
             
             
             
           },
           'DS'={
             
           }, 
           'DM'= {
             
             
           },
           'LB' = {
             
             
           },
           'EG'={
             
             
           },
           'EX'={
             
             
           },
           'FC'={
             
             
           },
           'MA' = {
             
           },
           'MI' = {
             
           },
           'OM' = {
             
           },
           
           
           'RE' = {
             
           },
           
           
           'TA' = {
             
           },
           
           
           'VS' = {
           },
           'TS' = {
             
           },
           
           print(paste0("Domain: ", domain," not configured!"))
    )   
    
    
    exportDomain(cd,'csv', outdir)
  }
  
  
}