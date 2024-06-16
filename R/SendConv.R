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
#' SendConv(domains, indir, outdir)
SendConv<-function(domains, indir, outdir){
  stopifnot(is.character(domains))
  stopifnot(file.exists(indir))
  stopifnot(file.exists(outdir))
  
  for (i in domains){
    d<-loadDomain(i, indir)
    
    switch(i, 
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