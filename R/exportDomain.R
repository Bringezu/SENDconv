#' exportDomain
#'
#' @param domain SEND Domain compiled by convert function
#' @param outdir directory name for saving the output files
#' @param fileformat either csv, or pdf are supported
#' @export
#' 
#' @examples
#' domain<-'BW'
#' bw<-convert(domain)
#' exportDomain(bw, 'txt', 'outdir')
#'
exportDomain<-function(domain, fileformat, outdir){
  stopifnot(is.data.frame(domain), length(domain) >=1)
  stopifnot(is.character(fileformat))
  stopifnot(file.exists(outdir))
  
  switch (fileformat,
          'pdf' = {
            grDevices::pdf(paste0('SEND_',domain$DOMAIN %>% unique(),'.pdf'))
            gridExtra::grid.table(domain)
            grDevices::dev.off()
          },
          'txt' = {
            # Encoding(domain)<-'UTF-8'
            #write.table(domain, file=paste0(outdir,'SEND_',name,'.txt'), quote = F, fileEncoding = 'UTF-8', row.names = F, na='', dec='.', sep='\t')
            
            data.table::fwrite(domain, file=paste0(outdir,'SEND_',domain$DOMAIN %>% unique(),'.txt'), quote = F, showProgress=T, row.names = F, na='', dec='.', sep='\t', encoding = "UTF-8")
            # write_delim(domain, path = paste0(outdir,'SEND_',name,'.txt'), delim = "\t")
          }
  )
  
} 