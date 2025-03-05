
# Load library
library(devtools)

# Load Module with all functions
load_all()

# define directories
# indir <- data source directory

## list of indirs is provided below
indir<-c('/opt/provantis/DA/LIVE/', '/opt/provantis/DA/ARCH/','/opt/provantis/IV/LIVE/', '/opt/provantis/IV/ARCH/')



# indir<-'c:/temp/MQUEST_SEND/Provantis/DA/LIVE/'

for (i in seq_along(indir)) {
  

# outdir target directory for storing the SEND files
outdir<-paste0(indir[i], 'output/')

# load configuration from local project environment
c<-config()

for (i in seq_along(c$run)) {
  d<-loadDomain(c$name[i], indir[i])
  dd<-get(paste0(c$run[i]))(d)
  exportDomain(dd, 'txt', outdir)
}

}