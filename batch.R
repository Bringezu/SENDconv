
# Load library
library(devtools)

# Load Module with all functions
load_all()

# define directories
# indir <- data source directory
# indir<-'/opt/provantis/DA/LIVE/'
indir<-'c:/temp/MQUEST_SEND/Provantis/DA/LIVE/'

# outdir target directory for storing the SEND files
outdir<-paste0(indir, 'output/')

# load configuration from local project environment
c<-config()

for (i in seq_along(c$run)) {
  d<-loadDomain(c$name[i], indir)
  dd<-get(paste0(c$run[i]))(d)
  exportDomain(dd, 'txt', outdir)
}