library(dplR)
library(ggplot2)
library(reshape2)
library(dplyr)

meta=read.csv("ITRDB_NA_PSME.csv")
meta$flag = NA

fnames = list.files(path="Data/PSME-North_America", pattern = "*.rwl")
nfnames = length(fnames)

# floating chronologies
future = c('cana163.rwl', 'cana164.rwl', 'cana165.rwl', 'cana166.rwl',  'cana167.rwl')

for (i in 1:nfnames) {
  print(i)
  print(fnames[i])
  if (fnames[i] %in% future){
    next
  }
  dat=read.rwl(fname = paste0("Data/PSME-North_America/", fnames[i]))
  detrended=detrend(dat, method = "Mean", nyrs = 30)
  dat.crn=chron(detrended)
  sitename=strsplit(fnames[i], "\\.")[[1]][1]
  print(sitename)
  write.crn(dat.crn, fname=paste0("Output/crn/",sitename,".crn"))
  idx = which(meta$study.name==toupper(sitename))
  meta[idx, ]$flag="include"
}

write.csv(meta, "ITRDB_NA_PSME_FLAG.csv")
