library(dplR)
library(ggplot2)
library(reshape2)
library(dplyr)

###############################################################################################
## read in data
###############################################################################################

# site metadata
meta = read.csv("ITRDB_NA_PSME_FLAG.csv")
# meta$flag = NA

# climate data
ppt = read.csv('data/ppt_CRU.csv')
ppt = ppt[, which(colnames(ppt) != 'X00')]
colnames(ppt) = c('long', 'lat', 'study.id', 'variable', 'year', 
                  'jan', 'feb', 'mar', 'apr', 'may', 'june', 'july', 'aug', 'sep', 'oct', 'nov', 'dec')

tmean = read.csv('data/tmean_CRU.csv')
colnames(tmean) = c('long', 'lat', 'study.id', 'variable', 'year', 
                  'jan', 'feb', 'mar', 'apr', 'may', 'june', 'july', 'aug', 'sep', 'oct', 'nov', 'dec')


# ppt_CRU = read.csv('data/ppt_CRU.csv')
# tmean_CRU = read.csv('data/tmean_CRU.csv')
# 
# missing = unique(ppt_CRU$study.id)
# 
# ppt = ppt[which(!(ppt$study.id %in% missing)),]
# tmean = tmean[which(!(tmean$study.id %in% missing)),]
# 
# ppt = rbind(ppt, ppt_CRU)
# tmean = rbind(tmean, tmean_CRU)

# get crn filenames
fnames = list.files(path="output/crn/", pattern = "*.crn")
nfnames = length(fnames)

###############################################################################################
## linear regression between june ppt and crn
## to reproduce paper, will need to modify this so regression done for all months (I think)
###############################################################################################

lm.ppt = data.frame(site.id = character(0), 
                     site.num=numeric(0), 
                     lat = numeric(0), 
                     long=numeric(0),
                     slope = numeric(0),
                     rsq = numeric(0))
                     # data.frame(matrix(NA, nrow=0, ncol=12))) 

for (i in 1:nfnames) {
  print(i)
  print(fnames[i])
  
  site.id = toupper(strsplit(fnames[i], '[.]')[[1]][1])
  site.num = meta[match(toupper(site.id), meta$study.name), 'X']
  
  ppt.sub = ppt[which(ppt$study.id == site.id),]
  
  crn = read.crn(paste0("output/crn/",fnames[i]))
  crn.df = data.frame(year=rownames(crn), value=crn[,1])
  
  dat = merge(crn.df, ppt.sub, by.x='year', by.y='year')
  
  if (all(is.na(dat$june))) {
    next
  }
  
  # lm.ppt = cor(dat$value, dat[,7:ncol(dat)])
  fit = lm(value ~ june, data=dat)
  
  slope.ppt = fit$coefficients[2]
  
  rsq.ppt = summary(fit)$r.squared
  
  lm.sub = data.frame(site.id = site.id, 
                       site.num=site.num, 
                       lat = dat$lat[1], 
                       long = dat$long[1], 
                       slope = slope.ppt,
                       rsq = rsq.ppt)
  lm.ppt = rbind(lm.ppt, lm.sub)
  
}

write.csv(lm.ppt, 'output/lm.dat.ppt.csv', row.names=FALSE)

# 
# lm.melt = melt(lm.dat, id.vars=c('site.id', 'site.num', 'lat', 'long'))
# 
ggplot(data=lm.ppt) +
  geom_point(aes(x=lat, y=slope))

ggplot(data=lm.ppt) +
  geom_point(aes(x=lat, y=rsq))

# 
# ggplot(data=subset(cor.melt, variable=='X08')) +
#   geom_point(aes(x=lat, y=value))


###############################################################################################
## linear regression between june tmean and crn
## to reproduce paper, will need to modify this so regression done for all months (I think)
###############################################################################################

# for now commented out because I didn't change it to do the regression yet
# needs to essentially be identical to ppt regression above, but for tmean

# cor.dat = data.frame(site.id = character(0),
#                      site.num=numeric(0),
#                      lat = numeric(0),
#                      long=numeric(0),
#                      data.frame(matrix(NA, nrow=0, ncol=12)))
# 
# for (i in 1:nfnames) {
#   print(i)
#   print(fnames[i])
#   
#   site.id = strsplit(fnames[i], '[.]')[[1]][1]
#   site.num = meta[match(toupper(site.id), meta$study.name), 'X']
#   
#   # tmean.sub = tmean[which(tmean$mypoints.id == site.num),]
#   tmean.sub = tmean[which(tmean$mypoints.id == toupper(site.id)),]
#   
#   crn = read.crn(paste0("Output/crn/",fnames[i]))
#   crn.df = data.frame(year=rownames(crn), value=crn[,1])
#   
#   dat = merge(crn.df, tmean.sub, by.x='year', by.y='Year')
#   
#   cor.tmean = cor(dat$value, dat[,7:ncol(dat)])
#   
#   cor.sub = data.frame(site.id = site.id,
#                        site.num=site.num,
#                        lat = dat$lat[1],
#                        long= dat$long[1],
#                        cor.tmean)
#   cor.dat = rbind(cor.dat, cor.sub)
#   
# }
# 
# write.csv(cor.dat, 'Output/cor.dat.tmean.csv', row.names=FALSE)
# 
# cor.melt = melt(cor.dat, id.vars=c('site.id', 'site.num', 'lat', 'long'))
# 
# ggplot(data=cor.melt) +
#   geom_point(aes(x=site.num, y=value))
# 
# ggplot(data=subset(cor.melt, variable=='X08')) +
#   geom_point(aes(x=lat, y=value))

###############################################################################################
## calculate drought index
###############################################################################################

# for each site, for each year, determine drought index
# drought index in chen:
# average temperature from april to june of current growing season
# divided by total precip from september previous year to june of the current

# determine cumulative precip from prev sept to current june
ppt.cum = data.frame(site.id = character(0), 
                    site.num=numeric(0), 
                    lat = numeric(0), 
                    long=numeric(0),
                    year = numeric(0),
                    ppt = numeric(0))

for (i in 1:nfnames) {
  print(i)
  print(fnames[i])
  
  site.id = toupper(strsplit(fnames[i], '[.]')[[1]][1])
  site.num = meta[match(toupper(site.id), meta$study.name), 'X']
  
  ppt.sub = ppt[which(ppt$study.id == site.id),]
  
  years = sort(ppt.sub$year)
  N_years = length(years)
  
  for (j in 1:N_years){
    if (j == 1) {
      next
    }
    sum.prev = sum(ppt.sub[which(ppt.sub$year == years[j-1]),14:17], na.rm=TRUE)
    sum.current = sum(ppt.sub[which(ppt.sub$year == years[j]),6:11], na.rm=TRUE)
    sum.tot = sum.prev + sum.current
    
    
    ppt.cum.sub = data.frame(site.id = site.id,
                         site.num=site.num,
                         lat = ppt.sub$lat[1],
                         long= ppt.sub$long[1],
                         year = years[j],
                         ppt = sum.tot)
    ppt.cum = rbind(ppt.cum, ppt.cum.sub)
    
  }
  
}

# determine average temperature for current april, may, june
# take average of tmean values for these months
tmean.avg = data.frame(site.id = character(0), 
                     site.num=numeric(0), 
                     lat = numeric(0), 
                     long=numeric(0),
                     year = numeric(0),
                     tmean = numeric(0))


#help andria this is where we get stuck!!!
for (i in 1:nfnames) {
  print(i)
  print(fnames[i])
  
  site.id = toupper(strsplit(fnames[i], '[.]')[[1]][1])
  site.num = meta[match(toupper(site.id), meta$study.name), 'X']
  
  tmean.sub = tmean[which(tmean$study.id == toupper(site.id)),]
  if(any(duplicated(tmean.sub))){
    tmean.sub = tmean.sub[!duplicated(tmean.sub),]
  }
  
  years = sort(tmean.sub$year)
  N_years = length(years)
  
  for (j in 1:N_years){
    tmean.current = mean(as.numeric(tmean.sub[which(tmean.sub$year == years[j]),9:11], na.rm=TRUE))
    
    tmean.avg.sub = data.frame(site.id = site.id,
                             site.num=site.num,
                             lat = tmean.sub$lat[1],
                             long= tmean.sub$long[1],
                             year = years[j],
                             tmean = tmean.current)
    tmean.avg = rbind(tmean.avg, tmean.avg.sub)
    
  }
  
}

# merge the data frame of average temp and cumulative ppt
di.dat = merge(tmean.avg, ppt.cum)

# add a drought index column
# determined by average temp divided by cumulative ppt 
di.dat$drought = di.dat$tmean/di.dat$ppt

###############################################################################################
## linear regression between drought index and chronology
## by site
###############################################################################################

lm.drought = data.frame(site.id = character(0), 
                    site.num=numeric(0), 
                    lat = numeric(0), 
                    long=numeric(0),
                    slope = numeric(0),
                    rsq = numeric(0))
# data.frame(matrix(NA, nrow=0, ncol=12))) 

for (i in 1:nfnames) {
  print(i)
  print(fnames[i])
  
  site.id = strsplit(fnames[i], '[.]')[[1]][1]
  site.num = meta[match(toupper(site.id), meta$study.name), 'X']
  
  di.sub = di.dat[which(di.dat$site.num == site.num),]
  
  crn = read.crn(paste0("output/crn/",fnames[i]))
  crn.df = data.frame(year=rownames(crn), value=crn[,1])
  
  dat = merge(crn.df, di.sub, by.x='year', by.y='year')
  
  if (all(is.na(dat$drought))) {
    next
  }
  
  if (any(is.infinite(dat$drought))){
    dat$drought[which(is.infinite(dat$drought))] = NA
  }
  
  # lm.ppt = cor(dat$value, dat[,7:ncol(dat)])
  fit = lm(value ~ drought, data=dat)
  
  slope.drought = fit$coefficients[2]
  
  rsq.drought = summary(fit)$r.squared
  
  lm.sub = data.frame(site.id = site.id, 
                      site.num=site.num, 
                      lat = dat$lat[1], 
                      long = dat$long[1], 
                      slope = unname(slope.drought),
                      rsq = rsq.drought)
  print(lm.sub)
  lm.drought = rbind(lm.drought, lm.sub)
  
}

write.csv(lm.drought, 'output/lm.dat.drought.csv', row.names=FALSE)

###############################################################################################
## figures
###############################################################################################

# histogram of drought indices
ggplot(data = di.dat) + 
  geom_histogram(aes(x=drought))

# regression slope by latitude
# note some slope values large and positive; why?
ggplot(data=lm.drought) +
  geom_point(aes(x=lat, y=slope))

# regression slope by longitude
ggplot(data=lm.drought) +
  geom_point(aes(x=long, y=slope))

# based on the two figures above, it looks like the sites in question 
# are all at about the same location (lat and long)

# rsquared value by latitude
# note some rsquared value quite low for higher latitudes; why?
ggplot(data=lm.drought) +
  geom_point(aes(x=lat, y=rsq))

lm.hi = lm.drought[which(lm.drought$slope > 20),]
lm.lo = lm.drought[which(lm.drought$slope < (-50)),]

# note that the sites that have outlier slopes are all from wa*** sites
# but not all wa*** sites have outlier slopes
lm.drought[which(substr(lm.drought$site.id,1,2)=='wa'),]




di.sub = di.dat[which(di.dat$site.id == toupper(lm.bad$site.id[1])),]

crn = read.crn(paste0("output/crn/",paste0(lm.bad$site.id[1], '.crn')))
crn.df = data.frame(year=rownames(crn), value=crn[,1])

dat = merge(crn.df, di.sub, by.x='year', by.y='year')

ggplot(data=dat) + geom_point(aes(x=value, y=drought))


