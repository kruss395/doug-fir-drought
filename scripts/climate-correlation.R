library(ggplot2)
library(tidyr)
library(scales)
library(forcats)
library(dplR)
library(reshape2)
library(mgcv)
library(dplyr)

# site metadata
meta = read.csv("data/ITRDB_NA_PSME_FLAG.csv")

fnames = list.files(path="output/crn/", pattern = "*.crn")
nfnames = length(fnames)

lnames = c(paste0('P',c('may', 'june', 'july', 'aug', 'sep', 'oct', 'nov', 'dec')),
           c('jan', 'feb', 'mar', 'apr', 'may', 'june', 'july', 'aug', 'sep', 'oct', 'nov', 'dec'))

#################################################################################################
## read in climate data
#################################################################################################

tmean = read.csv('data/tmean_CRU.csv', stringsAsFactors = FALSE)

cor.tmean = data.frame(site.id = character(0), 
                     site.num=numeric(0), 
                     lat = numeric(0), 
                     long=numeric(0),
                     data.frame(matrix(NA, nrow=0, ncol=20))) 

for (i in 1:nfnames) {
  print(i)
  # print(sites[i])
  
  site.id = toupper(strsplit(fnames[i], '[.]')[[1]][1])
  site.num = meta[match(site.id, meta$study.name), 'X']
  
  tmean.sub = tmean[which(tmean$study.id == site.id),]
  
  crn = read.crn(paste0("output/crn/",fnames[i]))
  crn.df = data.frame(year=rownames(crn), value=crn[,1])
  
  # agbi.site = agbi.mean[which(agbi.mean$site.orig == sites[i]),]
  
  dat = merge(crn.df, tmean.sub)#, by.x='year', by.y='Year')
  dat$year = as.numeric(dat$year)
  
  dat.wide = dat
  # dat.wide = pivot_wider(dat, 
  #                   id_cols = c('year', 'site', 'lat', 'long', 'agbi.mean'), 
  #                   names_from = 'month',
  #                   values_from = 'tmean', 
  #                   names_sort = TRUE)
  # dat.wide = data.frame(dat.wide)
  
  dat.prev = dat.wide[,c('year', 'may', 'june', 'july', 'aug', 'sep', 'oct', 'nov', 'dec')]
  dat.prev$year = dat.prev$year + 1
  colnames(dat.prev) = c('year', paste0('P',c('may', 'june', 'july', 'aug', 'sep', 'oct', 'nov', 'dec')))
  
  dat.wide = merge(dat.wide, dat.prev)
  
  # cor.tmean = cor(dat$agbi.mean, foo[,6])
  cor.vec = cor(dat.wide$value, dat.wide[,7:ncol(dat.wide)])
  
  cor.tmean.sub = data.frame(site.id  = site.id,
                       site.num = site.num,
                       lat  = dat.wide$lat[1],
                       long = dat.wide$long[1],
                       cor.vec)
  cor.tmean = rbind(cor.tmean, cor.tmean.sub)
  
}

# image(t(as.matrix(cor.dat[,5:ncol(cor.dat)])))

cor.tmean.melt = melt(cor.tmean, id.vars=c('site.id', 'site.num', 'lat', 'long'))
cor.tmean.melt$variable = factor(cor.tmean.melt$variable, levels=lnames)
                                   
write.csv(cor.tmean.melt, 'output/cor.dat.tmean.csv', row.names=FALSE)

#################################################################################################
## read in ppt data
#################################################################################################

ppt = read.csv('data/ppt_CRU.csv', stringsAsFactors = FALSE)


cor.ppt = data.frame(site.id = character(0), 
                     site.num=numeric(0), 
                     lat = numeric(0), 
                     long=numeric(0),
                     data.frame(matrix(NA, nrow=0, ncol=12))) 

for (i in 1:nfnames) {
  print(i)
  # print(sites[i])
  
  site.id = toupper(strsplit(fnames[i], '[.]')[[1]][1])
  site.num = meta[match(site.id, meta$study.name), 'X']
  
  ppt.sub = ppt[which(ppt$study.id == site.id),]
  
  crn = read.crn(paste0("output/crn/",fnames[i]))
  crn.df = data.frame(year=rownames(crn), value=crn[,1])
  
  # agbi.site = agbi.mean[which(agbi.mean$site.orig == sites[i]),]
  
  dat = merge(crn.df, ppt.sub)#, by.x='year', by.y='Year')
  dat$year = as.numeric(dat$year)
  dat.wide = dat
  
  dat.prev = dat.wide[,c('year', 'may', 'june', 'july', 'aug', 'sep', 'oct', 'nov', 'dec')]
  dat.prev$year = dat.prev$year + 1
  colnames(dat.prev) = c('year', paste0('P',c('may', 'june', 'july', 'aug', 'sep', 'oct', 'nov', 'dec')))
  
  dat.wide = merge(dat.wide, dat.prev)
  
  # cor.tmean = cor(dat$agbi.mean, foo[,6])
  cor.vec = cor(dat.wide$value, dat.wide[,7:ncol(dat.wide)])
  
  cor.ppt.sub = data.frame(site.id  = site.id,
                       site.num = site.num,
                       lat  = dat.wide$lat[1],
                       long = dat.wide$long[1],
                       cor.vec)
  cor.ppt = rbind(cor.ppt, cor.ppt.sub)
  
}

image(t(as.matrix(cor.ppt[,5:ncol(cor.ppt)])))

cor.ppt.melt = melt(cor.ppt, id.vars=c('site.id', 'site.num', 'lat', 'long'))
cor.ppt.melt$variable = factor(cor.ppt.melt$variable, levels=lnames)


write.csv(cor.ppt.melt, 'output/cor.dat.ppt.csv', row.names=FALSE)

#################################################################################################
## read in climate data
#################################################################################################

tmin = read.csv('data/tmin_CRU.csv', stringsAsFactors = FALSE)

cor.tmin = data.frame(site.id = character(0), 
                     site.num=numeric(0), 
                     lat = numeric(0), 
                     long=numeric(0),
                     data.frame(matrix(NA, nrow=0, ncol=12))) 

for (i in 1:nfnames) {
  print(i)
  
  site.id = toupper(strsplit(fnames[i], '[.]')[[1]][1])
  site.num = meta[match(site.id, meta$study.name), 'X']
  
  tmin.sub = tmin[which(tmin$study.id == site.id),]
  
  crn = read.crn(paste0("output/crn/",fnames[i]))
  crn.df = data.frame(year=rownames(crn), value=crn[,1])
  
  dat = merge(crn.df, tmin.sub)#, by.x='year', by.y='Year')
  dat$year = as.numeric(dat$year)
  
  dat.wide = dat
  
  dat.prev = dat.wide[,c('year', 'may', 'june', 'july', 'aug', 'sep', 'oct', 'nov', 'dec')]
  dat.prev$year = dat.prev$year + 1
  colnames(dat.prev) = c('year', paste0('P',c('may', 'june', 'july', 'aug', 'sep', 'oct', 'nov', 'dec')))
  
  dat.wide = merge(dat.wide, dat.prev)
  
  # cor.tmin = cor(dat$agbi.mean, foo[,6])
  cor.vec = cor(dat.wide$value, dat.wide[,7:ncol(dat.wide)])
  
  cor.tmin.sub = data.frame(site.id  = site.id,
                       site.num = site.num,
                       lat  = dat.wide$lat[1],
                       long = dat.wide$long[1],
                       cor.vec)
  cor.tmin = rbind(cor.tmin, cor.tmin.sub)
  
}

image(t(as.matrix(cor.tmin[,5:ncol(cor.tmin)])))

cor.tmin.melt = melt(cor.tmin, id.vars=c('site.id', 'site.num', 'lat', 'long'))
cor.tmin.melt$variable = factor(cor.tmin.melt$variable, levels=lnames)

write.csv(cor.tmin.melt, 'output/cor.dat.tmin.csv', row.names=FALSE)

#################################################################################################
## tmax
#################################################################################################

tmax = read.csv('data/tmax_CRU.csv', stringsAsFactors = FALSE)

cor.tmax = data.frame(site.id = character(0), 
                      site.num=numeric(0), 
                      lat = numeric(0), 
                      long=numeric(0),
                      data.frame(matrix(NA, nrow=0, ncol=12))) 

for (i in 1:nfnames) {
  print(i)
  
  site.id = toupper(strsplit(fnames[i], '[.]')[[1]][1])
  site.num = meta[match(site.id, meta$study.name), 'X']
  
  tmax.sub = tmax[which(tmax$study.id == site.id),]
  
  crn = read.crn(paste0("output/crn/",fnames[i]))
  crn.df = data.frame(year=rownames(crn), value=crn[,1])
  
  dat = merge(crn.df, tmax.sub)#, by.x='year', by.y='Year')
  dat$year = as.numeric(dat$year)
  
  dat.wide = dat
  
  dat.prev = dat.wide[,c('year', 'may', 'june', 'july', 'aug', 'sep', 'oct', 'nov', 'dec')]
  dat.prev$year = dat.prev$year + 1
  colnames(dat.prev) = c('year', paste0('P',c('may', 'june', 'july', 'aug', 'sep', 'oct', 'nov', 'dec')))
  
  dat.wide = merge(dat.wide, dat.prev)
  
  # cor.tmax = cor(dat$agbi.mean, foo[,6])
  cor.vec = cor(dat.wide$value, dat.wide[,7:ncol(dat.wide)])
  
  cor.tmax.sub = data.frame(site.id  = site.id,
                            site.num = site.num,
                            lat  = dat.wide$lat[1],
                            long = dat.wide$long[1],
                            cor.vec)
  cor.tmax = rbind(cor.tmax, cor.tmax.sub)
  
}

image(t(as.matrix(cor.tmax[,5:ncol(cor.tmax)])))

cor.tmax.melt = melt(cor.tmax, id.vars=c('site.id', 'site.num', 'lat', 'long'))
cor.tmax.melt$variable = factor(cor.tmax.melt$variable, levels=lnames)

write.csv(cor.tmax.melt, 'output/cor.dat.tmax.csv', row.names=FALSE)

#################################################################################################
## drought index
#################################################################################################

di = read.csv('data/drought_CRU.csv', stringsAsFactors = FALSE)

cor.di = data.frame(site.id = character(0), 
                      site.num=numeric(0), 
                      lat = numeric(0), 
                      long=numeric(0),
                      data.frame(matrix(NA, nrow=0, ncol=3))) 

for (i in 1:nfnames) {
  print(i)
  
  site.id = toupper(strsplit(fnames[i], '[.]')[[1]][1])
  site.num = meta[match(site.id, meta$study.name), 'X']
  
  di.sub = di[which(di$site.id == site.id),]
  
  crn = read.crn(paste0("output/crn/",fnames[i]))
  crn.df = data.frame(year=rownames(crn), value=crn[,1])
  
  dat = merge(crn.df, di.sub)#, by.x='year', by.y='Year')
  dat$year = as.numeric(dat$year)
  
  dat.wide = dat
  
  # dat.prev = dat.wide[,c('year', 'may', 'june', 'july', 'aug', 'sep', 'oct', 'nov', 'dec')]
  # dat.prev$year = dat.prev$year + 1
  # colnames(dat.prev) = c('year', paste0('P',c('may', 'june', 'july', 'aug', 'sep', 'oct', 'nov', 'dec')))
  # 
  # dat.wide = merge(dat.wide, dat.prev)
  # 
  # cor.tmax = cor(dat$agbi.mean, foo[,6])
  cor.vec = cor(dat.wide$value, dat.wide[,7:ncol(dat.wide)])
  
  cor.di.sub = data.frame(site.id  = site.id,
                            site.num = site.num,
                            lat  = dat.wide$lat[1],
                            long = dat.wide$long[1],
                            cor.vec)
  cor.di = rbind(cor.di, cor.di.sub)
  
}

image(t(as.matrix(cor.di[,5:ncol(cor.di)])))

dnames = c('tmean.grow', 'ppt.dormant', 'drought')
colnames(cor.di) = c('site.id', 'site.num', 'lat', 'long', 'tmean.grow', 'ppt.dormant', 'drought')

cor.di.melt = melt(cor.di, id.vars=c('site.id', 'site.num', 'lat', 'long'))
cor.di.melt$variable = factor(cor.di.melt$variable, levels=dnames)

write.csv(cor.di.melt, 'output/cor.dat.di.csv', row.names=FALSE)


#################################################################################################
## plot correlation
#################################################################################################
dat = read.csv('output/cor.dat.di.csv')
# set dat to be cor.VAR.melt, where VAR is tmean, tmax, tmin, ppt
# dat = cor.tmean.melt
# dat = cor.ppt.melt

dat$site.id = fct_reorder(dat$site.id, dat$lat)

foo = dat %>% 
  pivot_wider(id_cols=c('site.id', 'site.num', 'lat', 'long'), names_from=variable, values_from=value) 

cor(foo$ppt.dormant, foo$drought, na.rm=TRUE)
  

ggplot(data=dat) + 
  geom_tile(aes(x=variable, y=site.id, fill=value)) +
  scale_fill_gradient2(low = muted("red"),
                       mid = "white",
                       high = muted("blue"),
                       midpoint = 0,
                       space = "Lab",
                       na.value = "grey50",
                       limits = c(-1, 1))

ggplot(data=dat) +
  geom_point(aes(x=variable, y=value))

ggplot(data=dat) +
  geom_boxplot(aes(x=variable, y=value)) +
  geom_hline(yintercept=0, linetype='dashed', col = 'red')

ggplot(data=dat) +
  geom_point(aes(x=lat, y=value, colour=variable)) +
  geom_smooth(aes(x=lat, y=value, colour=variable), formula= y~x, method=lm, se=TRUE, size=1) 

ggplot(data=dat) +
  geom_point(aes(x=lat, y=value, colour=variable)) +
  geom_smooth(aes(x=lat, y=value, colour=variable), formula= y~x+I(x^2), method=lm, se=TRUE, size=1) 

ggplot(data=dat) +
  geom_point(aes(x=lat, y=value)) +
  geom_smooth(aes(x=lat, y=value), formula= y~x+I(x^2), method=lm, se=TRUE, size=1) +
  facet_wrap(~variable)

ggplot(data=dat) +
  geom_point(aes(x=lat, y=value)) +
  geom_smooth(aes(x=lat, y=value), formula= y~x, method=loess, se=TRUE, size=1) +
  facet_wrap(~variable)

ggplot(data=dat) +
  geom_point(aes(x=lat, y=value)) +
  geom_smooth(aes(x=lat, y=value), formula= y~s(x), method="gam", se=TRUE, size=1) +
  facet_wrap(~variable)

ggplot(data=dat) +
  geom_histogram(aes(x=value)) + 
  facet_wrap(~variable)


