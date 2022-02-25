


## read in ITRDB site data
##
coords = read.csv('ITRDB_NA_PSME.csv')#[,c('lat', 'long')]
# coords = coords[which(coords$keep!='N'),]

site_meta = coords[,c(1,2,5:7)]
colnames(site_meta) = c('ID1', 'ID2', 'lat', 'long', 'el')

site_meta$ID1 = as.character(site_meta$ID1)
site_meta$el = as.numeric(site_meta$el)

write.csv(site_meta, 'Data/climateNA_input.csv', row.names = FALSE, quote=FALSE, na='.', eol = "\r\n")


clim = read.csv('Data/climateNA_1901-2019.csv', stringsAsFactors = FALSE)

ppt = data.frame(clim[,c('Longitude', 'Latitude', 'ID2')], 
                 variable = rep('ppt'), 
                 year = clim[,'Year'], 
                 clim[,which(substr(colnames(clim), 1, 3)=='PPT')])
colnames(ppt) = c('long', 'lat', 'study.id', 'variable', 'year', 
                  'jan', 'feb', 'mar', 'apr', 'may', 'june', 'july', 'aug', 'sep', 'oct', 'nov', 'dec')
write.csv(ppt, 'Data/ppt_CRU.csv', row.names=FALSE)


tmean = data.frame(clim[,c('Longitude', 'Latitude', 'ID2')], 
                 variable = rep('tmean'), 
                 year = clim[,'Year'], 
                 clim[,which(substr(colnames(clim), 1, 3)=='Tav')])
colnames(tmean) = c('long', 'lat', 'study.id', 'variable', 'year', 
                  'jan', 'feb', 'mar', 'apr', 'may', 'june', 'july', 'aug', 'sep', 'oct', 'nov', 'dec')
write.csv(tmean, 'Data/tmean_CRU.csv', row.names=FALSE)

tmin = data.frame(clim[,c('Longitude', 'Latitude', 'ID2')], 
                   variable = rep('tmin'), 
                   year = clim[,'Year'], 
                   clim[,which(substr(colnames(clim), 1, 3)=='Tmi')])
colnames(tmin) = c('long', 'lat', 'study.id', 'variable', 'year', 
                    'jan', 'feb', 'mar', 'apr', 'may', 'june', 'july', 'aug', 'sep', 'oct', 'nov', 'dec')
write.csv(tmin, 'Data/tmin_CRU.csv', row.names=FALSE)

tmax = data.frame(clim[,c('Longitude', 'Latitude', 'ID2')], 
                   variable = rep('tmin'), 
                   year = clim[,'Year'], 
                   clim[,which(substr(colnames(clim), 1, 3)=='Tma')])
colnames(tmax) = c('long', 'lat', 'study.id', 'variable', 'year', 
                    'jan', 'feb', 'mar', 'apr', 'may', 'june', 'july', 'aug', 'sep', 'oct', 'nov', 'dec')
write.csv(tmax, 'Data/tmax_CRU.csv', row.names=FALSE)
