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

dat = read.csv('data/dat_ecomem.csv', stringsAsFactors = FALSE)

site.ids = unique(dat$site.id)
N.sites = length(site.ids)

site.use = site.ids[sample(N.sites, size=100)]

dat.sub = dat[which(dat$site.id %in% site.use),]

#################################################################################################
## try with agbi and drought
#################################################################################################
library(EcoMem)

# Fit ecological memory model
mod = ecomem(value ~ drought + lat, 
             data = dat.sub, 
             mem.vars = "drought",
             L = 6, 
             timeID = "year", 
             groupID = "site.id",
             n.post = 500, 
             thin = 10,
             burn.in = 1000)


# Summarize marginal posterior distributions of model parameters
boreal.mem.summ = memsum(mod, verbose = FALSE)
boreal.mem.summ 

# Plot boreal tree growth memory to ftc defoliation
plotmem(mod)



# Fit ecological memory model
mod = ecomem(value ~ snow_winter + lat + long, 
             data = dat.sub, 
             mem.vars = "snow_winter",
             L = 5, 
             timeID = "year", 
             groupID = "site.id",
             n.post = 500, 
             thin = 10,
             burn.in = 1000)


# Summarize marginal posterior distributions of model parameters
boreal.mem.summ = memsum(mod, verbose = FALSE)
boreal.mem.summ 

# Plot boreal tree growth memory to ftc defoliation
plotmem(mod)

# coef.summ = memsum(mod, cred.int = 0.95, verbose = FALSE)
# coef.summ$var.type = c(rep('beta', n.covars + 1 + length(mem.vars)), 'sig.y', rep(rep('wts', L+1), length(mem.vars)))
# 
# coef.summ$var = factor(coef.summ$var, rev(coef.summ$var))
# 
# coef.summ = coef.summ[which(!(coef.summ$var.type %in% c('sig.y'))),]
# 
# ggplot(aes(x = var, y = mean, ymin = q0.025, ymax = q0.975),
#        data = coef.summ) +
#   geom_linerange() +
#   geom_point(shape = 4, size = 2) +
#   # geom_point(aes(y = sim.val, color = In),
#   #            size = 2, alpha = 0.6) +
#   # scale_color_manual(values = c("red","green")) +
#   xlab("Parameter") + ylab("Posterior Summary") +
#   facet_wrap(~ var.type, ncol = 1, scale = "free") +
#   coord_flip() +
#   theme_bw() +
#   labs(color = "Simulated value in\ncredible interval")
# ggsave('figures/posterior_samples_ppt_dormat.png')
