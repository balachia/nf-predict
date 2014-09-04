library(data.table)
library(ggplot2)

rm(list=ls())

setwd('~/Data/nf-raw/svdpp/')

d <- 50

dt <- readRDS(paste0('out/',d,'/movie_residuals.Rds'))
write.foreign(dt, 'movie_residuals.txt', 'movie_residuals_code.do', package='Stata')
write.dta(dt, 'movie_residuals.dta')

ggplot(dt, aes(rating_date, lchi_pval)) + geom_jitter(alpha=0.1, size=0.5)
ggplot(dt, aes(rating_date, sqrt(resid_var))) + geom_jitter(alpha=0.1, size=0.5)
