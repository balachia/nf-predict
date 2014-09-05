library(data.table)
library(reshape2)
library(ggplot2)

rm(list=ls())

setwd('~/Data/nf-raw/svdpp')

d <- 50

rmse.by.ucount <- readRDS(paste0('out/',d,'/rmse-by-user-rating-count.Rds'))
rmse.by.mcount <- readRDS(paste0('out/',d,'/rmse-by-movie-rating-count.Rds'))

rmse.by.ucount[, leftratings := sum(nratings) - cumsum(nratings) + nratings]
rmse.by.mcount[, leftratings := sum(nratings) - cumsum(nratings) + nratings]

mrmse.ucount <- melt(rmse.by.ucount, id.vars=c('user_rating_count','nratings'))
mrmse.mcount <- melt(rmse.by.mcount, id.vars=c('movie_rating_count','nratings'))

mrmse.ucount[, value_norm := value / max(value, na.rm=TRUE), by=variable]
mrmse.mcount[, value_norm := value / max(value, na.rm=TRUE), by=variable]

setwd(paste0('out/',d,'/plots/'))

png('rmse-curve-user.png', width=1200, height=900)
ggplot(mrmse.ucount[variable=='rmse'], aes(user_rating_count, value)) +
    scale_color_brewer() + 
    scale_x_log10(breaks=10^seq(1,5,1)) +
    xlab('# Prior Ratings by User') + ylab('RMSE of Ratings in Group') +
    theme(text=element_text(size=24)) + 
    geom_point()
dev.off()

png('rmse-cdf-curve-user.png', width=1200, height=900)
ggplot(mrmse.ucount, aes(user_rating_count, value_norm, color=variable)) + 
    scale_color_brewer(type='qual', name='Value', labels=c('RMSE','Ratings Left\nPast Cutoff')) + 
    xlab('# Prior Ratings by User') + ylab('Fraction of Max Value') +
    scale_x_log10(breaks=10^seq(1,5,1)) +
    scale_y_continuous(breaks=seq(0,1,0.1)) + 
    theme(text=element_text(size=24)) + 
    geom_point()
dev.off()

png('rmse-curve-movie.png', width=1200, height=900)
ggplot(mrmse.mcount[variable=='rmse'], aes(movie_rating_count, value)) +
    scale_color_brewer() + 
    scale_x_log10(breaks=10^seq(1,5,1)) +
    xlab('# Prior Ratings for Movie') + ylab('RMSE of Ratings in Group') +
    theme(text=element_text(size=24)) + 
    geom_point()
dev.off()

png('rmse-cdf-curve-movie.png', width=1200, height=900)
ggplot(mrmse.mcount, aes(movie_rating_count, value_norm, color=variable)) + 
    scale_color_brewer(type='qual', name='Value', labels=c('RMSE','Ratings Left\nPast Cutoff')) + 
    xlab('# Prior Ratings for Movie') + ylab('Fraction of Max Value') +
    scale_x_log10(breaks=10^seq(1,5,1)) +
    scale_y_continuous(breaks=seq(0,1,0.1)) + 
    theme(text=element_text(size=24)) + 
    geom_point()
dev.off()

