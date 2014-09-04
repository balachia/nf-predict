library(data.table)
library(reshape2)
library(ggplot2)

rm(list=ls())

setwd('~/Data/nf-raw/svdpp')

d <- 50

rmse.by.ucount <- readRDS(paste0('out/',d,'/rmse-by-user-rating-count.Rds'))
rmse.by.mcount <- readRDS(paste0('out/',d,'/rmse-by-movie-rating-count.Rds'))


