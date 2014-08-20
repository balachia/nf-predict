library(Matrix)
library(data.table)

rm(list=ls())
options(max.print=5000)

readMM.dense <- function(fin, skip=3, ...) {
    dt <- fread(fin, skip=skip, ...)
    as.matrix(dt)
}

d <- 50

setwd(paste0('~/Data/nf-raw/svdpp/out/', d))

# load data
mu <- readMM.dense('all-nf.mm_global_mean.mm')
bi <- readMM.dense('all-nf.mm_V_bias.mm')
qi <- readMM.dense('all-nf.mm_V.mm')
bu <- readMM.dense('all-nf.mm_U_bias.mm')
pwu <- readMM.dense('all-nf.mm_U.mm')

ratings <- readRDS('ratings.Rds')
idxi <- ratings[, movie_id]
idxu <- ratings[, user_idx]
rm(ratings); gc()

# drop last column and reformat
mu <- mu[1]
qi <- qi[,1:d]
pwu <- pwu[,1:d] + pwu[,1:d + d]

nu <- nrow(bu)
ni <- nrow(bi)

# test predictions
res <- lapply(1:nrow(ratings), function (i) {
#     res <- mu + bi[i] + bu + crossprod(qi[i,], pwu)
    res <- mu + bi[idxi[i]] + bu[idxu[i]] + (pwu[idxu[i],] %*% qi[idxi[i],])
#     res <- mu + bi[i] + bu + (pwu %*% qi[i,])
#     res <- c(min(res), mean(res), median(res), max(res))
    if(i %% 1000 == 0) print(c(i, res))
    res
})

