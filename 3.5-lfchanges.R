library(data.table)
library(parallel)
library(foreign)

# have months 11:84 available
# so need to predict 12:84 from 11:83

rm(list=ls())
options(max.print=5000)

readMM.dense <- function(fin, skip=3, keep=NULL, ...) {
    dt <- fread(fin, skip=skip, ...)
    if(!is.null(keep)) dt <- dt[, keep, with=FALSE]
    as.matrix(dt)
}

setwd('~/Data/nf-raw/svdpp/')

d <- 50
valid.movies <- fread('valid_movies_2014.csv')
setnames(valid.movies, names(valid.movies), 'movie_id')
setkey(valid.movies, movie_id)

types <- factor(c('um', 'u0', '0m', '00'))

allqi <- lapply(11:84, function(i) {
    cat(i, 'start\n')
    qi <- readMM.dense(paste0('out/',d,'/',i,'/all-nf-',i,'.mm_V.mm'), keep=1:d)
    qi <- qi[,1:d]
    qi
})

allmaps <- lapply(11:84, function(i) {
    mmap <- readRDS(paste0('mapping/movie-',i,'.Rds'))
    mmap[, rtime := i]
    mmap
})
allmaps <- rbindlist(allmaps)

mids <- unique(allmaps[,movie_id])

# can be sped up with rcpp? stupid sexy for loop
Rprof(tmp <- tempfile(), line.profiling=TRUE)
meandiffs <- sapply(mids, function(mid) {
    whichmid <- which(mids==mid)
    if(whichmid %% 100 == 0 || whichmid == length(mids)) {
        cat(whichmid / length(mids), '\n')
    }

    moviemaps <- allmaps[movie_id == mid]
    mini <- moviemaps[, min(rtime)]
    maxi <- moviemaps[, max(rtime)]

    if(mini==maxi) {
        res <- NA
    } else {
        qrow0 <- allqi[[moviemaps[1, rtime - 10]]][moviemaps[1,movie_idx],]
        diff <- 0
        for(x in 2:nrow(moviemaps)) {
            qrow1 <- allqi[[moviemaps[x, rtime - 10]]][moviemaps[x,movie_idx],]
            diff <- diff + sqrt(sum((qrow1-qrow0)^2))
#             diff <- diff + sqrt(sum((
#                     allqi[[moviemaps[x, rtime - 10]]][moviemaps[x,movie_idx],] -
#                     allqi[[moviemaps[x-1, rtime - 10]]][moviemaps[x-1,movie_idx],]
#                 )^2))
            qrow0 <- qrow1
        }
        res <- diff / (maxi - mini)
    }

#     qis <- sapply(1:nrow(moviemaps), function(x) {
#         allqi[[moviemaps[x, rtime - 10]]][moviemaps[x,movie_idx],]
#     })

    res
})
Rprof(NULL)
summaryRprof(tmp, lines='show')
unlink(tmp)

diffs.dt <- data.table(movie_id=mids, mean_lfdiff=meandiffs)
write.dta(diffs.dt,paste0('out/',d,'/rsync/latent-factor-changes.dta'))

# resdts <- mclapply(12:84, mc.cores=20, function(ipred) {
#     cat(ipred, 'start\n')
#     ptm <- proc.time()
# 
#     i <- ipred - 1
# 
#     umap <- readRDS(paste0('mapping/user-',i,'.Rds'))
#     mmap <- readRDS(paste0('mapping/movie-',i,'.Rds'))

#     setkey(subr, user_id)
#     setkey(umap, user_id)
#     subr <- merge(subr, umap, all.x=TRUE)
# 
#     setkey(subr, movie_id)
#     setkey(mmap, movie_id)
#     subr <- merge(subr, mmap, all.x=TRUE)

    # load data
#     mu <- readMM.dense(paste0('out/',d,'/',i,'/all-nf-',i,'.mm_global_mean.mm'))
#     bi <- readMM.dense(paste0('out/',d,'/',i,'/all-nf-',i,'.mm_V_bias.mm'))
#     qi <- readMM.dense(paste0('out/',d,'/',i,'/all-nf-',i,'.mm_V.mm'), keep=1:d)
#     bu <- readMM.dense(paste0('out/',d,'/',i,'/all-nf-',i,'.mm_U_bias.mm'))
#     pwu <- readMM.dense(paste0('out/',d,'/',i,'/all-nf-',i,'.mm_U.mm'), keep=1:(2*d))

    # drop last column and reformat
#     mu <- mu[1]
#     qi <- qi[,1:d]
#     pwu <- pwu[,1:d] + pwu[,1:d + d]

#     subr[, predicted := as.numeric(NA)]
#     idx.um <- which(subr[, !is.na(user_idx) & !is.na(movie_idx)])
#     idx.u0 <- which(subr[, !is.na(user_idx) & is.na(movie_idx)])
#     idx.0m <- which(subr[, is.na(user_idx) & !is.na(movie_idx)])
#     idx.00 <- which(subr[, is.na(user_idx) & is.na(movie_idx)])
# 
#     # user and movie
#     if(length(idx.um) > 0) {
#         subr[idx.um, 
#              `:=`(predicted = mu + bi[movie_idx] + bu[user_idx] + rowSums(pwu[user_idx,] * qi[movie_idx,]),
#                   type=types[1])]
#     }
#     # user, no movie
#     if(length(idx.u0) > 0) {
#         subr[idx.u0, 
#              `:=`(predicted = mu + bu[user_idx],
#                   type=types[2])]
#     }
#     # movie, no user
#     if(length(idx.0m) > 0) {
#         subr[idx.0m, 
#              `:=`(predicted = mu + bi[movie_idx],
#                   type=types[3])]
#     }
#     # no user, no movie
#     if(length(idx.00) > 0) {
#         subr[idx.00, 
#              `:=`(predicted = mu,
#                   type=types[4])]
#     }

#     cat(ipred, 'done (', proc.time()[3] - ptm[3] ,')\n')

#     subr[, list(movie_id, user_id, predicted, type)]
# })
