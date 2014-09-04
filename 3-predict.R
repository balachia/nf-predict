library(Matrix)
library(data.table)
library(parallel)

# have months 11:84 available
# so need to predict 12:84 from 11:83

rm(list=ls())
options(max.print=5000)

readMM.dense <- function(fin, skip=3, ...) {
    dt <- fread(fin, skip=skip, ...)
    as.matrix(dt)
}

setwd('~/Data/nf-raw/svdpp/')

d <- 50
ratings <- readRDS('ratings.Rds')
ratings[, user_idx := NULL]
valid.movies <- fread('valid_movies_2014.csv')
setnames(valid.movies, names(valid.movies), 'movie_id')
setkey(valid.movies, movie_id)

types <- factor(c('um', 'u0', '0m', '00'))

resdts <- mclapply(12:84, mc.cores=20, function(ipred) {
    cat(ipred, 'start\n')
    ptm <- proc.time()

    i <- ipred - 1
    subr <- ratings[rating_date == ipred]

    umap <- readRDS(paste0('mapping/user-',i,'.Rds'))
    mmap <- readRDS(paste0('mapping/movie-',i,'.Rds'))

    setkey(subr, user_id)
    setkey(umap, user_id)
    subr <- merge(subr, umap, all.x=TRUE)

    setkey(subr, movie_id)
    setkey(mmap, movie_id)
    subr <- merge(subr, mmap, all.x=TRUE)

    # load data
    mu <- readMM.dense(paste0('out/',d,'/',i,'/all-nf-',i,'.mm_global_mean.mm'))
    bi <- readMM.dense(paste0('out/',d,'/',i,'/all-nf-',i,'.mm_V_bias.mm'))
    qi <- readMM.dense(paste0('out/',d,'/',i,'/all-nf-',i,'.mm_V.mm'))
    bu <- readMM.dense(paste0('out/',d,'/',i,'/all-nf-',i,'.mm_U_bias.mm'))
    pwu <- readMM.dense(paste0('out/',d,'/',i,'/all-nf-',i,'.mm_U.mm'))

    # drop last column and reformat
    mu <- mu[1]
    qi <- qi[,1:d]
    pwu <- pwu[,1:d] + pwu[,1:d + d]

    subr[, predicted := as.numeric(NA)]
    idx.um <- which(subr[, !is.na(user_idx) & !is.na(movie_idx)])
    idx.u0 <- which(subr[, !is.na(user_idx) & is.na(movie_idx)])
    idx.0m <- which(subr[, is.na(user_idx) & !is.na(movie_idx)])
    idx.00 <- which(subr[, is.na(user_idx) & is.na(movie_idx)])

    # user and movie
    if(length(idx.um) > 0) {
        subr[idx.um, 
             `:=`(predicted = mu + bi[movie_idx] + bu[user_idx] + rowSums(pwu[user_idx,] * qi[movie_idx,]),
                  type=types[1])]
    }
    # user, no movie
    if(length(idx.u0) > 0) {
        subr[idx.u0, 
             `:=`(predicted = mu + bu[user_idx],
                  type=types[2])]
    }
    # movie, no user
    if(length(idx.0m) > 0) {
        subr[idx.0m, 
             `:=`(predicted = mu + bi[movie_idx],
                  type=types[3])]
    }
    # no user, no movie
    if(length(idx.00) > 0) {
        subr[idx.00, 
             `:=`(predicted = mu,
                  type=types[4])]
    }

    cat(ipred, 'done (', proc.time()[3] - ptm[3] ,')\n')

    subr[, list(movie_id, user_id, predicted, type)]
})

resdt <- rbindlist(resdts)
setkey(resdt, movie_id)

# save predictions
saveRDS(resdt, paste0('out/', d, '/predicted.Rds'), compress=FALSE)
saveRDS(resdt[valid.movies], paste0('out/', d, '/valid_predicted.Rds'), compress=FALSE)

# valid.predict <- as.matrix(resdt[valid.movies])
valid.predict <- resdt[valid.movies]
nvalid <- nrow(valid.predict)

chunk_size <- 1e5
chunks <- lapply(1:ceiling(nvalid / chunk_size), function(i) {
    offset <- (i-1) * chunk_size
    (1 + offset) : min(chunk_size + offset, nvalid)
})

ptm <- proc.time()
for (i in seq_along(chunks)) {
    chunk <- chunks[[i]]

    if(i == 1) {
        write.table(valid.predict[chunk,], paste0('out/', d, '/valid_predicted.csv'),
                    sep=',', row.names=FALSE)
    } else {
        write.table(valid.predict[chunk,], paste0('out/', d, '/valid_predicted.csv'),
                    sep=',', row.names=FALSE, col.names=FALSE, append=TRUE)
    }
    
    cat('\r', rep(' ', 50))
    done <- i / length(chunks)
    total <- (proc.time()[3] - ptm[3]) / done
    cat('\r', i, '/', length(chunks), '::', min(chunk), '-', max(chunk), '(', total, ')')
}
cat('\n')

# write.csv(as.matrix(resdt[valid.movies]), paste0('out/', d, '/valid_predicted.csv'))

# setwd(paste0('~/Data/nf-raw/svdpp/out/', d))
# 
# # load data
# mu <- readMM.dense('all-nf.mm_global_mean.mm')
# bi <- readMM.dense('all-nf.mm_V_bias.mm')
# qi <- readMM.dense('all-nf.mm_V.mm')
# bu <- readMM.dense('all-nf.mm_U_bias.mm')
# pwu <- readMM.dense('all-nf.mm_U.mm')
# 
# ratings <- readRDS('../../ratings.Rds')
# idxi <- ratings[, movie_id]
# idxu <- ratings[, user_idx]
# nratings <- nrow(ratings)
# # rm(ratings); gc()
# 
# # drop last column and reformat
# mu <- mu[1]
# qi <- qi[,1:d]
# pwu <- pwu[,1:d] + pwu[,1:d + d]
# 
# nu <- nrow(bu)
# ni <- nrow(bi)

# generate predictions
#system.time(ratings[, predicted := mu + bi[movie_id] + bu[user_idx] + rowSums(pwu[user_idx,] * qi[movie_id,])])

# nratings <- floor(nrow(ratings) / 10)

# chunk_size <- 1e4
# chunks <- lapply(1:ceiling(nratings / chunk_size), function(i) {
#     offset <- (i-1) * chunk_size
#     (1 + offset) : min(chunk_size + offset, nratings)
# })
# 
# cat('\n')
# # res <- local({
# f <- fifo(tempfile(), open='w+b', blocking=FALSE)
# res <- mclapply(c(list(NULL),chunks), mc.cores=10, function(chunk) {
#     if(is.null(chunk)) {
#         done <- 0
#         #test <- rep(TRUE, length(chunks))
#         while(done < nratings && !isIncomplete(f)) {
#             msg <- readBin(f, 'double')
# #             if(msg != chunk_size && msg != (nratings %% chunk_size)) msg <- 0
# #             chunkn <- ceiling(msg / chunk_size)
#             done <- done + msg
# #             done <- done + (if(msg %% chunk_size == 0) chunk_size else msg %% chunk_size)
#             cat(sprintf('\r%7.3f %%', 100 * done / nratings))
# #             cat(chunkn,done, (chunkn * chunk_size) / done,  '\n')
#             #test[chunkn] <- FALSE
#             #if(sum(test) < 50) print(which(test)) else print('>50')
#         }
#         NULL
#     } else {
#         res <- ratings[chunk, mu + bi[movie_id] + bu[user_idx] + rowSums(pwu[user_idx,] * qi[movie_id,])]
#         writeBin(as.numeric(length(chunk)), f)
# #         writeBin(as.numeric(max(chunk)), f)
# #         cat(sprintf('\r%7.3f %%', 100 * max(chunk) / nratings))
#         res
#     }
# })
# close(f)
# # res
# # })
# cat('\n')

# res <- do.call(c, res)
# ratings[, predicted := res]
# ratings[, user_idx := NULL]

# saveRDS(ratings, paste0('ratings',d,'.Rds'))
# write.csv(ratings, paste0('ratings',d,'.csv'))

# cat('\n')
# # test predictions
# res <- sapply(1:(nratings/1), function (i) {
# #     res <- mu + bi[i] + bu + crossprod(qi[i,], pwu)
#     res <- mu + bi[idxi[i]] + bu[idxu[i]] + (pwu[idxu[i],] %*% qi[idxi[i],])
# #     res <- mu + bi[i] + bu + (pwu %*% qi[i,])
# #     res <- c(min(res), mean(res), median(res), max(res))
#     #if(i %% 1000 == 0) print(c(i, res))
#     if(i %% 10000 == 0) cat(sprintf('\r%6.2f %%', 100 * i/nratings))
#     res
# })
# cat('\n')
# 
# # add in to ratings
# ratings[, predicted := res]



