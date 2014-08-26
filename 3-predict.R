library(Matrix)
library(data.table)
library(parallel)

rm(list=ls())
options(max.print=5000)

readMM.dense <- function(fin, skip=3, ...) {
    dt <- fread(fin, skip=skip, ...)
    as.matrix(dt)
}

d <- 100

setwd(paste0('~/Data/nf-raw/svdpp/out/', d))

# load data
mu <- readMM.dense('all-nf.mm_global_mean.mm')
bi <- readMM.dense('all-nf.mm_V_bias.mm')
qi <- readMM.dense('all-nf.mm_V.mm')
bu <- readMM.dense('all-nf.mm_U_bias.mm')
pwu <- readMM.dense('all-nf.mm_U.mm')

ratings <- readRDS('../../ratings.Rds')
idxi <- ratings[, movie_id]
idxu <- ratings[, user_idx]
nratings <- nrow(ratings)
# rm(ratings); gc()

# drop last column and reformat
mu <- mu[1]
qi <- qi[,1:d]
pwu <- pwu[,1:d] + pwu[,1:d + d]

nu <- nrow(bu)
ni <- nrow(bi)

# generate predictions
#system.time(ratings[, predicted := mu + bi[movie_id] + bu[user_idx] + rowSums(pwu[user_idx,] * qi[movie_id,])])

# nratings <- floor(nrow(ratings) / 10)

chunk_size <- 1e4
chunks <- lapply(1:ceiling(nratings / chunk_size), function(i) {
    offset <- (i-1) * chunk_size
    (1 + offset) : min(chunk_size + offset, nratings)
})

cat('\n')
# res <- local({
f <- fifo(tempfile(), open='w+b', blocking=FALSE)
res <- mclapply(c(list(NULL),chunks), mc.cores=10, function(chunk) {
    if(is.null(chunk)) {
        done <- 0
        #test <- rep(TRUE, length(chunks))
        while(done < nratings && !isIncomplete(f)) {
            msg <- readBin(f, 'double')
#             if(msg != chunk_size && msg != (nratings %% chunk_size)) msg <- 0
#             chunkn <- ceiling(msg / chunk_size)
            done <- done + msg
#             done <- done + (if(msg %% chunk_size == 0) chunk_size else msg %% chunk_size)
            cat(sprintf('\r%7.3f %%', 100 * done / nratings))
#             cat(chunkn,done, (chunkn * chunk_size) / done,  '\n')
            #test[chunkn] <- FALSE
            #if(sum(test) < 50) print(which(test)) else print('>50')
        }
        NULL
    } else {
        res <- ratings[chunk, mu + bi[movie_id] + bu[user_idx] + rowSums(pwu[user_idx,] * qi[movie_id,])]
        writeBin(as.numeric(length(chunk)), f)
#         writeBin(as.numeric(max(chunk)), f)
#         cat(sprintf('\r%7.3f %%', 100 * max(chunk) / nratings))
        res
    }
})
close(f)
# res
# })
cat('\n')

res <- do.call(c, res)
ratings[, predicted := res]
ratings[, user_idx := NULL]

saveRDS(ratings, paste0('ratings',d,'.Rds'))
write.csv(ratings, paste0('ratings',d,'.csv'))

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



