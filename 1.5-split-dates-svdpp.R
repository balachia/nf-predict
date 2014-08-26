library(data.table)
library(Matrix)

setwd('~/Data/nf-raw/svdpp')
rm(list=ls())
gc()

system.time(ratings <- readRDS('ratings.Rds'))

max.date <- ratings[, max(rating_date)]
min.date <- ratings[, min(rating_date)]

for (mdate in min.date:max.date) {
    print(mdate)
    system.time(subr <- ratings[rating_date <= mdate, list(user_id, movie_id, rating)])

    subr[, movie_idx := .GRP, by=movie_id]
    subr[, user_idx := .GRP, by=user_id]

    saveRDS(subr[,list(movie_idx=min(movie_idx)), by=movie_id],
            paste0('mapping/movie-',mdate,'.Rds'))
    saveRDS(subr[,list(user_idx=min(user_idx)), by=user_id],
            paste0('mapping/user-',mdate,'.Rds'))

    system.time(ratingsMM <- sparseMatrix(i=subr[, user_idx],
                                          j=subr[, movie_idx],
                                          x=subr[, rating]))
#     rm(ratings)
    gc()

    system.time(writeMM(ratingsMM, paste0('all-nf-',mdate,'.mm')))
    rm(ratingsMM)
    gc()
}
