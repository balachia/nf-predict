library(data.table)
library(Matrix)

system.time(ratings <- readRDS('ratings.Rds'))

max.date <- ratings[, max(rating_date)]

for (mdate in 1:max.date) {
    system.time(ratingsMM <- sparseMatrix(i=ratings[rating_date <= mdate, user_idx],
                                          j=ratings[rating_date <= mdate, movie_id],
                                          x=ratings[rating_date <= mdate, rating]))
    rm(ratings)
    gc()

    system.time(writeMM(ratingsMM, paste0('all-nf-',mdate,'.mm')))
    rm(ratingsMM)
    gc()
}
