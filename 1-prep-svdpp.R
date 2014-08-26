library(RMySQL)
library(Matrix)
library(data.table)

rm(list=ls())

setwd('~/Data/nf-raw/svdpp')

con <- dbConnect(MySQL(), user='netflix-read', dbname='netflix')

#ratings <- dbSendQuery(con, 'SELECT user_id, movie_id, rating FROM ratings')
#ratings <- fetch(ratings, -1)

# takes a bit of time -- up to 8 mins?
system.time(ratings <- dbGetQuery(con, 'SELECT user_id, movie_id, rating, rating_date FROM ratings'))
system.time(ratings <- as.data.table(ratings))
gc()

# generate user index
system.time(ratings[, user_idx := .GRP, by=user_id])
gc()

system.time(saveRDS(ratings, 'ratings.Rds'))
# write out user id mapping
system.time(write.csv(ratings[, min(user_id), by=user_idx], 'user-mapping.csv'))

system.time(ratingsMM <- sparseMatrix(i=ratings$user_idx, j=ratings$movie_id, x=ratings$rating))
rm(ratings)
gc()

system.time(writeMM(ratingsMM, 'all-nf.mm'))
rm(ratingsMM)
gc()

