library(data.table)
library(Rcpp)

setwd('~/Data/nf-raw/svdpp')

rm(list=ls())

d <- 50

cppFunction('
NumericVector welford_variance_cpp(NumericVector xs) {
    int n = xs.size();
    NumericVector out(n);

    double mean = 0;
    double M2 = 0;

    for(int i = 0; i < n; i++) {
        double delta = xs[i] - mean;
        mean += delta / (i + 1);
        M2 += delta * (xs[i] - mean);
        out[i] = M2 / i;
    }

    return out;
}
')

welford.variance <- function(x) {
    means <- c(0, cumsum(x) / 1:length(x))
    M2n <- Reduce(function(a,b) {
            a + (x[b] - means[b])*(x[b] - means[b+1])
        }, c(0,1:length(x)), accumulate=TRUE)
    M2n <- M2n[-1]
    M2n / (1:length(x) - 1)
}

ratings <- readRDS('ratings.Rds')
predicted <- readRDS(paste0('out/',d,'/predicted.Rds'))

setkey(ratings, movie_id, user_id)
setkey(predicted, movie_id, user_id)

ratings[, user_idx := NULL]

dt <- merge(ratings, predicted, all=TRUE)

# add residual
dt[, predicted.in.range := pmin(5,pmax(1,predicted))]
dt[, residual := predicted.in.range - rating]

# get effective number of prior ratings
setkey(dt, user_id, rating_date)
dt[, user_rating_count := 1:.N - 1, by=user_id]
dt[, user_rating_count := min(user_rating_count), by=list(user_id, rating_date)]

setkey(dt, movie_id, rating_date)
dt[, movie_rating_count := 1:.N - 1, by=movie_id]
dt[, movie_rating_count := min(movie_rating_count), by=list(movie_id, rating_date)]

# things needed to characterize the variance
dt[, md_block_start := min(.I), by=list(movie_id, rating_date)]
dt[, md_block_end := max(.I), by=list(movie_id, rating_date)]
dt[, md_previous_block := md_block_start - 1]
dt[md_previous_block==0, md_previous_block := NA]
dt[, same_movie_previous_block := movie_id[md_previous_block] == movie_id]
dt[is.na(same_movie_previous_block), same_movie_previous_block := FALSE]

dt[, resid_var := welford_variance_cpp(residual), by=movie_id]
dt[, last_resid_var := resid_var[md_previous_block]]
dt[(!same_movie_previous_block), last_resid_var := Inf]

# dt[, resid_sum := cumsum(residual), by=movie_id]
# dt[, nratings := 1:.N, by=movie_id]
# dt[, resid_mean := ((nratings - 1) * ) / n]
# dt[, resid_mean := resid_sum[md_block_end] / nratings[md_block_end]]
# dt[, resid_resid_sq := ]
# 
# dt[, resid_sum := resid_sum[md_previous_block]]
# dt[(!same_movie_previous_block), resid_sum := 0]
# dt[, nratings := 1:.N, by=movie_id]
# dt[, nratings := nratings[md_previous_block]]
# dt[(!same_movie_previous_block), nratings := 0]
# dt[, resid_mean := resid_sum / nratings]

# collapse ratings by movie
ptm <- proc.time()
res <- dt[, list(rating_mean = mean(rating, na.rm=TRUE),
                 resid_mean = mean(residual, na.rm=TRUE),
#                  resid_sd = sd(residual, na.rm=TRUE),
                 resid_var = var(residual, na.rm=TRUE),
                 last_resid_var = last_resid_var[1],
#                  resid_sd_um = sd(residual[type=='um'], na.rm=TRUE),
#                  nratings_um = sum(type=='um', na.rm=TRUE),
                 nratings = .N), by=list(movie_id, rating_date)]
proc.time() - ptm
res

# chi square p value
res[, chi_score := (nratings - 1) * resid_var / last_resid_var]
res[, chi_lpval := pchisq(q=chi_score, df=nratings - 1)]
res[, chi_rpval := pchisq(q=chi_score, df=nratings - 1, lower.tail=FALSE)]
res[, chi_pval := 2 * pmin(chi_lpval, chi_rpval)]
res[, lchi_pval := log(chi_pval)]

saveRDS(res, paste0('out/',d,'/movie_residuals.Rds'))

# txtplot(res[!is.na(lchi_pval) & is.finite(lchi_pval), rating_date],
#         res[!is.na(lchi_pval) & is.finite(lchi_pval), lchi_pval])

# all.movie.times <- CJ(movie_id=res[, unique(movie_id)], rating_date=min(res$rating_date):max(res$rating_date))
# res[all.movie.times, roll=TRUE]

# res[, resid_gap := resid_sd - c(NA, resid_sd[1:(.N-1)]), by=movie_id]

