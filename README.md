# Steps

## 1-prep-svdpp.R

Load ratings from SQL, break out by month and save as R data.table.

## 1.5-split-dates-svdpp.R

Split out ratings by month, generating NIST Market Matrix files containing all rating up to and including that month.

In addition, change indexing (i.e. mapping all users in data up to that point to the integers 1:N users) for users and movies and saves these mappings,
to be used later for predictions.

## 2-run-svdpp.sh

Run the svdpp program, taking two inputs:

 - number of latent factors
 - month-year id

At the end, clean up and move output files to factors/month directory.

## 2.5-drive-svdpp.sh

Run svdpp for each month in the data, cleaning up processing files after each run.

## 3-predict.R

Run predictions for each rating, using all ratings up to the previous month.

To predict for out-of-sample users/movies, use only the global mean/user mean/movie mean, as available.

End with a ridiculous csv dump because R can't handle giant csvs.


