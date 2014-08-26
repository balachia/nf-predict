DATA=~/Data/nf-raw/svdpp
GCHI=~/.local/src/graphchi-cpp
SOLVER=$GCHI/toolkits/collaborative_filtering
FILE=all-nf.mm
FILE=all-nf-$2.mm
#FILE=smallnetflix_mm
export GRAPHCHI_ROOT=$GCHI/

cd $DATA
mkdir -p out/$1/$2

#$SOLVER/svdpp --training=smallnetflix_mm --biassgd_lambda=1e-4 --biassgd_gamma=1e-4 --minval=1 --maxval=5 --max_iter=6 --quiet=1
$SOLVER/svdpp --training=$FILE --biassgd_lambda=1e-4 --biassgd_gamma=1e-4 --minval=1 --maxval=5 --D=$1 --max_iter=50 --quiet=1 --halt_on_rmse_increase=1 > out/$1/$2/log$1-$2.out
#$SOLVER/svdpp --training=smallnetflix_mm --biassgd_lambda=1e-4 --biassgd_gamma=1e-4 --minval=1 --maxval=5 --D=$1 --max_iter=40 --quiet=0 --halt_on_rmse_increase=5 > log$1.out

mv "$FILE"_global_mean.mm out/$1/$2/
mv "$FILE"_U.mm out/$1/$2/
mv "$FILE"_V.mm out/$1/$2/
mv "$FILE"_U_bias.mm out/$1/$2/
mv "$FILE"_V_bias.mm out/$1/$2/

