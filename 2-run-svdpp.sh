#cd ~/Data/nf-raw/svdpp

DATA=~/Data/nf-raw/svdpp
SOLVER=~/src/graphchi-cpp/toolkits/collaborative_filtering
GCHI=~/src/graphchi-cpp/
FILE=all-nf.mm
#FILE=smallnetflix_mm
export GRAPHCHI_ROOT=$GCHI

cd $DATA
#$SOLVER/svdpp --training=smallnetflix_mm --biassgd_lambda=1e-4 --biassgd_gamma=1e-4 --minval=1 --maxval=5 --max_iter=6 --quiet=1
$SOLVER/svdpp --training=$FILE --biassgd_lambda=1e-4 --biassgd_gamma=1e-4 --minval=1 --maxval=5 --D=$1 --max_iter=40 --quiet=0 --halt_on_rmse_increase=5 > log$1.out
#$SOLVER/svdpp --training=smallnetflix_mm --biassgd_lambda=1e-4 --biassgd_gamma=1e-4 --minval=1 --maxval=5 --D=$1 --max_iter=40 --quiet=0 --halt_on_rmse_increase=5 > log$1.out

mkdir -1 out/$1

mv all-nf.mm_global_mean.mm out/$1/
mv all-nf.mm_U.mm out/$1/
mv all-nf.mm_V.mm out/$1/
mv all-nf.mm_U_bias.mm out/$1/
mv all-nf.mm_V_bias.mm out/$1/

