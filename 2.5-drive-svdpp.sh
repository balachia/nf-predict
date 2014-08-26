#for i in {11..80}
for i in {81..84}
do
    echo "Pass #$i"

    sh 2-run-svdpp.sh 50 $i

    rm -r ~/Data/nf-raw/svdpp/all-nf-$i.mm.*
    rm -r ~/Data/nf-raw/svdpp/all-nf-$i.mm_*
done
