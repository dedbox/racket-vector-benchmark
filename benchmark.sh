#!/bin/bash

for N in 2 4 8 16
do
    for K in 2 4 8 16
    do
        echo "N=$N K=$N" | tee -a results.csv
        for i in {1..5}
        do
            racket benchmark.rkt $N $K | tee -a results.csv
        done
    done
done
