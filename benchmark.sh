#!/bin/bash

echo -n "	" > results.csv
grep "  ;;; " benchmark.rkt | cut -f4- -d\  | strings -s "	" >> results.csv
echo >> results.csv

for M in 2 4 8 16
do
    for N in 2 4 8 16
    do
        echo -n "M=$M N=$N" | tee -a results.csv
        for i in {1..5}
        do
            racket benchmark.rkt $M $N | tee -a results.csv
        done
    done
done
