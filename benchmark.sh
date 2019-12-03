#!/bin/bash

for x in 1..15
do
    racket benchmark.rkt | tee -a results.csv
done
