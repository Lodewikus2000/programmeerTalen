#!/bin/bash

NR_TO_UNPACK="1 2 3 4 5 6"
RUNS="1 2 3 4 5"

# TO DO
gnuplot -e "set terminal pdf; \
            set title 'average value'; \
            set boxwidth -2; \
            set xrange [0:7]; \
            plot 'hc-logs/hc-statistics.dat' using 1:3:4:xtic(2) with boxerrorbars;"  > ./hc-average-values.pdf

for param in $NR_TO_UNPACK
do
  gnuplot -e "set terminal pdf; \
              set title 'runs'; \
              plot  for [i = 1:5] 'hc-logs/param-$param-run-'.i.'-all-cleaned.dat'\
              with lines title 'parameter $param run '.i.'';"  > ./hc-unpack$param-runs.pdf
done
