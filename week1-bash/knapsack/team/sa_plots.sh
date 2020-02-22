#!/bin/bash

MAX_TEMPERATURE="0 1 10 100 1000 10000"

gnuplot -e "set terminal pdf; \
            set title 'average value'; \
            set boxwidth -2; \
            set xrange [0:7]; \
            plot 'sa-logs/sa-statistics.dat' using 1:3:4:xtic(2) with boxerrorbars;"  > ./sa-average-values.pdf

for param in $MAX_TEMPERATURE
do
  gnuplot -e "set terminal pdf; \
              set title 'runs'; \
              plot  for [i = 1:5] 'sa-logs/param-$param-run-'.i.'-all-cleaned.dat'\
              with lines title 'temp $param run '.i.'';"  > ./sa-temp$param-runs.pdf
done
