#
# required packages:
# - gnuplot
# - datamash


# Files with each iteration and value of 2 runs with parameter set to 3:
# - param-3-run-1-all.dat
# - param-3-run-2-all.dat
#
# Make a plot with:
#
echo "make plot of run param-3-run-1-all.dat and param-3-run-2-all.dat"
echo "- param-3-runs.pdf"
gnuplot -e 'set terminal pdf; set title "runs"; plot "param-3-run-1-all.dat" with lines, "param-3-run-2-all.dat" with lines' > param-3-runs.pdf

# File with all the final values for each run with parameter set to 3:
# - final-3-.dat
#
# Get the mean value and standard deviation of the values in the file with:
#
echo "Mean value and standard deviation of data in final-3-.dat:"
cat final-3-.dat | datamash  mean 1 sstdev 1 

# File with the average value and standard deviation of each of 6 runs
# - average-value-data.dat
#
# 1 param:1 505254,4	1786,8100906364
# 2 param:2 506666,6	464,79113588794
# 3 param:3 505085,2	1351,0605833937
# 4 param:4 502186,4	1049,9894285182
# 5 param:5 499284,6	1097,7391766718
# 6 param:6 496052	2200,5879896064
# | |       |           | 
# | |       |           +- standard deviation
# | |       +- average value
# | +- label
# +- position on x axis
#
# Make a plot with:
#
echo "make plot of average-value-data.dat"
echo "- average-value-data.pdf"
gnuplot -e "set terminal pdf; \
            set title 'average value'; \
            set boxwidth -2; \
            set xrange [0:7]; \
            plot './average-value-data.dat' using 1:3:4:xtic(2) with boxerrorbars;"  > ./average-value-data.pdf
