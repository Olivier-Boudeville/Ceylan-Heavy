set terminal png size 1000,600
set autoscale
unset log                              
unset label                            
set xtic auto						   
set ytic auto		
set grid				   
set title "Computing actual testCeylanTime scheduling granularity"
set xlabel "Requested duration (microsecond)"
set ylabel "Measured duration (microsecond)"
set terminal png
set output "testCeylanTime-schedulingGranularity.png"
plot "testCeylanTime-granularity.dat" using 1:2 with linespoints
