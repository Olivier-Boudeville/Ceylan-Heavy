set autoscale
unset log                              
unset label                            
set xtic auto						   
set ytic auto		
set grid				   
set title "Computing actual scheduling granularity"
set xlabel "Requested duration (microsecond)"
set ylabel "Measured duration (microsecond)"
set terminal png
set output "schedulingGranularity.png"
plot "granularity.dat" using 1:2 with linespoints
