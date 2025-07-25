
reset

set title 'Tempos de execucao timestep'


set terminal png    
set output 'graf2.1.gnu.png'

#set logscale x 2
#set logscale y 2
set xlabel 'Ciclo timestep '
set ylabel 'tempo (s)'
set yrange [0:35.0]
set xrange [0:370.0]


#set key left top

plot  'outputATM_mediazoom.txt' using 1:2 with line linecolor rgb 'red' title 'WRF'

