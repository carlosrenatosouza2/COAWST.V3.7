reset

set title 'Tempos entre acoplamentos de 30min [COAWST-24h] EGEON'


set terminal png    
set output 'graftimestep_lines_EGEON.gnu.png'


set ylabel 'Tempo (s)'
set xlabel 'Intervalos entre acoplamentos'
set yrange [50:110]

set key left top



plot  'graftimestep.gnu.atm.input_EGEON' using 1:2 with line linecolor rgb 'red' title 'WRF (IO 1h/1h', \


