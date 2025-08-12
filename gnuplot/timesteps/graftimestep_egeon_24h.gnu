reset

set title 'Tempos entre acoplamentos de 30min [COAWST-24h] EGEON'


set terminal png    
set output 'graftimestep_lines_EGEON_24h.gnu.png'


set ylabel 'Tempo (s)'
set xlabel 'Intervalos entre acoplamentos'
set yrange [70:100]

set key left top



plot  'graftimestep.gnu.atm.input_EGEON_24h' using 2:1 with line linecolor rgb 'red' title 'WRF (IO: 3h/3h)', \


