reset

set title 'Tempos entre acoplamentos de 30min [COAWST-24h]'

set terminal png    
set output 'graftimestep_points.gnu.png'


set ylabel 'Tempo (s)'
set xlabel 'Intervalos entre acoplamentos'
set yrange [0:200]

set key left top



plot  'graftimestep.gnu.atm.input' using 1:2 with points linecolor rgb 'red' pointtype 5 title 'WRF', \
      'graftimestep.gnu.ocn.input' using 1:2 with points linecolor rgb 'blue' pointtype 9 title 'OCN'




unset output





reset

set title 'Tempos entre acoplamentos de 30min [COAWST-24h]'

set terminal png    
set output 'graftimestep_lines.gnu.png'


set ylabel 'Tempo (s)'
set xlabel 'Intervalos entre acoplamentos'
set yrange [0:200]

set key left top



plot  'graftimestep.gnu.atm.input' using 1:2 with line linecolor rgb 'red' title 'WRF', \
      'graftimestep.gnu.ocn.input' using 1:2 with line linecolor rgb 'blue' title 'OCN'

