
reset

set title 'Tempos Wall Time COAWST v3.7'


set terminal png    
set output 'graf3.gnu.png'

set xlabel 'Wall clock (s)'
set ylabel 'Fases das Componentes'
set yrange [-10:700.0]
set xrange [0:350.0]

plot  'graf3.gnu.input1'  using 3:1    with line linecolor rgb 'black'          title 'WRF Run',       'graf3.gnu.input2'  using 3:1    with line linecolor rgb 'blue'           title 'OCN Run'

