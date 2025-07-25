
reset

set title 'Tempos Wall Time COAWST v3.7'


set terminal png    
set output 'tempos-coawst01.png'

#set logscale x 2
#set logscale y 2
set xlabel 'Wall clock (s)'
set ylabel 'Fases das Componentes'
set yrange [0:3.0]

set label 'ATM media:44s minmax:56s' at 270.0,0.4
set label 'OCN media:95s minmax:105s' at 270.0,0.7

set label 'ATM media:257s minmax:270s' at 314.0,0.9
set label 'OCN media:206s minmax:217s' at 365.0,1.2

set label 'ATM media:0,006s minmax:12s' at 470.0,1.4
set label 'OCN media:0,035s minmax:10s' at 470.0,1.7

plot  'tempos.txt'  using 2:1 with line linecolor rgb 'red'   title 'WRF Init',   \
      'temposO.txt' using 2:1 with line linecolor rgb 'blue'  title 'OCN Init',  \
      'tempos.txt'  using 4:3 with line linecolor rgb 'red'   title 'WRF Run',      \
      'temposO.txt' using 4:3 with line linecolor rgb 'blue'  title 'OCN Run',        \
      'tempos.txt'  using 6:5 with line linecolor rgb 'red'   title 'WRF Finalize', \
      'temposO.txt' using 6:5 with line linecolor rgb 'blue'  title 'OCN Finalize'

