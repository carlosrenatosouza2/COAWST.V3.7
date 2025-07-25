reset

set title 'Tempos entre acoplamentos de 30min [COAWST-24h]'

set terminal png    
set output 'graftimestep_bars.gnu.png'

set boxwidth 0.5
set style fill solid
set ylabel 'Tempo (s)'
set xlabel 'Intervalos entre acoplamentos'
set yrange [0:200]
#set key left top



plot "graftimestep.gnu.atm.input" using 1:6 with boxes title 'WRF TEA+Wait(s+r)', \
     "graftimestep.gnu.atm.input" using 1:2 with boxes title 'WRF Tempo Entre Acopl.'



