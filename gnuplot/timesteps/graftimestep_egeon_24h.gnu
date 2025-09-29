reset

set title 'Tempos entre acoplamentos de 30min [COAWST-24h] EGEON'


set terminal png    
set output 'graftimestep_lines_EGEON_24h2.gnu.png'


set ylabel 'Tempo (s)'
set xlabel 'Intervalos entre acoplamentos'
set yrange [0:130]

set key left top

# Estilo para as linhas verticais pontilhadas
set style line 10 lc rgb "gray" dt 2 lw 1

# Adiciona linhas verticais nos pontos 1, 7, 13, ...
do for [x=1:49:6] {
    set arrow from x, graph 0 to x, graph 1 nohead ls 10
}


plot  'graftimestep.gnu.ATM.input_EGEON_24h' using 1:2 with lines linecolor rgb 'red' title 'WRF (IO: 3/3h)', \
      'graftimestep.gnu.OCN.input_EGEON_24h' using 2:1 with lines linecolor rgb 'blue' title 'ROMS (IO: 3/3h)'
