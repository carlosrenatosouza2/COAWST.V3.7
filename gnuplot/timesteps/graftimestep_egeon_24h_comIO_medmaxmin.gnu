reset
set title 'Tempos entre acoplamentos de 30min [COAWST-24h] EGEON'

set terminal png
set output 'graftimestep_lines_EGEON_24h_comIO_medminmax.png'

set ylabel 'Tempo (s)'
set xlabel 'Intervalos entre acoplamentos'
set yrange [0:130]
set key left top

# Estilo para as linhas verticais pontilhadas
set style line 10 lc rgb "gray" dt 2 lw 1
do for [x=1:49:6] {
    set arrow from x, graph 0 to x, graph 1 nohead ls 10
}

# Plota faixa de minimo/maximo preenchida + linha da media
plot  'media_ranks/med.min.max.graftimestep.OCN+ATM.input_EGEON_24h_comIO' using 1:4:3 with filledcurves fs transparent solid 0.2 lc rgb "red" title 'Min-Max WRF (com IO)', \
      '' using 1:2 with lines lw 2 lc rgb 'red' title 'Media WRF (com IO)'
