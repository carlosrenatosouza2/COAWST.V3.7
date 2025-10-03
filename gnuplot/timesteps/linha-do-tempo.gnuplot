# Terminal de saída (imagem PNG)
set terminal pngcairo size 1000,400 enhanced font 'Arial,12'
set output 'linha-do-tempo.png'

# Rótulos dos eixos
set xlabel "Tempo (s)"
set ylabel "Eventos"

# Coloca rótulos no eixo Y para cada série
set ytics ("acpl ATM" 1, "acpl OCN" 1.1, "waitS ATM" 1.2, "waitR ATM" 1.3, "waitR OCN" 1.4, "waitS OCN" 1.5)

# Remove grid vertical do Y (opcional)
set grid xtics

# Range automático do X (vai de 0 até o maior tempo encontrado)
set xrange [0:*]
set yrange [0.8:3.0]

# Plot
plot \
    'linha-do-tempo_gnuplot_input_acpl_atm.txt' using 1:(1)    with points pt 14 ps 0.7 lc rgb "red" title "antes cpl ATM", \
    'linha-do-tempo_gnuplot_input_acpl_atm.txt' using 2:(1)    with points pt  7 ps 0.7 lc rgb "red"  title "depois cpl ATM", \
    'linha-do-tempo_gnuplot_input_acpl_ocn.txt' using 1:(1.1)  with points pt 14 ps 0.7 lc rgb "blue" title "antes cpl OCN", \
    'linha-do-tempo_gnuplot_input_acpl_ocn.txt' using 2:(1.1)  with points pt  7 ps 0.7 lc rgb "blue"  title "depois cpl OCN", \
    'linha-do-tempo_gnuplot_input_ws_atm.txt'   using 1:(1.2)  with points pt 19 ps 0.7 lc rgb "red" title "antes waitS ATM", \
    'linha-do-tempo_gnuplot_input_ws_atm.txt'   using 1:(1.2)  with points pt 20 ps 0.7 lc rgb "red"  title "depois waitS ATM", \
    'linha-do-tempo_gnuplot_input_wr_atm.txt'   using 1:(1.3)  with points pt  8 ps 0.7 lc rgb "red" title "antes waitR ATM", \
    'linha-do-tempo_gnuplot_input_wr_atm.txt'   using 1:(1.3)  with points pt  9 ps 0.7 lc rgb "red"  title "depois waitR ATM", \
    'linha-do-tempo_gnuplot_input_wr_ocn.txt'   using 1:(1.4)  with points pt  8 ps 0.7 lc rgb "blue" title "antes waitR OCN", \
    'linha-do-tempo_gnuplot_input_wr_ocn.txt'   using 1:(1.4)  with points pt  9 ps 0.7 lc rgb "blue"  title "depois waitR OCN", \
    'linha-do-tempo_gnuplot_input_ws_ocn.txt'   using 1:(1.5)  with points pt 19 ps 0.7 lc rgb "blue" title "antes waitS OCN", \
    'linha-do-tempo_gnuplot_input_ws_ocn.txt'   using 1:(1.5)  with points pt 20 ps 0.7 lc rgb "blue"  title "depois waitS OCN", \
    
    
# Fecha saída
unset output
