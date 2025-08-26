reset

set title 'Tempos entre acoplamentos de 30min [24h-3/3h]'


set terminal png    
set output 'graftimestep_egeon_24h_exp_d.gnu.png'


set ylabel 'Tempo (s)'
set xlabel 'Intervalos entre acoplamentos'
set yrange [0:170]

set key left top

# Le o tempo total do arquivo fort.1000
tempo_total_s = real(system("awk '{print $4}' tempototal_exp_d.txt"))

# Converte para horas, minutos e segundos
h = int(tempo_total_s / 3600)
m = int((tempo_total_s - h*3600)/60)
s = tempo_total_s - h*3600 - m*60

# Cria string formatada hh:mm:ss
tempo_str = sprintf("%02dh%02dm", h, m)
#tempo_str = sprintf("%02dh%02dm", h, m)
#tempo_str = sprintf("%02dm", m)

# Coloca o tempo no grafico (canto superior direito)
set label 1 sprintf("Tempo total: %s", tempo_str) at graph 0.98,0.95 right

# Estilo para as linhas verticais pontilhadas
set style line 10 lc rgb "gray" dt 2 lw 1

# Adiciona linhas verticais nos pontos 1, 7, 13, ...
do for [x=1:50:6] {
    set arrow from x, graph 0 to x, graph 1 nohead ls 10
}


plot  'graftimestep_egeon_24h_exp_d.gnu_input_atm.txt' every ::1 using 2:1 with lines linecolor rgb 'red' title 'WRF (64c)', \
      'graftimestep_egeon_24h_exp_d.gnu_input_ocn.txt' every ::1 using 2:1 with lines linecolor rgb 'blue' title 'ROMS (64c)'
