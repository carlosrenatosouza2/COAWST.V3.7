reset

# ============================
# Parametros do experimento
#
#exp     = "e"; ncoresA = 256; ncoresO = 128
#exp     = "h"; ncoresA = 384; ncoresO = 128
#exp     = "i"; ncoresA = 384; ncoresO = 256
#exp     = "j"; ncoresA = 512; ncoresO = 128
#exp     = "k"; ncoresA = 512; ncoresO = 256
exp     = "l"; ncoresA = 768; ncoresO = 128; cnivel = 000; lab="semIO"
#exp     = "m"; ncoresA = 768; ncoresO = 256
#exp     = "n"; ncoresA = 1024; ncoresO = 128
#exp     = "o"; ncoresA = 1024; ncoresO = 256

#======================

set title sprintf("Tempos entre acoplamentos de 30min [24h-3/3h] - Exp %s \n escrita comentada nivel %d", exp,cnivel)

set terminal png
set output sprintf("graftimestep_egeon_24h_exp_%s_co_n%d_%s.gnu.png", exp, cnivel, lab)

set ylabel 'Tempo (s)'
set xlabel 'Intervalos entre acoplamentos'
set yrange [0:170]
set key left top

# Le o tempo total do arquivo correspondente ao experimento
tempo_total_s = real(system(sprintf("cat graftimestep_egeon_24h_exp_%s_TTOTAL.txt", exp)))

# Converte para horas, minutos e segundos
h = int(tempo_total_s / 3600)
m = int((tempo_total_s - h*3600)/60)
s = tempo_total_s - h*3600 - m*60

# Cria string formatada hh:mm
tempo_str = sprintf("%02dm", m)

# Coloca o tempo no gráfico (canto superior direito)
set label 1 sprintf("Tempo total: %s", tempo_str) at graph 0.98,0.95 right

# Estilo para as linhas verticais pontilhadas
set style line 10 lc rgb "gray" dt 2 lw 1

# Adiciona linhas verticais nos pontos 1, 7, 13, ...
do for [x=1:50:6] {
    set arrow from x, graph 0 to x, graph 1 nohead ls 10
}

plot sprintf("graftimestep_egeon_24h_exp_%s_atm_mediacycles.txt", exp) every ::1 using 2:1 with lines linecolor rgb 'red'  title sprintf("WRF (%d)", ncoresA), \
     sprintf("graftimestep_egeon_24h_exp_%s_ocn_mediacycles.txt", exp) every ::1 using 2:1 with lines linecolor rgb 'blue' title sprintf("ROMS (%d)", ncoresO)
