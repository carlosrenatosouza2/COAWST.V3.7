reset

# ============================
# Parametros do experimento
exp = "l"; ncoresA = 768; ncoresO = 128; cnivel = 000; lab="somaRamnks-semIO"
#======================

set terminal png size 1000,800
set output sprintf("graftimestep_egeon_24h_exp_%s_co_n%d_%s_comWait.gnu.png", exp, cnivel, lab)

set multiplot layout 2,1 title sprintf("Tempos entre acoplamentos de 30min [24h-3/3h] - Exp %s - %s", exp,lab) font ",14"

#-------------------------------------------------
# Painel superior
set xlabel ''          # tira o label do x (vai no gráfico de baixo)
set ylabel 'Tempo (s)'
#set yrange [0:170]
set yrange [0:50000]
set key left top

# Tempo total(em segundos e em mm:ss)
tempo_total_s = real(system(sprintf("cat graftimestep_egeon_24h_exp_%s_TTOTAL.txt | head -1 | tail -1", exp)))
h = int(tempo_total_s / 3600)
m = int((tempo_total_s - h*3600)/60)
s = tempo_total_s - h*3600 - m*60
tempo_str = sprintf("%02dm", m)

# --- Tempo ciclos WRF
tempo_atm_s = real(system(sprintf("cat graftimestep_egeon_24h_exp_%s_TTOTAL.txt | head -6 | tail -1", exp)))
h = int(tempo_atm_s / 3600)
m = int((tempo_atm_s - h*3600)/60)
s = tempo_atm_s - h*3600 - m*60
tempo_ciclosatm = sprintf("%02dm:%02ds", m, s)

# --- Tempo ciclos ROMS
tempo_ocn_s = real(system(sprintf("cat graftimestep_egeon_24h_exp_%s_TTOTAL.txt | head -7 | tail -1", exp)))
h = int(tempo_ocn_s / 3600)
m = int((tempo_ocn_s - h*3600)/60)
s = tempo_ocn_s - h*3600 - m*60
tempo_ciclosocn = sprintf("%02dm:%02ds", m, s)

set label 1 sprintf("Tempo total: %s (%.2fs)", tempo_str, tempo_total_s)                 at graph 0.98,0.95 right
set label 2 sprintf("Tempo total ciclos WRF: %s (%.2fs)", tempo_ciclosatm, tempo_atm_s)  at graph 0.98,0.88 right
set label 3 sprintf("Tempo total ciclos ROMS: %s (%.2fs)", tempo_ciclosocn, tempo_ocn_s) at graph 0.98,0.81right




# Estilo para linhas verticais
set style line 10 lc rgb "gray" dt 2 lw 1
do for [x=1:50:6] {
    set arrow from x, graph 0 to x, graph 1 nohead ls 10
}

plot sprintf("graftimestep_egeon_24h_exp_%s_atm_mediacycles.txt", exp) every ::1 using 2:1 with lines lc rgb 'red'  title sprintf("WRF (%d)", ncoresA), \
     sprintf("graftimestep_egeon_24h_exp_%s_ocn_mediacycles.txt", exp) every ::1 using 2:1 with lines lc rgb 'blue' title sprintf("ROMS (%d)", ncoresO)

unset label 1
unset label 2
unset label 3
unset arrow

#-------------------------------------------------
# Painel inferior
set xlabel 'Intervalos entre acoplamentos'
set ylabel 'Tempo (s)'
#set yrange [0:35]      # ajusta para a ordem de grandeza das 4 linhas
set yrange [0:6000]  
set key left top

# Tempo total dos waits
tempo_total_s = real(system(sprintf("cat graftimestep_egeon_24h_exp_%s_TTOTAL.txt | head -2 | tail -1", exp)))
h = int(tempo_total_s / 3600)
m = int((tempo_total_s - h*3600)/60)
s = tempo_total_s - h*3600 - m*60
tempo_wsatm = sprintf("%02dm:%02ds", m, s)

tempo_total_s = real(system(sprintf("cat graftimestep_egeon_24h_exp_%s_TTOTAL.txt | head -3 | tail -1", exp)))
h = int(tempo_total_s / 3600)
m = int((tempo_total_s - h*3600)/60)
s = tempo_total_s - h*3600 - m*60
tempo_wratm = sprintf("%02dm:%02ds", m, s)

tempo_total_s = real(system(sprintf("cat graftimestep_egeon_24h_exp_%s_TTOTAL.txt | head -4 | tail -1", exp)))
h = int(tempo_total_s / 3600)
m = int((tempo_total_s - h*3600)/60)
s = tempo_total_s - h*3600 - m*60
tempo_wrocn = sprintf("%02dm:%02ds", m, s)

tempo_total_s = real(system(sprintf("cat graftimestep_egeon_24h_exp_%s_TTOTAL.txt | head -5 | tail -1", exp)))
h = int(tempo_total_s / 3600)
m = int((tempo_total_s - h*3600)/60)
s = tempo_total_s - h*3600 - m*60
tempo_wsocn = sprintf("%02dm:%02ds", m, s)



# mesmas linhas verticais
set style line 10 lc rgb "gray" dt 2 lw 1
do for [x=1:50:6] {
    set arrow from x, graph 0 to x, graph 1 nohead ls 10
}

plot sprintf("graftimestep_egeon_24h_exp_%s_atm_mediacycles_waits.txt", exp) every ::1 using 2:1 with lines lc rgb 'dark-red'  title  sprintf("%s : WaitS WRF", tempo_wsatm), \
     sprintf("graftimestep_egeon_24h_exp_%s_atm_mediacycles_waitr.txt", exp) every ::1 using 2:1 with lines lc rgb 'orange' title sprintf("%s : WaitR WRF", tempo_wratm), \
     sprintf("graftimestep_egeon_24h_exp_%s_ocn_mediacycles_waits.txt", exp) every ::1 using 2:1 with lines lc rgb 'blue' title sprintf("%s : WaitS ROMS", tempo_wsocn), \
     sprintf("graftimestep_egeon_24h_exp_%s_ocn_mediacycles_waitr.txt", exp) every ::1 using 2:1 with lines lc rgb 'skyblue'  title sprintf("%s : WaitR ROMS", tempo_wrocn)

unset multiplot
