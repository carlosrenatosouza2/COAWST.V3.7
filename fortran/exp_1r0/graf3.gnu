 reset
 set title 'Tempos Wall Time COAWST v3.7'
 set terminal png
 set output 'graf3.gnu.png'
 set xlabel 'Wall clock (s)'
 set ylabel 'Cores'
 set yrange [-10:1000]
 set xrange [0:300]
 set key left top
 plot 'graf3.gnu.input1' using  2:1 with line linecolor rgb 'black' title 'WRF Run', \
      'graf3.gnu.input1' using  3:1 with line linecolor rgb 'blue      ' title 'ATM_ISend', \
      'graf3.gnu.input1' using  4:1 with line linecolor rgb 'red       ' title 'ATM_Waits', \
      'graf3.gnu.input1' using  5:1 with line linecolor rgb 'green     ' title 'ATM_IRecv', \
      'graf3.gnu.input1' using  6:1 with line linecolor rgb 'red       ' title 'ATM_Waitr', \
      'graf3.gnu.input1' using  7:1 with line linecolor rgb 'blue      ' title 'ATM_ISend', \
      'graf3.gnu.input1' using  8:1 with line linecolor rgb 'red       ' title 'ATM_Waits', \
      'graf3.gnu.input1' using  9:1 with line linecolor rgb 'green     ' title 'ATM_IRecv', \
      'graf3.gnu.input1' using 10:1 with line linecolor rgb 'red       ' title 'ATM_Waitr', \
      'graf3.gnu.input1' using 11:1 with line linecolor rgb 'blue      ' title 'ATM_ISend', \
      'graf3.gnu.input1' using 12:1 with line linecolor rgb 'red       ' title 'ATM_Waits', \
      'graf3.gnu.input1' using 13:1 with line linecolor rgb 'green     ' title 'ATM_IRecv', \
      'graf3.gnu.input1' using 14:1 with line linecolor rgb 'red       ' title 'ATM_Waitr', \
      'graf3.gnu.input1' using 15:1 with line linecolor rgb 'black' title 'WRF Run'
