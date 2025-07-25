
reset

set title 'Tempos Wall Time COAWST v3.7'


set terminal png    
set output 'graf2.gnu.png'

#set logscale x 2
#set logscale y 2
set xlabel 'Wall clock (s)'
set ylabel 'Fases das Componentes'
set yrange [0:3.0]

#set label 'ATM media:44s minmax:56s' at 270.0,0.4
#set label 'OCN media:95s minmax:105s' at 270.0,0.7

#set label 'ATM media:257s minmax:270s' at 314.0,0.9
#set label 'OCN media:206s minmax:217s' at 365.0,1.2

#set label 'ATM media:0,006s minmax:12s' at 470.0,1.4
#set label 'OCN media:0,035s minmax:10s' at 470.0,1.7

set key left top

plot  'graf2.gnu.input1'  using 2:1    with line linecolor rgb 'red'          title 'WRF Init', \
      'graf2.gnu.input1'  using 4:3    with line linecolor rgb 'red'          title 'WRF Run', \
      'graf2.gnu.input1'  using 6:5    with line linecolor rgb 'red'          title 'WRF Finalize', \
      'graf2.gnu.input1'  using 8:7    with line linecolor rgb 'dark-violet'  title 'WRF MCT_ISend1', \
      'graf2.gnu.input1'  using 12:11  with line linecolor rgb 'dark-violet'  title 'WRF MCT_ISend2', \
      'graf2.gnu.input1'  using 16:15  with line linecolor rgb 'dark-violet'  title 'WRF MCT_ISend3', \
      'graf2.gnu.input1'  using 10:9   with line linecolor rgb 'magenta'      title 'WRF MCT_Waits1', \
      'graf2.gnu.input1'  using 14:13  with line linecolor rgb 'magenta'      title 'WRF MCT_Waits2', \
      'graf2.gnu.input1'  using 18:17  with line linecolor rgb 'magenta'      title 'WRF MCT_Waits2', \
      'graf2.gnu.input2'  using 2:1    with line linecolor rgb 'blue'         title 'OCN Init', \
      'graf2.gnu.input2'  using 4:3    with line linecolor rgb 'blue'         title 'OCN Run', \
      'graf2.gnu.input2'  using 6:5    with line linecolor rgb 'blue'         title 'OCN Finalize', \
      'graf2.gnu.input2'  using 8:7    with line linecolor rgb 'green'        title 'OCN MCT_ISend1', \
      'graf2.gnu.input2'  using 12:11  with line linecolor rgb 'green'        title 'OCN MCT_ISend2', \
      'graf2.gnu.input2'  using 16:15  with line linecolor rgb 'green'        title 'OCN MCT_ISend3', \
      'graf2.gnu.input2'  using 10:9   with line linecolor rgb 'black'        title 'OCN MCT_Waits1', \
      'graf2.gnu.input2'  using 14:13  with line linecolor rgb 'black'        title 'OCN MCT_Waits2', \
      'graf2.gnu.input2'  using 18:17  with line linecolor rgb 'black'        title 'OCN MCT_Waits2'

