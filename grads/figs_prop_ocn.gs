'reinit'


'open /home/luciano.pezzi/COAWST.V3.7/Work/ATLSW12/wr_06022022/z.ctl'

'set gxout shaded'
'set display color white'
'c'
'set grads off'
'set grid off'
'set mpdset brmap_mres'
*'set mproj scaled'
*'set annot 0 0'
*'set xlab off'
*'set ylab off'
*'set mpt * 1 1'
*'set map 1 1 5'
'set csmooth on'


'set t 9'
'set z 1'
'color 0 30 3 -kind blue->white->red'
'd temp'
'cbarn 1 4.44833 8.81375 '

'draw title Temperatura da Superficie do Mar - ROMS(COAWST)'

'draw string 8.65 1.0 [oC]'

'printim prec_ocn.png'



'
'quit'
