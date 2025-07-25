'reinit'


'open /home/luciano.pezzi/COAWST.V3.7/Work/ATLSW12/wr_18022023/wrfout_d02_2023-02-18.ctl'

'set gxout shaded'
'set display color white'
'c'
'set grads off'
'set grid off'
'set mpdset brmap_hires'
*'set mproj scaled'
*'set annot 0 0'
*'set xlab off'
*'set ylab off'
*'set mpt * 1 1'
*'set map 1 1 5'
'set csmooth on'


tt=17
'set t 'tt
'color 5 350 20 -kind white->blue->darkblue->red'
'd (rainc-rainc(t-9)+rainnc-rainnc(t-9)))'
'cbarn'

'draw title Precipitacao prevista - 19/02/2023 - WRF(COAWST)'

'draw string 9.95 0.7 [mm/24h]'

'printim prec_wrf.png'

'
'quit'
