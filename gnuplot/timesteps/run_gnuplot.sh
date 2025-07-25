#!/bin/bash 

# intervalos de tempo entre acoplamentos:
#utlimo graf q entrar na proposta:
#export rootname="graftimestep"
#rm -f ${rootname}_lines.gnu.png ${rootname}_points.gnu.png
#gnuplot ${rootname}.gnu
#display ${rootname}_lines.gnu.png ${rootname}_points.gnu.png


export rootname="graftimestep_bars"
rm -f ${rootname}.gnu.png
gnuplot ${rootname}.gnu
display ${rootname}.gnu.png
