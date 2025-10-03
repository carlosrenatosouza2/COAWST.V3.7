#!/bin/bash

DIRHOME=$(pwd)


nedit \
${DIRHOME}/linha-do-tempo.f90 \
${DIRHOME}/compf90.bash \
${DIRHOME}/linha-do-tempo.gnuplot \
${DIRHOME}/linha-do-tempo_gnuplot_input_acpl_atm.txt \
${DIRHOME}/linha-do-tempo_gnuplot_input_acpl_ocn.txt \
${DIRHOME}/kit.bash &
