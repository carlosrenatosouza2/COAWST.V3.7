#!/bin/bash

DIRHOME=$(pwd)


nedit \
${DIRHOME}/graftimestep_egeon_24h_soRank0_comWait_somaRanks.f90 \
${DIRHOME}/compf90.bash \
${DIRHOME}/graftimestep_egeon_24h_todos_exp_comWait.gnu \
${DIRHOME}/graftimestep_egeon_24h_exp_l_atm_mediacycles.txt \
${DIRHOME}/graftimestep_egeon_24h_exp_l_ocn_mediacycles.txt \
${DIRHOME}/graftimestep_egeon_24h_exp_l_0_atm.txt \
${DIRHOME}/graftimestep_egeon_24h_exp_l_0_ocn.txt \
${DIRHOME}/graftimestep_egeon_24h_exp_l_atm_mediacycles_waits.txt \
${DIRHOME}/graftimestep_egeon_24h_exp_l_atm_mediacycles_waitr.txt \
${DIRHOME}/graftimestep_egeon_24h_exp_l_0_atm_waits.txt \
${DIRHOME}/graftimestep_egeon_24h_exp_l_0_atm_waitr.txt \
${DIRHOME}/graftimestep_egeon_24h_exp_l_ocn_mediacycles_waitr.txt \
${DIRHOME}/graftimestep_egeon_24h_exp_l_ocn_mediacycles_waits.txt \
${DIRHOME}/graftimestep_egeon_24h_exp_l_0_ocn_waitr.txt \
${DIRHOME}/graftimestep_egeon_24h_exp_l_0_ocn_waits.txt \
${DIRHOME}/graftimestep_egeon_24h_exp_l_TTOTAL.txt \
${DIRHOME}/kit.bash &
