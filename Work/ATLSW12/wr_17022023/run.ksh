#!/bin/ksh

limpa.sh

sbatch   run_atlsw12_WR.sh
hora=$(date | awk '{print $4}')

myqs



# Experimentos rodar com outros config. de cores ::
#
# - modificar namelis.input: WRF
#     nproc_x                             = 24,   !C:32,  A:16,
#     nproc_y                             = 30,   !C:40,  A:20,
#
# - modificar coupling_ATLSW12.in :
#     NnodesATM =  1280                    ! atmospheric model 40x32
#     NnodesATM =  720                    ! atmospheric model 30x24
#     NnodesATM =  320                    ! atmospheric model 20x16
#
# - modificar em run_atlsw12_WR.sh:
#     #PBS -l mppwidth=1040     B
#     #PBS -l mppwidth=640     A
#     #PBS -l mppwidth=1600     C
#
#     aprun -n 640 coawstM ./coupling_ATLSW12.in 1> rws1.out 2> rws1.err     !A
#     aprun -n 1040 coawstM ./coupling_ATLSW12.in 1> rws1.out 2> rws1.err     !B
#     aprun -n 1600 coawstM ./coupling_ATLSW12.in 1> rws1.out 2> rws1.err     !B



