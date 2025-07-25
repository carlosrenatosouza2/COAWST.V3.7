#!/bin/bash


# Config.:
# /home/carlos.renato/COAWST.V3.7/Projects/ATLSW12/wr_17022023


cd /home/carlos.renato/COAWST.V3.7/Work/ATLSW12/wr_17022023

# limpa a area:
limpa.sh

qsub run_atlsw12_WR.sh
