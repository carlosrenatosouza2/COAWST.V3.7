#!/bin/sh
#PBS -l mppwidth=32
#PBS -N ARWpost
#PBS -j oe
#PBS -o /scratch/luciano.pezzi/COAWST.V3.7/ARWpost/ARW_test.out
#PBS -l walltime=02:30:00
#PBS -q workq
  echo "Running ARWpost on KERANA"
#
  export MPICH_ENV_DISPLAY=1
  export MPICH_ABORT_ON_ERROR=1
  export MPICH_RANK_REORDER_DISPLAY=1
  export MPICH_RANK_REORDER_METHOD=1
  export MALLOC_MMAP_MAX_=0
  export MALLOC_TRIM_THRESHOLD_=536870912
  export OMP_NUM_THREADS=1
#
  EXECDIR=${PBS_O_WORKDIR}
  
  cd $EXECDIR
export  FILENV=assign.txt
cat > assign.txt <<EOT
assign -N swap_endian g:su
assign -N swap_endian g:du
EOT

export  ATP_ENABLED=1
 ulimit -a
 ulimit -c unlimited
 ulimit -s unlimited
#ulimit -m unlimited
# ulimit -a
 
  aprun -n 1 ./ARWpost.exe  1> arw.out 2> arw.err
  
# \rm metgrid.log.00*
