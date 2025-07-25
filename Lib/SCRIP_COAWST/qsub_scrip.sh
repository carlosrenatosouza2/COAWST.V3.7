#!/bin/sh
#PBS -l mppwidth=32
#PBS -N SCRIP
#PBS -j oe
#PBS -o Joe_test.out
#PBS -l walltime=35600:00:00
#PBS -q workq
  echo "Running GEOGRID on KERANA"
#
  export MPICH_ENV_DISPLAY=1
  export MPICH_ABORT_ON_ERROR=1
  export MPICH_RANK_REORDER_DISPLAY=1
  export MPICH_RANK_REORDER_METHOD=1
  export MALLOC_MMAP_MAX_=0
  export MALLOC_TRIM_THRESHOLD_=536870912
  export OMP_NUM_THREADS=1
#
  EXECDIR=/scratch/luciano.pezzi/COAWST.V3.7/Lib/SCRIP_COAWST
  chmod 755 *
# export  ATP_ENABLED=1
# ulimit -a
# ulimit -c unlimited
# ulimit -s unlimited
# ulimit -m unlimited
# ulimit -a

  
 cd $EXECDIR
 
 aprun -n 32 scrip_coawst /scratch/luciano.pezzi/COAWST.V3.7/Lib/SCRIP_COAWST/scrip_coawst_wrs.in 1> joe.out 2> joe.err
  
