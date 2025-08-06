#!/bin/bash
#SBATCH --job-name=ExpA0           # nome do job
#SBATCH --nodes=10                       # quantidade de nós
#SBATCH --partition=batch                # fila que vai submeter
#SBATCH --tasks-per-node=64             # quantidade de cores por nó
#SBATCH --ntasks=640                    # Total de cores (nodes*tasks)
#SBATCH --time=8:00:00                  #tempo para rodar
#SBATCH --output=/mnt/beegfs/carlos.souza/Doutorado/COAWST.V3.7/Work/ATLSW12/wr_17022023/rws1_total.out
#SBATCH --exclusive
#
# Script to run COAWST in a given Work directory
# Luciano Pezzi - February 2022
#
  echo "Running COAWST on EGEON"
  echo "Rodando em ${ntasks} CORES"
#
# Machine defifnitions for CRAY
#
  export MPICH_ENV_DISPLAY=1
  export MPICH_ABORT_ON_ERROR=1
  export MPICH_RANK_REORDER_DISPLAY=1
  export OMP_NUM_THREADS=1
#
# Root, Project and work directories definition
#  
  MODEL=ATLSW12
  COAVER=COAWST.V3.7
  EXPER=wr_17022023
  
  ROOTDIR=/mnt/beegfs/carlos.souza/Doutorado/${COAVER}
  WORKDIR=/mnt/beegfs/carlos.souza/Doutorado/COAWST.V3.7
  PROJDIR=${ROOTDIR}/Projects/${MODEL}/${EXPER}
  EXECDIR=${WORKDIR}/Work/${MODEL}/${EXPER}
  echo '${PROJDIR}='${PROJDIR}
  echo '${EXECDIR}='${EXECDIR}
# 

  cd $SLURM_SUBMIT_DIR

. ${WORKDIR}/config_env_gnu_egeon.ksh


 echo "Lista de módulos carregados: "
 module list
 echo "============================="

# 
# Copying input files for WRF
#
./link.sh
\cp -f ${ROOTDIR}/LANDUSE.TBL .
\cp -f ${ROOTDIR}/GENPARM.TBL .
\cp -f ${ROOTDIR}/VEGPARM.TBL .
\cp -f ${ROOTDIR}/URBPARM.TBL . 
\cp -f ${ROOTDIR}/SOILPARM.TBL .
\cp -f ${ROOTDIR}/RRTM_DATA_DBL . 
\cp -f ${ROOTDIR}/RRTM_DATA .
\cp -f ${ROOTDIR}/WRF/run/RRTMG* .

#\cp -f -r ${PROJDIR}/200* .
\cp -f ${PROJDIR}/swa12_*.nc .
\cp -f ${PROJDIR}/namelist.input .
\cp -f ${PROJDIR}/roms_ini_*.nc .
\cp -f ${PROJDIR}/roms_bry_*.nc .
\cp -f ${PROJDIR}/wrflowinp_d0* .
\cp -f ${PROJDIR}/wrfbdy_d0* .
\cp -f ${PROJDIR}/wrfinput_d0* .
\cp -f ${PROJDIR}/coupling_ATLSW12.in .
\cp -f ${PROJDIR}/ocean_ATLSW12.in .
#\cp -f ${PROJDIR}/*swan* .
#\cp -f ${PROJDIR}/swan* .
\cp -f ${PROJDIR}/scrip_wrs_static.nc .
\cp -f ${ROOTDIR}/coawstM .
#
# sleep 30
#
  chmod 755 *

ulimit -s unlimited
MPI_PARAMS="-iface ib0 -bind-to core -map-by core"
export OMP_NUM_THREADS=1
export MKL_NUM_THREADS=1
export I_MPI_DEBUG=5
export MKL_DEBUG_CPU_TYPE=5
#
# Submitting Job
#
date
#time mpirun -env MKL_DEBUG_CPU_TYPE=5 -env UCX_NET_DEVICES=mlx5_0:1 -genvall ./coawstM ./coupling_ATLSW12.in &> rws.out
time mpirun ./coawstM ./coupling_ATLSW12.in &> rws.out
#time srun ./coawstM ./coupling_ATLSW12.in &> rws.out
date
