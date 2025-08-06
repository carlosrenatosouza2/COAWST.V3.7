#!/bin/bash
#SBATCH --job-name=Controle           # nome do job
#SBATCH --partition=batch                # fila que vai submeter
#SBATCH --ntasks=256                    # Total de cores (nodes*tasks)
###SBATCH --nodes=2                       # quantidade de nós
###SBATCH --tasks-per-node=128             # quantidade de cores por nó
#SBATCH --time=08:00:00                  #tempo para rodar
#SBATCH --output=/mnt/beegfs/carlos.souza/temp/COAWST.V3.7/Work/ATLSW12/wr_17022023/rws1_total.out
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
  
  ROOTDIR=/mnt/beegfs/carlos.souza/temp/${COAVER}
  WORKDIR=/mnt/beegfs/carlos.souza/temp/${COAVER}
  PROJDIR=${ROOTDIR}/Projects/${MODEL}/${EXPER}
  EXECDIR=${WORKDIR}/Work/${MODEL}/${EXPER}
  echo '${PROJDIR}='${PROJDIR}
  echo '${EXECDIR}='${EXECDIR}
# 

  cd $SLURM_SUBMIT_DIR

#. ${WORKDIR}/config_env_intel_egeon.ksh
#. ${WORKDIR}/config_env_gnu_egeon.ksh
#. ${WORKDIR}/config_env_gnu_egeon_mylibs.ksh



. ${WORKDIR}/config_env_gnu_egeon_gcc+mpichspack_libsJP.ksh



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
#time /opt/spack/opt/spack/linux-rhel8-zen2/gcc-9.4.0/mpich-4.0.2-gpof2pvzizt3kv5rqkn4jmwakp7n6uk7/bin/mpirun -n ${SLURM_NTASKS} ./coawstM ./coupling_ATLSW12.in &> rws.out
#time mpirun -n ${SLURM_NTASKS} -env MKL_DEBUG_CPU_TYPE=5 -env UCX_NET_DEVICES=mlx5_0:1 -genvall ./coawstM ./coupling_ATLSW12.in &> rws.out
time mpirun -n ${SLURM_NTASKS} -genvall -env UCX_NET_DEVICES=mlx5_0:1 ./coawstM ./coupling_ATLSW12.in &> rws.out
#srun --nodes=2 --ntasks=256 --cpus-per-task=128 ./coawstM ./coupling_ATLSW12.in &> rws.out
date
