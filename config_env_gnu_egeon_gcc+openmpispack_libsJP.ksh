#!/bin/ksh

module purge
module load ohpc
module unload openmpi4/4.1.1
module unload gnu9/9.4.0
module list


. /mnt/beegfs/carlos.souza/spack_gnu/env.sh
#spack load gcc@11.3.0 mpich@4.0.2
spack load gcc@11.3.0 openmpi@4.1.3

# Locale
export LC_ALL="en_US.UTF-8"


export DIRBEEGGFS=/mnt/beegfs/carlos.souza/Doutorado
export MCT_INCDIR=${DIRBEEGGFS}/COAWST.V3.7/Lib/MCT/gnu/include
export MCT_LIBDIR=${DIRBEEGGFS}/COAWST.V3.7/Lib/MCT/gnu/lib
export MPEU_LIBDIR=${DIRBEEGGFS}/COAWST.V3.7/Lib/MCT/gnu/lib

# from my libs @JP
export MYLIBS=/mnt/beegfs/carlos.souza/libs
export NETCDF=${MYLIBS}
export HDF5_DIR=${MYLIBS}

export NETCDF_DIR=${NETCDF}
export NETCDF_INCDIR=${NETCDF_DIR}/include
export NETCDF_LIBDIR=${NETCDF_DIR}/lib
export NETCDF_CONFIG=${NETCDF_DIR}/bin/nf-config
export NETCDFHOME=${NETCDF_DIR}
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${MYLIBS}/lib
export PATH=${PATH}:${MYLIBS}/bin

export HDF5=${HDF5_DIR}
export PHDF5=${HDF5_DIR}
export HDF5_LIBDIR=${HDF5_DIR}/lib





export WRFIO_NCD_LARGE_FILE_SUPPORT=1
export NETCDF_classic=1





echo "MCT_INCDIR=$MCT_INCDIR"
echo "MCT_LIBDIR=$MCT_LIBDIR"
echo "MPEU_LIBDI=$MPEU_LIBDIR"
echo ""
echo "NETCDF=$NETCDF"
echo "HDF5_DIR=$HDF5_DIR"
echo "NETCDF_CONFIG=$NETCDF_CONFIG"
echo "MPI_DIR=$MPI_DIR"
echo "LD_LIBRARY_PATH=${LD_LIBRARY_PATH}"
echo ""
echo "PATH=${PATH}"
echo ""

echo "gfortran --version"
gfortran --version
echo "which:"
which gfortran
which mpif90
which mpirun
