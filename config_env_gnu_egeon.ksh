#!/bin/ksh

module purge
module load ohpc
module load phdf5
module load netcdf
module load netcdf-fortran
module list

# Locale
export LC_ALL="en_US.UTF-8"


export DIRBEEGGFS=/mnt/beegfs/carlos.souza/Doutorado
export MCT_INCDIR=${DIRBEEGGFS}/COAWST.V3.7/Lib/MCT/gnu/include
export MCT_LIBDIR=${DIRBEEGGFS}/COAWST.V3.7/Lib/MCT/gnu/lib
export MPEU_LIBDIR=${DIRBEEGGFS}/COAWST.V3.7/Lib/MCT/gnu/lib

export NETCDF=${NETCDF_FORTRAN_DIR}
#export NETCDF=/mnt/beegfs/paulo.kubota/lib/netcdf
#export NETCDF=/mnt/beegfs/monan/libs/netcdf
#export HDF5_DIR=/mnt/beegfs/paulo.kubota/lib/hdf5-1.12.1

export HDF5=${HDF5_DIR}
export PHDF5=${HDF5_DIR}
export PATH=${PATH}:${NETCDF}/bin:${HDF5_DIR}:${HDF5_DIR}/bin
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${NETCDF}/bin:${HDF5_DIR}:${HDF5_DIR}/bin

export NETCDF_DIR=${NETCDF}
export NETCDF_INCDIR=${NETCDF_DIR}/include
export NETCDF_LIBDIR=${NETCDF_DIR}/lib
export HDF5_LIBDIR=${HDF5_DIR}/lib
export WRFIO_NCD_LARGE_FILE_SUPPORT=1
export NETCDFHOME=${NETCDF_DIR}
export NETCDF_classic=1

export NETCDF_CONFIG=${NETCDF_DIR}/bin/nf-config
#export NETCDF_CONFIG=/opt/ohpc/pub/libs/gnu9/openmpi4/netcdf/4.7.4/bin/nc-config
#export NETCDF_CONFIG=/mnt/beegfs/paulo.kubota/lib/netcdf/bin/nc-config
#export NETCDF_CONFIG=${NETCDF_DIR}/bin/nc-config

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
