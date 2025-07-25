#!/bin/ksh

module purge
module load ohpc
module swap gnu9 intel
module swap openmpi4 impi
module load hwloc
module load phdf5
module load netcdf
module load netcdf-fortran
module list

# Locale
export LC_ALL="en_US.UTF-8"

module list

export DIRBEEGGFS=/mnt/beegfs/carlos.souza/Doutorado
export MCT_INCDIR=${DIRBEEGGFS}/COAWST.V3.7/Lib/MCT/intel/include
export MCT_LIBDIR=${DIRBEEGGFS}/COAWST.V3.7/Lib/MCT/intel/lib
export MPEU_LIBDIR=${DIRBEEGGFS}/COAWST.V3.7/Lib/MCT/intel/lib

export NETCDF=${NETCDF_FORTRAN_DIR}
export HDF5=${HDF5_DIR}
export PHDF5=${HDF5_DIR}
export PATH=${PATH}:${NETCDF}/bin:${HDF5_DIR}:${HDF5_DIR}/bin:/opt/ohpc/pub/libs/intel/impi/netcdf/4.7.4/bin

export NETCDF_DIR=${NETCDF}
export NETCDF_INCDIR=${NETCDF_DIR}/include
export NETCDF_LIBDIR=${NETCDF_DIR}/lib
export HDF5_LIBDIR=${HDF5_DIR}/lib
export WRFIO_NCD_LARGE_FILE_SUPPORT=1
export NETCDFHOME=${NETCDF_DIR}
export NETCDF_classic=1
#export NETCDF_CONFIG=${NETCDF_DIR}/bin/nf-config
export NETCDF_CONFIG=/opt/ohpc/pub/libs/intel/impi/netcdf/4.7.4/bin/nc_config

echo "MCT_INCDIR=$MCT_INCDIR"
echo "MCT_LIBDIR=$MCT_LIBDIR"
echo "MPEU_LIBDI=$MPEU_LIBDIR"
echo ""
echo "NETCDF=$NETCDF"
echo "HDF5_DIR=$HDF5_DIR"
echo "NETCDF_CONFIG=$NETCDF_CONFIG"
