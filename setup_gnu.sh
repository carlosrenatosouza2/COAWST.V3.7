. /opt/modules/default/etc/modules.sh
#

module list
module unload netcdf
module swap PrgEnv-cray PrgEnv-gnu
#module swap PrgEnv-pgi PrgEnv-cray
module unload netcdf-hdf5parallel
module load netcdf-hdf5parallel
module unload libfast
module load libfast
export MCT_INCDIR=${HOME}/COAWST.V3.7/Lib/MCT/pgi/include
export MCT_LIBDIR=${HOME}/COAWST.V3.7/Lib/MCT/pgi/lib
export MPEU_LIBDIR=${HOME}/COAWST.V3.7/Lib/MCT/pgi/lib
#export MCT_INCDIR=${HOME}/COAWST.V3.2/Lib/MCT/pgi/include
#export MCT_LIBDIR=${HOME}/COAWST.V3.2/Lib/MCT/pgi/lib
#export MPEU_LIBDIR=${HOME}/COAWST.V3.2/Lib/MCT/pgi/lib
export NETCDF_CONFIG=/opt/cray/netcdf/4.1.3/pgi/109/bin/nc-config
export NETCDF=${NETCDF_DIR}
export NETCDF_INCDIR=${NETCDF_DIR}/include
export NETCDF_LIBDIR=${NETCDF_DIR}/lib
export HDF5_LIBDIR=${HDF5_DIR}/lib
export HDF5=${HDF5_DIR}
export PHDF5=${HDF5_DIR}
export WRFIO_NCD_LARGE_FILE_SUPPORT=1
export NETCDFHOME=${NETCDF_DIR}
export NETCDF_classic=1


export FC=ftn
export CC=gcc
export WWATCH3_NETCDF=NC4                                         #ambiente para WW3

#module load perftools-lite/6.1.0
module list
