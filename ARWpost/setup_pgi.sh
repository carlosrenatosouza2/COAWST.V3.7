. /opt/modules/default/etc/modules.sh
#
# echo "Compiling with Cray Compiler "
# echo "Compiling with PGI Compiler "
module list
module unload netcdf
module swap PrgEnv-cray PrgEnv-pgi
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
export NETCDF=${NETCDF_DIR}
export NETCDF_INCDIR=${NETCDF_DIR}/include
export NETCDF_LIBDIR=${NETCDF_DIR}/lib
export HDF5_LIBDIR=${HDF5_DIR}/lib
export PHDF5=${HDF5_DIR}
export WRFIO_NCD_LARGE_FILE_SUPPORT=1
export NETCDFHOME=${NETCDF_DIR}
module list
export NETCDF_classic=1
