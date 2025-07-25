. /opt/modules/default/etc/modules.sh
#
echo "Compiling with Cray Compiler "
# echo "Compiling with PGI Compiler "
module list
module unload netcdf
module swap PrgEnv-cray PrgEnv-pgi
#module swap PrgEnv-pgi PrgEnv-cray
module unload netcdf-hdf5parallel
#module load netcdf-hdf5parallel
module load netcdf
module unload libfast
# module load libfast
export MCT_INCDIR=${HOME}/COAWST/Lib/MCT/include
export MCT_LIBDIR=${HOME}/COAWST/Lib/MCT/lib
export MPEU_LIBDIR=${HOME}/COAWST/Lib/MCT/lib
export NETCDF=${NETCDF_DIR}
export NETCDF_INCDIR=${NETCDF_DIR}/include
export HDF5_LIBDIR=${HDF5_DIR}/lib
export PHDF5=${HDF5_DIR}
export WRFIO_NCD_LARGE_FILE_SUPPORT=1

export JASPERLIB=${HOME}/local/lib 
export JASPERINC=${HOME}/local/include
module list
