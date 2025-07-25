. /opt/modules/default/etc/modules.sh
#
# echo "Compiling with Cray Compiler "
# echo "Compiling with PGI Compiler "
module list
module unload netcdf
module swap PrgEnv-cray PrgEnv-pgi
#module swap PrgEnv-pgi PrgEnv-cray
module unload netcdf-hdf5parallel
#module load netcdf-hdf5parallel
module load netcdf/4.1.3
module unload libfast
module load libfast

export NETCDF_CONFIG=/opt/cray/netcdf/4.1.3/pgi/109/bin/nc-config
export NETCDF=${NETCDF_DIR}
export NETCDF_INCDIR=${NETCDF_DIR}/include
export NETCDF_LIBDIR=${NETCDF_DIR}/lib
export HDF5_LIBDIR=${HDF5_DIR}/lib
export PHDF5=${HDF5_DIR}
export NETCDFHOME=${NETCDF_DIR}
#export FC=ftn

export WWATCH3_NETCDF=NC4                                         #ambiente para WW3

module list

#./w3_clean
#./w3_new

#./w3_make









