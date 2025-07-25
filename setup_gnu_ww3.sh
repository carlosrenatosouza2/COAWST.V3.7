. /opt/modules/default/etc/modules.sh
#
module purge
module unload netcdf
module unload netcdf-hdf5parallel
module unload libfast
module load libfast
module load PrgEnv-gnu/4.0.36
module load netcdf-hdf5parallel/4.2.0

export NETCDF_CONFIG=/opt/cray/netcdf/4.2.0/gnu/46/nc-config
export NETCDF=${NETCDF_DIR}
export NETCDF_INCDIR=${NETCDF_DIR}/include
export NETCDF_LIBDIR=${NETCDF_DIR}/lib
export HDF5_LIBDIR=${HDF5_DIR}/lib
export PHDF5=${HDF5_DIR}
export NETCDFHOME=${NETCDF_DIR}
export FC=ftn

export WWATCH3_NETCDF=NC4                                         #ambiente para WW3

module list


# ./w3_setup /home/jonas.carvalho/wwatch3/ww3v516/  -c gnu -s swin

#./w3_clean
#./w3_new
#./w3_make









