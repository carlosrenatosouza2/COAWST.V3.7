#!/bin/ksh

module purge
module load ohpc
module unload openmpi4/4.1.1
module unload gnu9/9.4.0
module list


. /mnt/beegfs/carlos.souza/spack_gnu/env.sh
spack load gcc@11.3.0 mpich@4.0.2

#sem pnetcdf:
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/mnt/beegfs/carlos.souza/spack_gnu/opt/spack/linux-rhel8-zen2/gcc-11.3.0/mpich-4.0.2-6keefgqzkt6d3fcx6ln4xnuskobw3h57/lib
# com pnetcdf:
#export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/mnt/beegfs/carlos.souza/spack_gnu/opt/spack/linux-rhel8-zen2/gcc-11.3.0/mpich-4.0.2-6keefgqzkt6d3fcx6ln4xnuskobw3h57/lib:/mnt/beegfs/carlos.souza/spack_gnu/opt/spack/linux-rhel8-zen2/gcc-11.3.0/parallel-netcdf-1.12.2-zhhlfakl5k3klwg275wwmw6j435pbwix/lib

# sem pnetcdf:
export PKG_CONFIG_PATH=${PKG_CONFIG_PATH}:/mnt/beegfs/carlos.souza/spack_gnu/opt/spack/linux-rhel8-zen2/gcc-11.3.0/mpich-4.0.2-6keefgqzkt6d3fcx6ln4xnuskobw3h57/lib/pkgconfig
#com pnetcdf:
#export PKG_CONFIG_PATH=${PKG_CONFIG_PATH}:/mnt/beegfs/carlos.souza/spack_gnu/opt/spack/linux-rhel8-zen2/gcc-11.3.0/mpich-4.0.2-6keefgqzkt6d3fcx6ln4xnuskobw3h57/lib/pkgconfig:/mnt/beegfs/carlos.souza/spack_gnu/opt/spack/linux-rhel8-zen2/gcc-11.3.0/parallel-netcdf-1.12.2-zhhlfakl5k3klwg275wwmw6j435pbwix/lib/pkgconfig

#sem pnetcdf:
export PATH=${PATH}:/mnt/beegfs/carlos.souza/spack_gnu/opt/spack/linux-rhel8-zen2/gcc-11.3.0/mpich-4.0.2-6keefgqzkt6d3fcx6ln4xnuskobw3h57/bin
#com pnetcdf:
#export PATH=${PATH}:/mnt/beegfs/carlos.souza/spack_gnu/opt/spack/linux-rhel8-zen2/gcc-11.3.0/mpich-4.0.2-6keefgqzkt6d3fcx6ln4xnuskobw3h57/bin:/mnt/beegfs/carlos.souza/spack_gnu/opt/spack/linux-rhel8-zen2/gcc-11.3.0/parallel-netcdf-1.12.2-zhhlfakl5k3klwg275wwmw6j435pbwix/bin
#export INCLUDE=${INCLUDE}:/opt/ohpc/pub/libs/gnu9/openmpi4/pnetcdf/1.12.2/include

export MPI_DIR=/mnt/beegfs/carlos.souza/spack_gnu/opt/spack/linux-rhel8-zen2/gcc-11.3.0/mpich-4.0.2-6keefgqzkt6d3fcx6ln4xnuskobw3h57
export LIBFABRIC_DIR=/mnt/beegfs/carlos.souza/spack_gnu/opt/spack/linux-rhel8-zen2/gcc-11.3.0/libfabric-1.14.1-46vn2sfrhokuqglfoalq6cvxicc56tbn

#export LIBFABRIC_BIN=/mnt/beegfs/carlos.souza/spack_gnu/opt/spack/linux-rhel8-zen2/gcc-11.3.0/libfabric-1.14.1-46vn2sfrhokuqglfoalq6cvxicc56tbn/bin
#export LIBFABRIC_LIB=/mnt/beegfs/carlos.souza/spack_gnu/opt/spack/linux-rhel8-zen2/gcc-11.3.0/libfabric-1.14.1-46vn2sfrhokuqglfoalq6cvxicc56tbn/lib
#export LIBFABRIC_INC=/mnt/beegfs/carlos.souza/spack_gnu/opt/spack/linux-rhel8-zen2/gcc-11.3.0/libfabric-1.14.1-46vn2sfrhokuqglfoalq6cvxicc56tbn/include


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
#export PNETCDF=/mnt/beegfs/carlos.souza/spack_gnu/opt/spack/linux-rhel8-zen2/gcc-11.3.0/parallel-netcdf-1.12.2-zhhlfakl5k3klwg275wwmw6j435pbwix

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

#export PNETCDF_DIR=${PNETCDF}
#export PNETCDF_LIB=${PNETCDF}/lib
#export PNETCDF_INC=${PNETCDF}/include



export WRFIO_NCD_LARGE_FILE_SUPPORT=1
export NETCDF_classic=1
export PNETCDF_QUILT=




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
echo "gfortran:"
which gfortran
echo "mpif90:"
which mpif90
echo "mpirun:"
which mpirun
echo "PNETCDF=$PNETCDF"
echo "PNETCDF_QUILT=${PNETCDF_QUILT}"
