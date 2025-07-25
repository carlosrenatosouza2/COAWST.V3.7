#!/bin/ksh

DIRHOME=/mnt/beegfs/carlos.souza/Doutorado/COAWST.V3.7

nedit \
${DIRHOME}/Master/mct_driver.h \
${DIRHOME}/Build/master.f90 \
${DIRHOME}/Master/read_coawst_par.F \
${DIRHOME}/Build/read_coawst_par.f90 \
${DIRHOME}/Master/MCT_coupler/mct_coupler_params.F \
${DIRHOME}/Build/mct_coupler_params.f90 \
${DIRHOME}/Build/read_model_inputs.f90 \
${DIRHOME}/WRF/main/module_wrf_top.F \
${DIRHOME}/WRF/main/module_wrf_top.f90 \
${DIRHOME}/IOmod/src/iomod.f90 \
${DIRHOME}/IOmod/Makefile \
${DIRHOME}/Projects/ATLSW12/atlsw12.h \
${DIRHOME}/Projects/ATLSW12/wr_17022023/namelist.input \
${DIRHOME}/Projects/ATLSW12/wr_17022023/ocean_ATLSW12.in \
${DIRHOME}/Projects/ATLSW12/wr_17022023/coupling_ATLSW12.in \
${DIRHOME}/Work/ATLSW12/wr_17022023/run_atlsw12_WR.sh \
${DIRHOME}/howto_compile.sh \
${DIRHOME}/Work/ATLSW12/wr_17022023/rws.out \
${DIRHOME}/coawst.gnu.atlsw12_noclean \
${DIRHOME}/coawst.gnu.atlsw12 \
${DIRHOME}/coawst.bash.atlsw12 \
${DIRHOME}/makefile \
${DIRHOME}/Compilers/make_macros.h \
${DIRHOME}/Compilers/Linux-gfortran.mk \
${DIRHOME}/Build/MakeDepend \
${DIRHOME}/kit2.ksh &



#${DIRHOME}/Master/read_model_inputs.F \
