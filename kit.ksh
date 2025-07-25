#!/bin/ksh

DIRHOME=/mnt/beegfs/carlos.souza/Doutorado/COAWST.V3.7

nedit ${DIRHOME}/Build/master.f90 \
${DIRHOME}/WRF/main/module_wrf_top.F \
${DIRHOME}/WRF/main/module_wrf_top.f90 \
${DIRHOME}/WRF/frame/module_integrate.F \
${DIRHOME}/WRF/frame/module_integrate.f90 \
${DIRHOME}/WRF/frame/atm_coupler.F \
${DIRHOME}/WRF/frame/atm_coupler.f90 \
${DIRHOME}/WRF/frame/module_domain.f90 \
${DIRHOME}/WRF/frame/module_domain.F \
${DIRHOME}/Lib/MCT/mct/m_Transfer.F90 \
${DIRHOME}/Build/ocean_control.f90 \
${DIRHOME}/Build/main3d.f90 \
${DIRHOME}/Build/ocean_coupler.f90 \
${DIRHOME}/Projects/ATLSW12/wr_17022023/namelist.input \
${DIRHOME}/Projects/ATLSW12/wr_17022023/ocean_ATLSW12.in \
${DIRHOME}/Projects/ATLSW12/wr_17022023/coupling_ATLSW12.in \
${DIRHOME}/Work/ATLSW12/wr_17022023/run_atlsw12_WR.sh \
${DIRHOME}/Work/ATLSW12/wr_17022023/run.ksh \
${DIRHOME}/WRF/share/solve_interface.F \
${DIRHOME}/WRF/share/solve_interface.f90 \
${DIRHOME}/WRF/frame/module_io.F \
${DIRHOME}/WRF/frame/module_io.f90 \
${DIRHOME}/WRF/frame/module_io_quilt.F \
${DIRHOME}/WRF/frame/module_io_quilt_new.F \
${DIRHOME}/WRF/frame/module_io_quilt_old.F \
${DIRHOME}/WRF/frame/module_io_quilt.f90 \
${DIRHOME}/kit.ksh \
${DIRHOME}/coawst.bash.atlsw12 \
${DIRHOME}/Compilers/Linux-gfortran.mk \
${DIRHOME}/howto_compile.sh \
${DIRHOME}/config_env_gnu_egeon_gcc+mpichspack_libsJP.ksh &

echo ""
echo "wrf_run             > integrate        (WRF/frame/module_integrate.F -> f90"
echo "integrate           > atm_coupling     (WRF/frame/atm_coupler.F     -> f90"
echo "atm_coupling        > atm2ocn_coupling (WRF/frame/atm_coupler.F     -> f90"
echo "atm2ocn_coupling    > MCT_ISend        (Lib/MCT/mct/m_Transfer.F90"
echo "MCT_ISend => isend_ > MPI_ISend        (Lib/MCT/mct/m_Transfer.F90"
