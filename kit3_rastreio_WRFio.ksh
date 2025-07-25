#!/bin/ksh

DIRHOME=/mnt/beegfs/carlos.souza/Doutorado/COAWST.V3.7
WRFHOME=${DIRHOME}/WRF


nedit \
${DIRHOME}//Master/master.F \
${WRFHOME}/main/module_wrf_top.F \
${WRFHOME}/frame/module_integrate.F \
${WRFHOME}/share/mediation_integrate.F \
${WRFHOME}/share/module_io_domain.F \
${WRFHOME}/share/output_wrf.F \
${WRFHOME}/share/wrf_ext_write_field.F \
${WRFHOME}/frame/module_io.F \
${WRFHOME}/frame/module_dm.F \
${WRFHOME}/external/io_netcdf/wrf_io.F90 \
${WRFHOME}/external/io_netcdf/ext_ncd_put_var_td.code \
${WRFHOME}/external/io_netcdf/field_routines.F90 \
${DIRHOME}/kit3_rastreio_WRFio.ksh &



nedit \
${DIRHOME}/Build/master.f90 \
${WRFHOME}/main/module_wrf_top.f90 \
${WRFHOME}/frame/module_integrate.f90 \
${WRFHOME}/share/mediation_integrate.f90 \
${WRFHOME}/share/module_io_domain.f90 \
${WRFHOME}/share/output_wrf.f90 \
${WRFHOME}/share/wrf_ext_write_field.f90 \
${WRFHOME}/frame/module_io.f90 \
${WRFHOME}/frame/module_dm.f90 \
${WRFHOME}/external/io_netcdf/wrf_io.f \
${WRFHOME}/external/io_netcdf/field_routines.f &






#${WRFHOME}/share/mediation_wrfmain.F \


#${WRFHOME}/external/io_netcdf/wrf_io.F90 \
#${WRFHOME}/external/io_netcdf/wrf_io.f \
#${WRFHOME}/external/io_netcdf/module_wrfsi_static.F90 \
#${WRFHOME}/external/io_netcdf/module_wrfsi_static.f \
#${WRFHOME}/share/track_driver.F \
#${WRFHOME}/share/track_driver.f90 \
#${WRFHOME}/share/module_trajectory.F \
#${WRFHOME}/share/module_trajectory.f90 \


#${WRFHOME}/frame/module_integrate.F \
#${WRFHOME}/frame/module_integrate.f90 \
#${WRFHOME}/main/module_wrf_top.F \
#${WRFHOME}/main/module_wrf_top.f90 \
#${DIRHOME}/kit3_rastreio_WRFio.ksh &

#${WRFHOME}/inc/check_auxstream_alarms.inc \
#${WRFHOME}/inc/med_last_solve_io.inc \
#${WRFHOME}/main/convert_em.F \
#${WRFHOME}/share/mediation_integrate.F \
#${WRFHOME}/share/mediation_integrate.f90 \
#${WRFHOME}/tools/gen_streams.c \
#nedit \
#${WRFHOME}/share/mediation_integrate.F \
#${WRFHOME}/share/module_io_domain.F \
#${WRFHOME}/share/output_wrf.F \
#${WRFHOME}/share/wrf_ext_write_field.F \
#${WRFHOME}/frame/module_io.F \
#${WRFHOME}/external/RSL_LITE/module_dm.F \
#${WRFHOME}/frame/module_dm.F \
#${WRFHOME}/external/io_netcdf/wrf_io.F90 \
#${WRFHOME}/external/io_netcdf/field_routines.F90 \
#${DIRHOME}/howto_compile.sh &

#nedit \
#${WRFHOME}/share/mediation_integrate.f90 \
#${WRFHOME}/share/module_io_domain.f90 \
#${WRFHOME}/share/output_wrf.f90 \
#${WRFHOME}/share/wrf_ext_write_field.f90 \
#${WRFHOME}/frame/module_io.f90 \
#${WRFHOME}/frame/module_dm.f90 \
#${WRFHOME}/external/io_netcdf/wrf_io.f \
#${WRFHOME}/external/io_netcdf/field_routines.f \
#${DIRHOME}/kit3_rastreio_WRFio.ksh &

#nedit \
#${WRFHOME}/share/mediation_integrate.F \
#${WRFHOME}/share/mediation_integrate.f90 \
#${WRFHOME}/share/module_io_domain.F \
#${WRFHOME}/share/module_io_domain.f90 \
#${WRFHOME}/share/output_wrf.F \
#${WRFHOME}/share/output_wrf.f90 \
#${WRFHOME}/share/wrf_ext_write_field.F \
#${WRFHOME}/share/wrf_ext_write_field.f90 \
#${WRFHOME}/frame/module_io.F \
#${WRFHOME}/frame/module_io.f90 \
#${WRFHOME}/external/RSL_LITE/module_dm.F \
#${WRFHOME}/frame/module_dm.F \
#${WRFHOME}/frame/module_dm.f90 \
#${WRFHOME}/external/io_netcdf/wrf_io.F90 \
#${WRFHOME}/external/io_netcdf/wrf_io.f \
#${WRFHOME}/external/io_netcdf/field_routines.F90 \
#${WRFHOME}/external/io_netcdf/field_routines.f \
#${DIRHOME}/howto_compile.sh \
#${DIRHOME}/kit3_rastreio_WRFio.ksh &


#${WRFHOME}/inc/module_io_domain_defs.inc \
#${WRFHOME}/share/wrf_ext_write_field.F \
#${WRFHOME}/share/wrf_ext_write_field.f90 \
#${DIRHOME}/Master/mct_driver.h \
#${DIRHOME}/Build/master.f90 \
#${DIRHOME}/WRF/main/module_wrf_top.F \
#${DIRHOME}/WRF/main/module_wrf_top.f90 \
#${DIRHOME}/WRF/frame/module_integrate.F \
#${DIRHOME}/WRF/frame/module_integrate.f90 \
#${DIRHOME}/WRF/share/output_wrf.F \
#${DIRHOME}/WRF/share/output_wrf.f90 \
#${DIRHOME}/WRF/share/mediation_integrate.F \
#${DIRHOME}/WRF/share/mediation_integrate.f90 \
#${DIRHOME}/WRF/inc/module_io_domain_defs.inc \

#${DIRHOME}/Master/read_model_inputs.F \
#${DIRHOME}/Projects/ATLSW12/atlsw12.h \
#${DIRHOME}/Projects/ATLSW12/wr_17022023/namelist.input \
#${DIRHOME}/Projects/ATLSW12/wr_17022023/ocean_ATLSW12.in \
#${DIRHOME}/Projects/ATLSW12/wr_17022023/coupling_ATLSW12.in \
#${DIRHOME}/Work/ATLSW12/wr_17022023/run_atlsw12_WR.sh \
#${DIRHOME}/Work/ATLSW12/wr_17022023/rws.out \
#${DIRHOME}/coawst.gnu.atlsw12_noclean \
#${DIRHOME}/coawst.gnu.atlsw12 \
#${DIRHOME}/coawst.bash.atlsw12 \
#${DIRHOME}/makefile \
