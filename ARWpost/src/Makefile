
include ../configure.arwp

OBJS = module_model_basics.o constants_module.o gridinfo_module.o ARWpost.o input_module.o output_module.o module_map_utils.o misc_definitions_module.o module_date_pack.o module_debug.o process_domain_module.o module_get_file_names.o module_interp.o module_basic_arrays.o module_diagnostics.o module_arrays.o module_pressure.o module_calc_height.o module_calc_pressure.o module_calc_theta.o module_calc_tk.o module_calc_tc.o module_calc_td.o module_calc_td2.o module_calc_rh.o module_calc_rh2.o module_calc_uvmet.o module_calc_slp.o module_calc_dbz.o module_calc_cape.o module_calc_wdir.o module_calc_wspd.o module_calc_clfr.o


wrong: 
	clear ;
	@echo " "
	@echo "go up one directory and type compile to build ARWpost"
	@echo " "
	@echo " "


all: ARWpost.exe

ARWpost.exe: $(OBJS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $@ $(OBJS)  \
		-L$(NETCDF)/lib -I$(NETCDF)/include  -lnetcdff -lnetcdf 


module_model_basics.o:

constants_module.o:

misc_definitions_module.o:

module_date_pack.o:

module_get_file_names.o:

module_arrays.o: module_model_basics.o gridinfo_module.o

module_pressure.o: module_model_basics.o

module_interp.o: input_module.o module_arrays.o module_pressure.o

module_map_utils.o:

module_debug.o:

gridinfo_module.o: misc_definitions_module.o module_debug.o module_get_file_names.o

input_module.o: gridinfo_module.o misc_definitions_module.o 

output_module.o: input_module.o module_model_basics.o module_arrays.o module_interp.o

process_domain_module.o: module_date_pack.o gridinfo_module.o input_module.o output_module.o misc_definitions_module.o module_debug.o module_interp.o module_basic_arrays.o module_diagnostics.o module_arrays.o module_model_basics.o module_pressure.o

ARWpost.o: gridinfo_module.o module_debug.o process_domain_module.o


module_basic_arrays.o: gridinfo_module.o module_arrays.o module_pressure.o module_interp.o constants_module.o

module_diagnostics.o: gridinfo_module.o output_module.o module_arrays.o module_interp.o module_pressure.o constants_module.o module_calc_height.o module_calc_pressure.o module_calc_tk.o module_calc_tc.o module_calc_theta.o module_calc_td.o module_calc_td2.o module_calc_rh.o module_calc_rh2.o module_calc_uvmet.o module_calc_slp.o module_calc_dbz.o module_calc_cape.o module_calc_wdir.o module_calc_wspd.o module_calc_clfr.o

module_calc_cape.o: module_model_basics.o constants_module.o
module_calc_dbz.o: module_model_basics.o constants_module.o
module_calc_height.o: module_model_basics.o constants_module.o
module_calc_pressure.o: module_model_basics.o
module_calc_slp.o: module_model_basics.o constants_module.o
module_calc_rh.o: module_model_basics.o constants_module.o
module_calc_rh2.o: module_model_basics.o constants_module.o
module_calc_tk.o: module_model_basics.o constants_module.o
module_calc_tc.o: module_model_basics.o constants_module.o
module_calc_td.o: module_model_basics.o
module_calc_td2.o: module_model_basics.o
module_calc_theta.o: module_model_basics.o
module_calc_uvmet.o: module_model_basics.o
module_calc_wdir.o: module_model_basics.o
module_calc_wspd.o: module_model_basics.o
module_calc_clfr.o: module_model_basics.o

clean:
	rm -f $(OBJS) *.mod

clobber:
	rm -f $(OBJS) *.mod
	rm -f *.f
	rm -f ARWpost.exe 
