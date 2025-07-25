










!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nl_config.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
SUBROUTINE nl_get_run_days ( id_id , run_days )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: run_days
  INTEGER id_id
  run_days = model_config_rec%run_days
  RETURN
END SUBROUTINE nl_get_run_days
SUBROUTINE nl_get_run_hours ( id_id , run_hours )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: run_hours
  INTEGER id_id
  run_hours = model_config_rec%run_hours
  RETURN
END SUBROUTINE nl_get_run_hours
SUBROUTINE nl_get_run_minutes ( id_id , run_minutes )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: run_minutes
  INTEGER id_id
  run_minutes = model_config_rec%run_minutes
  RETURN
END SUBROUTINE nl_get_run_minutes
SUBROUTINE nl_get_run_seconds ( id_id , run_seconds )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: run_seconds
  INTEGER id_id
  run_seconds = model_config_rec%run_seconds
  RETURN
END SUBROUTINE nl_get_run_seconds
SUBROUTINE nl_get_start_year ( id_id , start_year )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: start_year
  INTEGER id_id
  start_year = model_config_rec%start_year(id_id)
  RETURN
END SUBROUTINE nl_get_start_year
SUBROUTINE nl_get_start_month ( id_id , start_month )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: start_month
  INTEGER id_id
  start_month = model_config_rec%start_month(id_id)
  RETURN
END SUBROUTINE nl_get_start_month
SUBROUTINE nl_get_start_day ( id_id , start_day )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: start_day
  INTEGER id_id
  start_day = model_config_rec%start_day(id_id)
  RETURN
END SUBROUTINE nl_get_start_day
SUBROUTINE nl_get_start_hour ( id_id , start_hour )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: start_hour
  INTEGER id_id
  start_hour = model_config_rec%start_hour(id_id)
  RETURN
END SUBROUTINE nl_get_start_hour
SUBROUTINE nl_get_start_minute ( id_id , start_minute )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: start_minute
  INTEGER id_id
  start_minute = model_config_rec%start_minute(id_id)
  RETURN
END SUBROUTINE nl_get_start_minute
SUBROUTINE nl_get_start_second ( id_id , start_second )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: start_second
  INTEGER id_id
  start_second = model_config_rec%start_second(id_id)
  RETURN
END SUBROUTINE nl_get_start_second
SUBROUTINE nl_get_end_year ( id_id , end_year )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: end_year
  INTEGER id_id
  end_year = model_config_rec%end_year(id_id)
  RETURN
END SUBROUTINE nl_get_end_year
SUBROUTINE nl_get_end_month ( id_id , end_month )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: end_month
  INTEGER id_id
  end_month = model_config_rec%end_month(id_id)
  RETURN
END SUBROUTINE nl_get_end_month
SUBROUTINE nl_get_end_day ( id_id , end_day )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: end_day
  INTEGER id_id
  end_day = model_config_rec%end_day(id_id)
  RETURN
END SUBROUTINE nl_get_end_day
SUBROUTINE nl_get_end_hour ( id_id , end_hour )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: end_hour
  INTEGER id_id
  end_hour = model_config_rec%end_hour(id_id)
  RETURN
END SUBROUTINE nl_get_end_hour
SUBROUTINE nl_get_end_minute ( id_id , end_minute )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: end_minute
  INTEGER id_id
  end_minute = model_config_rec%end_minute(id_id)
  RETURN
END SUBROUTINE nl_get_end_minute
SUBROUTINE nl_get_end_second ( id_id , end_second )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: end_second
  INTEGER id_id
  end_second = model_config_rec%end_second(id_id)
  RETURN
END SUBROUTINE nl_get_end_second
SUBROUTINE nl_get_interval_seconds ( id_id , interval_seconds )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: interval_seconds
  INTEGER id_id
  interval_seconds = model_config_rec%interval_seconds
  RETURN
END SUBROUTINE nl_get_interval_seconds
SUBROUTINE nl_get_input_from_file ( id_id , input_from_file )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: input_from_file
  INTEGER id_id
  input_from_file = model_config_rec%input_from_file(id_id)
  RETURN
END SUBROUTINE nl_get_input_from_file
SUBROUTINE nl_get_fine_input_stream ( id_id , fine_input_stream )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: fine_input_stream
  INTEGER id_id
  fine_input_stream = model_config_rec%fine_input_stream(id_id)
  RETURN
END SUBROUTINE nl_get_fine_input_stream
SUBROUTINE nl_get_input_from_hires ( id_id , input_from_hires )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: input_from_hires
  INTEGER id_id
  input_from_hires = model_config_rec%input_from_hires(id_id)
  RETURN
END SUBROUTINE nl_get_input_from_hires
SUBROUTINE nl_get_rsmas_data_path ( id_id , rsmas_data_path )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: rsmas_data_path
  INTEGER id_id
  rsmas_data_path = trim(model_config_rec%rsmas_data_path)
  RETURN
END SUBROUTINE nl_get_rsmas_data_path
SUBROUTINE nl_get_all_ic_times ( id_id , all_ic_times )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: all_ic_times
  INTEGER id_id
  all_ic_times = model_config_rec%all_ic_times
  RETURN
END SUBROUTINE nl_get_all_ic_times
SUBROUTINE nl_get_julyr ( id_id , julyr )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: julyr
  INTEGER id_id
  julyr = model_config_rec%julyr(id_id)
  RETURN
END SUBROUTINE nl_get_julyr
SUBROUTINE nl_get_julday ( id_id , julday )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: julday
  INTEGER id_id
  julday = model_config_rec%julday(id_id)
  RETURN
END SUBROUTINE nl_get_julday
SUBROUTINE nl_get_gmt ( id_id , gmt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: gmt
  INTEGER id_id
  gmt = model_config_rec%gmt(id_id)
  RETURN
END SUBROUTINE nl_get_gmt
SUBROUTINE nl_get_input_inname ( id_id , input_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: input_inname
  INTEGER id_id
  input_inname = trim(model_config_rec%input_inname)
  RETURN
END SUBROUTINE nl_get_input_inname
SUBROUTINE nl_get_input_outname ( id_id , input_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: input_outname
  INTEGER id_id
  input_outname = trim(model_config_rec%input_outname)
  RETURN
END SUBROUTINE nl_get_input_outname
SUBROUTINE nl_get_bdy_inname ( id_id , bdy_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: bdy_inname
  INTEGER id_id
  bdy_inname = trim(model_config_rec%bdy_inname)
  RETURN
END SUBROUTINE nl_get_bdy_inname
SUBROUTINE nl_get_bdy_outname ( id_id , bdy_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: bdy_outname
  INTEGER id_id
  bdy_outname = trim(model_config_rec%bdy_outname)
  RETURN
END SUBROUTINE nl_get_bdy_outname
SUBROUTINE nl_get_rst_inname ( id_id , rst_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: rst_inname
  INTEGER id_id
  rst_inname = trim(model_config_rec%rst_inname)
  RETURN
END SUBROUTINE nl_get_rst_inname
SUBROUTINE nl_get_rst_outname ( id_id , rst_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: rst_outname
  INTEGER id_id
  rst_outname = trim(model_config_rec%rst_outname)
  RETURN
END SUBROUTINE nl_get_rst_outname
SUBROUTINE nl_get_write_input ( id_id , write_input )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: write_input
  INTEGER id_id
  write_input = model_config_rec%write_input
  RETURN
END SUBROUTINE nl_get_write_input
SUBROUTINE nl_get_write_restart_at_0h ( id_id , write_restart_at_0h )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: write_restart_at_0h
  INTEGER id_id
  write_restart_at_0h = model_config_rec%write_restart_at_0h
  RETURN
END SUBROUTINE nl_get_write_restart_at_0h
SUBROUTINE nl_get_write_hist_at_0h_rst ( id_id , write_hist_at_0h_rst )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: write_hist_at_0h_rst
  INTEGER id_id
  write_hist_at_0h_rst = model_config_rec%write_hist_at_0h_rst
  RETURN
END SUBROUTINE nl_get_write_hist_at_0h_rst
SUBROUTINE nl_get_adjust_output_times ( id_id , adjust_output_times )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: adjust_output_times
  INTEGER id_id
  adjust_output_times = model_config_rec%adjust_output_times
  RETURN
END SUBROUTINE nl_get_adjust_output_times
SUBROUTINE nl_get_adjust_input_times ( id_id , adjust_input_times )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: adjust_input_times
  INTEGER id_id
  adjust_input_times = model_config_rec%adjust_input_times
  RETURN
END SUBROUTINE nl_get_adjust_input_times
SUBROUTINE nl_get_diag_print ( id_id , diag_print )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: diag_print
  INTEGER id_id
  diag_print = model_config_rec%diag_print
  RETURN
END SUBROUTINE nl_get_diag_print
SUBROUTINE nl_get_nocolons ( id_id , nocolons )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: nocolons
  INTEGER id_id
  nocolons = model_config_rec%nocolons
  RETURN
END SUBROUTINE nl_get_nocolons
SUBROUTINE nl_get_cycling ( id_id , cycling )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: cycling
  INTEGER id_id
  cycling = model_config_rec%cycling
  RETURN
END SUBROUTINE nl_get_cycling
SUBROUTINE nl_get_output_diagnostics ( id_id , output_diagnostics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: output_diagnostics
  INTEGER id_id
  output_diagnostics = model_config_rec%output_diagnostics
  RETURN
END SUBROUTINE nl_get_output_diagnostics
SUBROUTINE nl_get_nwp_diagnostics ( id_id , nwp_diagnostics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: nwp_diagnostics
  INTEGER id_id
  nwp_diagnostics = model_config_rec%nwp_diagnostics
  RETURN
END SUBROUTINE nl_get_nwp_diagnostics
SUBROUTINE nl_get_output_ready_flag ( id_id , output_ready_flag )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: output_ready_flag
  INTEGER id_id
  output_ready_flag = model_config_rec%output_ready_flag
  RETURN
END SUBROUTINE nl_get_output_ready_flag
SUBROUTINE nl_get_force_use_old_data ( id_id , force_use_old_data )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: force_use_old_data
  INTEGER id_id
  force_use_old_data = model_config_rec%force_use_old_data
  RETURN
END SUBROUTINE nl_get_force_use_old_data
SUBROUTINE nl_get_usepio ( id_id , usepio )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: usepio
  INTEGER id_id
  usepio = model_config_rec%usepio
  RETURN
END SUBROUTINE nl_get_usepio
SUBROUTINE nl_get_pioprocs ( id_id , pioprocs )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: pioprocs
  INTEGER id_id
  pioprocs = model_config_rec%pioprocs
  RETURN
END SUBROUTINE nl_get_pioprocs
SUBROUTINE nl_get_piostart ( id_id , piostart )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: piostart
  INTEGER id_id
  piostart = model_config_rec%piostart
  RETURN
END SUBROUTINE nl_get_piostart
SUBROUTINE nl_get_piostride ( id_id , piostride )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: piostride
  INTEGER id_id
  piostride = model_config_rec%piostride
  RETURN
END SUBROUTINE nl_get_piostride
SUBROUTINE nl_get_pioshift ( id_id , pioshift )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: pioshift
  INTEGER id_id
  pioshift = model_config_rec%pioshift
  RETURN
END SUBROUTINE nl_get_pioshift
SUBROUTINE nl_get_dfi_opt ( id_id , dfi_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_opt
  INTEGER id_id
  dfi_opt = model_config_rec%dfi_opt
  RETURN
END SUBROUTINE nl_get_dfi_opt
SUBROUTINE nl_get_dfi_savehydmeteors ( id_id , dfi_savehydmeteors )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_savehydmeteors
  INTEGER id_id
  dfi_savehydmeteors = model_config_rec%dfi_savehydmeteors
  RETURN
END SUBROUTINE nl_get_dfi_savehydmeteors
SUBROUTINE nl_get_dfi_nfilter ( id_id , dfi_nfilter )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_nfilter
  INTEGER id_id
  dfi_nfilter = model_config_rec%dfi_nfilter
  RETURN
END SUBROUTINE nl_get_dfi_nfilter
SUBROUTINE nl_get_dfi_write_filtered_input ( id_id , dfi_write_filtered_input )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: dfi_write_filtered_input
  INTEGER id_id
  dfi_write_filtered_input = model_config_rec%dfi_write_filtered_input
  RETURN
END SUBROUTINE nl_get_dfi_write_filtered_input
SUBROUTINE nl_get_dfi_write_dfi_history ( id_id , dfi_write_dfi_history )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: dfi_write_dfi_history
  INTEGER id_id
  dfi_write_dfi_history = model_config_rec%dfi_write_dfi_history
  RETURN
END SUBROUTINE nl_get_dfi_write_dfi_history
SUBROUTINE nl_get_dfi_cutoff_seconds ( id_id , dfi_cutoff_seconds )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_cutoff_seconds
  INTEGER id_id
  dfi_cutoff_seconds = model_config_rec%dfi_cutoff_seconds
  RETURN
END SUBROUTINE nl_get_dfi_cutoff_seconds
SUBROUTINE nl_get_dfi_time_dim ( id_id , dfi_time_dim )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_time_dim
  INTEGER id_id
  dfi_time_dim = model_config_rec%dfi_time_dim
  RETURN
END SUBROUTINE nl_get_dfi_time_dim
SUBROUTINE nl_get_dfi_fwdstop_year ( id_id , dfi_fwdstop_year )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_fwdstop_year
  INTEGER id_id
  dfi_fwdstop_year = model_config_rec%dfi_fwdstop_year
  RETURN
END SUBROUTINE nl_get_dfi_fwdstop_year
SUBROUTINE nl_get_dfi_fwdstop_month ( id_id , dfi_fwdstop_month )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_fwdstop_month
  INTEGER id_id
  dfi_fwdstop_month = model_config_rec%dfi_fwdstop_month
  RETURN
END SUBROUTINE nl_get_dfi_fwdstop_month
SUBROUTINE nl_get_dfi_fwdstop_day ( id_id , dfi_fwdstop_day )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_fwdstop_day
  INTEGER id_id
  dfi_fwdstop_day = model_config_rec%dfi_fwdstop_day
  RETURN
END SUBROUTINE nl_get_dfi_fwdstop_day
SUBROUTINE nl_get_dfi_fwdstop_hour ( id_id , dfi_fwdstop_hour )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_fwdstop_hour
  INTEGER id_id
  dfi_fwdstop_hour = model_config_rec%dfi_fwdstop_hour
  RETURN
END SUBROUTINE nl_get_dfi_fwdstop_hour
SUBROUTINE nl_get_dfi_fwdstop_minute ( id_id , dfi_fwdstop_minute )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_fwdstop_minute
  INTEGER id_id
  dfi_fwdstop_minute = model_config_rec%dfi_fwdstop_minute
  RETURN
END SUBROUTINE nl_get_dfi_fwdstop_minute
SUBROUTINE nl_get_dfi_fwdstop_second ( id_id , dfi_fwdstop_second )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_fwdstop_second
  INTEGER id_id
  dfi_fwdstop_second = model_config_rec%dfi_fwdstop_second
  RETURN
END SUBROUTINE nl_get_dfi_fwdstop_second
SUBROUTINE nl_get_dfi_bckstop_year ( id_id , dfi_bckstop_year )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_bckstop_year
  INTEGER id_id
  dfi_bckstop_year = model_config_rec%dfi_bckstop_year
  RETURN
END SUBROUTINE nl_get_dfi_bckstop_year
SUBROUTINE nl_get_dfi_bckstop_month ( id_id , dfi_bckstop_month )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_bckstop_month
  INTEGER id_id
  dfi_bckstop_month = model_config_rec%dfi_bckstop_month
  RETURN
END SUBROUTINE nl_get_dfi_bckstop_month
SUBROUTINE nl_get_dfi_bckstop_day ( id_id , dfi_bckstop_day )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_bckstop_day
  INTEGER id_id
  dfi_bckstop_day = model_config_rec%dfi_bckstop_day
  RETURN
END SUBROUTINE nl_get_dfi_bckstop_day
SUBROUTINE nl_get_dfi_bckstop_hour ( id_id , dfi_bckstop_hour )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_bckstop_hour
  INTEGER id_id
  dfi_bckstop_hour = model_config_rec%dfi_bckstop_hour
  RETURN
END SUBROUTINE nl_get_dfi_bckstop_hour
SUBROUTINE nl_get_dfi_bckstop_minute ( id_id , dfi_bckstop_minute )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_bckstop_minute
  INTEGER id_id
  dfi_bckstop_minute = model_config_rec%dfi_bckstop_minute
  RETURN
END SUBROUTINE nl_get_dfi_bckstop_minute
SUBROUTINE nl_get_dfi_bckstop_second ( id_id , dfi_bckstop_second )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_bckstop_second
  INTEGER id_id
  dfi_bckstop_second = model_config_rec%dfi_bckstop_second
  RETURN
END SUBROUTINE nl_get_dfi_bckstop_second
SUBROUTINE nl_get_time_step ( id_id , time_step )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: time_step
  INTEGER id_id
  time_step = model_config_rec%time_step
  RETURN
END SUBROUTINE nl_get_time_step
SUBROUTINE nl_get_time_step_fract_num ( id_id , time_step_fract_num )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: time_step_fract_num
  INTEGER id_id
  time_step_fract_num = model_config_rec%time_step_fract_num
  RETURN
END SUBROUTINE nl_get_time_step_fract_num
SUBROUTINE nl_get_time_step_fract_den ( id_id , time_step_fract_den )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: time_step_fract_den
  INTEGER id_id
  time_step_fract_den = model_config_rec%time_step_fract_den
  RETURN
END SUBROUTINE nl_get_time_step_fract_den
SUBROUTINE nl_get_time_step_dfi ( id_id , time_step_dfi )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: time_step_dfi
  INTEGER id_id
  time_step_dfi = model_config_rec%time_step_dfi
  RETURN
END SUBROUTINE nl_get_time_step_dfi
SUBROUTINE nl_get_reasonable_time_step_ratio ( id_id , reasonable_time_step_ratio )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: reasonable_time_step_ratio
  INTEGER id_id
  reasonable_time_step_ratio = model_config_rec%reasonable_time_step_ratio
  RETURN
END SUBROUTINE nl_get_reasonable_time_step_ratio
SUBROUTINE nl_get_min_time_step ( id_id , min_time_step )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: min_time_step
  INTEGER id_id
  min_time_step = model_config_rec%min_time_step(id_id)
  RETURN
END SUBROUTINE nl_get_min_time_step
SUBROUTINE nl_get_min_time_step_den ( id_id , min_time_step_den )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: min_time_step_den
  INTEGER id_id
  min_time_step_den = model_config_rec%min_time_step_den(id_id)
  RETURN
END SUBROUTINE nl_get_min_time_step_den
SUBROUTINE nl_get_max_time_step ( id_id , max_time_step )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: max_time_step
  INTEGER id_id
  max_time_step = model_config_rec%max_time_step(id_id)
  RETURN
END SUBROUTINE nl_get_max_time_step
SUBROUTINE nl_get_max_time_step_den ( id_id , max_time_step_den )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: max_time_step_den
  INTEGER id_id
  max_time_step_den = model_config_rec%max_time_step_den(id_id)
  RETURN
END SUBROUTINE nl_get_max_time_step_den
SUBROUTINE nl_get_target_cfl ( id_id , target_cfl )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: target_cfl
  INTEGER id_id
  target_cfl = model_config_rec%target_cfl(id_id)
  RETURN
END SUBROUTINE nl_get_target_cfl
SUBROUTINE nl_get_target_hcfl ( id_id , target_hcfl )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: target_hcfl
  INTEGER id_id
  target_hcfl = model_config_rec%target_hcfl(id_id)
  RETURN
END SUBROUTINE nl_get_target_hcfl
SUBROUTINE nl_get_max_step_increase_pct ( id_id , max_step_increase_pct )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: max_step_increase_pct
  INTEGER id_id
  max_step_increase_pct = model_config_rec%max_step_increase_pct(id_id)
  RETURN
END SUBROUTINE nl_get_max_step_increase_pct
SUBROUTINE nl_get_starting_time_step ( id_id , starting_time_step )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: starting_time_step
  INTEGER id_id
  starting_time_step = model_config_rec%starting_time_step(id_id)
  RETURN
END SUBROUTINE nl_get_starting_time_step
SUBROUTINE nl_get_starting_time_step_den ( id_id , starting_time_step_den )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: starting_time_step_den
  INTEGER id_id
  starting_time_step_den = model_config_rec%starting_time_step_den(id_id)
  RETURN
END SUBROUTINE nl_get_starting_time_step_den
SUBROUTINE nl_get_step_to_output_time ( id_id , step_to_output_time )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: step_to_output_time
  INTEGER id_id
  step_to_output_time = model_config_rec%step_to_output_time
  RETURN
END SUBROUTINE nl_get_step_to_output_time
SUBROUTINE nl_get_adaptation_domain ( id_id , adaptation_domain )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: adaptation_domain
  INTEGER id_id
  adaptation_domain = model_config_rec%adaptation_domain
  RETURN
END SUBROUTINE nl_get_adaptation_domain
SUBROUTINE nl_get_use_adaptive_time_step ( id_id , use_adaptive_time_step )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: use_adaptive_time_step
  INTEGER id_id
  use_adaptive_time_step = model_config_rec%use_adaptive_time_step
  RETURN
END SUBROUTINE nl_get_use_adaptive_time_step
SUBROUTINE nl_get_use_adaptive_time_step_dfi ( id_id , use_adaptive_time_step_dfi )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: use_adaptive_time_step_dfi
  INTEGER id_id
  use_adaptive_time_step_dfi = model_config_rec%use_adaptive_time_step_dfi
  RETURN
END SUBROUTINE nl_get_use_adaptive_time_step_dfi
SUBROUTINE nl_get_max_dom ( id_id , max_dom )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: max_dom
  INTEGER id_id
  max_dom = model_config_rec%max_dom
  RETURN
END SUBROUTINE nl_get_max_dom
SUBROUTINE nl_get_lats_to_mic ( id_id , lats_to_mic )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: lats_to_mic
  INTEGER id_id
  lats_to_mic = model_config_rec%lats_to_mic
  RETURN
END SUBROUTINE nl_get_lats_to_mic
SUBROUTINE nl_get_s_we ( id_id , s_we )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: s_we
  INTEGER id_id
  s_we = model_config_rec%s_we(id_id)
  RETURN
END SUBROUTINE nl_get_s_we
SUBROUTINE nl_get_e_we ( id_id , e_we )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: e_we
  INTEGER id_id
  e_we = model_config_rec%e_we(id_id)
  RETURN
END SUBROUTINE nl_get_e_we
SUBROUTINE nl_get_s_sn ( id_id , s_sn )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: s_sn
  INTEGER id_id
  s_sn = model_config_rec%s_sn(id_id)
  RETURN
END SUBROUTINE nl_get_s_sn
SUBROUTINE nl_get_e_sn ( id_id , e_sn )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: e_sn
  INTEGER id_id
  e_sn = model_config_rec%e_sn(id_id)
  RETURN
END SUBROUTINE nl_get_e_sn
SUBROUTINE nl_get_s_vert ( id_id , s_vert )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: s_vert
  INTEGER id_id
  s_vert = model_config_rec%s_vert(id_id)
  RETURN
END SUBROUTINE nl_get_s_vert
SUBROUTINE nl_get_e_vert ( id_id , e_vert )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: e_vert
  INTEGER id_id
  e_vert = model_config_rec%e_vert(id_id)
  RETURN
END SUBROUTINE nl_get_e_vert
SUBROUTINE nl_get_num_metgrid_levels ( id_id , num_metgrid_levels )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: num_metgrid_levels
  INTEGER id_id
  num_metgrid_levels = model_config_rec%num_metgrid_levels
  RETURN
END SUBROUTINE nl_get_num_metgrid_levels
SUBROUTINE nl_get_num_metgrid_soil_levels ( id_id , num_metgrid_soil_levels )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: num_metgrid_soil_levels
  INTEGER id_id
  num_metgrid_soil_levels = model_config_rec%num_metgrid_soil_levels
  RETURN
END SUBROUTINE nl_get_num_metgrid_soil_levels
SUBROUTINE nl_get_p_top_requested ( id_id , p_top_requested )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: p_top_requested
  INTEGER id_id
  p_top_requested = model_config_rec%p_top_requested
  RETURN
END SUBROUTINE nl_get_p_top_requested
SUBROUTINE nl_get_interp_theta ( id_id , interp_theta )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: interp_theta
  INTEGER id_id
  interp_theta = model_config_rec%interp_theta
  RETURN
END SUBROUTINE nl_get_interp_theta
SUBROUTINE nl_get_interp_type ( id_id , interp_type )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: interp_type
  INTEGER id_id
  interp_type = model_config_rec%interp_type
  RETURN
END SUBROUTINE nl_get_interp_type
SUBROUTINE nl_get_rebalance ( id_id , rebalance )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: rebalance
  INTEGER id_id
  rebalance = model_config_rec%rebalance
  RETURN
END SUBROUTINE nl_get_rebalance
SUBROUTINE nl_get_vert_refine_method ( id_id , vert_refine_method )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: vert_refine_method
  INTEGER id_id
  vert_refine_method = model_config_rec%vert_refine_method(id_id)
  RETURN
END SUBROUTINE nl_get_vert_refine_method
SUBROUTINE nl_get_vert_refine_fact ( id_id , vert_refine_fact )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: vert_refine_fact
  INTEGER id_id
  vert_refine_fact = model_config_rec%vert_refine_fact
  RETURN
END SUBROUTINE nl_get_vert_refine_fact
SUBROUTINE nl_get_extrap_type ( id_id , extrap_type )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: extrap_type
  INTEGER id_id
  extrap_type = model_config_rec%extrap_type
  RETURN
END SUBROUTINE nl_get_extrap_type
SUBROUTINE nl_get_t_extrap_type ( id_id , t_extrap_type )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: t_extrap_type
  INTEGER id_id
  t_extrap_type = model_config_rec%t_extrap_type
  RETURN
END SUBROUTINE nl_get_t_extrap_type
SUBROUTINE nl_get_hypsometric_opt ( id_id , hypsometric_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: hypsometric_opt
  INTEGER id_id
  hypsometric_opt = model_config_rec%hypsometric_opt
  RETURN
END SUBROUTINE nl_get_hypsometric_opt
SUBROUTINE nl_get_lowest_lev_from_sfc ( id_id , lowest_lev_from_sfc )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: lowest_lev_from_sfc
  INTEGER id_id
  lowest_lev_from_sfc = model_config_rec%lowest_lev_from_sfc
  RETURN
END SUBROUTINE nl_get_lowest_lev_from_sfc
SUBROUTINE nl_get_use_levels_below_ground ( id_id , use_levels_below_ground )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: use_levels_below_ground
  INTEGER id_id
  use_levels_below_ground = model_config_rec%use_levels_below_ground
  RETURN
END SUBROUTINE nl_get_use_levels_below_ground
SUBROUTINE nl_get_use_tavg_for_tsk ( id_id , use_tavg_for_tsk )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: use_tavg_for_tsk
  INTEGER id_id
  use_tavg_for_tsk = model_config_rec%use_tavg_for_tsk
  RETURN
END SUBROUTINE nl_get_use_tavg_for_tsk
SUBROUTINE nl_get_use_surface ( id_id , use_surface )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: use_surface
  INTEGER id_id
  use_surface = model_config_rec%use_surface
  RETURN
END SUBROUTINE nl_get_use_surface
SUBROUTINE nl_get_lagrange_order ( id_id , lagrange_order )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: lagrange_order
  INTEGER id_id
  lagrange_order = model_config_rec%lagrange_order
  RETURN
END SUBROUTINE nl_get_lagrange_order
SUBROUTINE nl_get_linear_interp ( id_id , linear_interp )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: linear_interp
  INTEGER id_id
  linear_interp = model_config_rec%linear_interp
  RETURN
END SUBROUTINE nl_get_linear_interp
SUBROUTINE nl_get_force_sfc_in_vinterp ( id_id , force_sfc_in_vinterp )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: force_sfc_in_vinterp
  INTEGER id_id
  force_sfc_in_vinterp = model_config_rec%force_sfc_in_vinterp
  RETURN
END SUBROUTINE nl_get_force_sfc_in_vinterp
SUBROUTINE nl_get_zap_close_levels ( id_id , zap_close_levels )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: zap_close_levels
  INTEGER id_id
  zap_close_levels = model_config_rec%zap_close_levels
  RETURN
END SUBROUTINE nl_get_zap_close_levels
SUBROUTINE nl_get_maxw_horiz_pres_diff ( id_id , maxw_horiz_pres_diff )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: maxw_horiz_pres_diff
  INTEGER id_id
  maxw_horiz_pres_diff = model_config_rec%maxw_horiz_pres_diff
  RETURN
END SUBROUTINE nl_get_maxw_horiz_pres_diff
SUBROUTINE nl_get_trop_horiz_pres_diff ( id_id , trop_horiz_pres_diff )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: trop_horiz_pres_diff
  INTEGER id_id
  trop_horiz_pres_diff = model_config_rec%trop_horiz_pres_diff
  RETURN
END SUBROUTINE nl_get_trop_horiz_pres_diff
SUBROUTINE nl_get_maxw_above_this_level ( id_id , maxw_above_this_level )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: maxw_above_this_level
  INTEGER id_id
  maxw_above_this_level = model_config_rec%maxw_above_this_level
  RETURN
END SUBROUTINE nl_get_maxw_above_this_level
SUBROUTINE nl_get_use_maxw_level ( id_id , use_maxw_level )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: use_maxw_level
  INTEGER id_id
  use_maxw_level = model_config_rec%use_maxw_level
  RETURN
END SUBROUTINE nl_get_use_maxw_level
SUBROUTINE nl_get_use_trop_level ( id_id , use_trop_level )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: use_trop_level
  INTEGER id_id
  use_trop_level = model_config_rec%use_trop_level
  RETURN
END SUBROUTINE nl_get_use_trop_level
SUBROUTINE nl_get_sfcp_to_sfcp ( id_id , sfcp_to_sfcp )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: sfcp_to_sfcp
  INTEGER id_id
  sfcp_to_sfcp = model_config_rec%sfcp_to_sfcp
  RETURN
END SUBROUTINE nl_get_sfcp_to_sfcp
SUBROUTINE nl_get_adjust_heights ( id_id , adjust_heights )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: adjust_heights
  INTEGER id_id
  adjust_heights = model_config_rec%adjust_heights
  RETURN
END SUBROUTINE nl_get_adjust_heights
SUBROUTINE nl_get_smooth_cg_topo ( id_id , smooth_cg_topo )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: smooth_cg_topo
  INTEGER id_id
  smooth_cg_topo = model_config_rec%smooth_cg_topo
  RETURN
END SUBROUTINE nl_get_smooth_cg_topo
SUBROUTINE nl_get_nest_interp_coord ( id_id , nest_interp_coord )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: nest_interp_coord
  INTEGER id_id
  nest_interp_coord = model_config_rec%nest_interp_coord
  RETURN
END SUBROUTINE nl_get_nest_interp_coord
SUBROUTINE nl_get_interp_method_type ( id_id , interp_method_type )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: interp_method_type
  INTEGER id_id
  interp_method_type = model_config_rec%interp_method_type
  RETURN
END SUBROUTINE nl_get_interp_method_type
SUBROUTINE nl_get_aggregate_lu ( id_id , aggregate_lu )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: aggregate_lu
  INTEGER id_id
  aggregate_lu = model_config_rec%aggregate_lu
  RETURN
END SUBROUTINE nl_get_aggregate_lu
SUBROUTINE nl_get_rh2qv_wrt_liquid ( id_id , rh2qv_wrt_liquid )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: rh2qv_wrt_liquid
  INTEGER id_id
  rh2qv_wrt_liquid = model_config_rec%rh2qv_wrt_liquid
  RETURN
END SUBROUTINE nl_get_rh2qv_wrt_liquid
SUBROUTINE nl_get_rh2qv_method ( id_id , rh2qv_method )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: rh2qv_method
  INTEGER id_id
  rh2qv_method = model_config_rec%rh2qv_method
  RETURN
END SUBROUTINE nl_get_rh2qv_method
SUBROUTINE nl_get_qv_max_p_safe ( id_id , qv_max_p_safe )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: qv_max_p_safe
  INTEGER id_id
  qv_max_p_safe = model_config_rec%qv_max_p_safe
  RETURN
END SUBROUTINE nl_get_qv_max_p_safe
SUBROUTINE nl_get_qv_max_flag ( id_id , qv_max_flag )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: qv_max_flag
  INTEGER id_id
  qv_max_flag = model_config_rec%qv_max_flag
  RETURN
END SUBROUTINE nl_get_qv_max_flag
SUBROUTINE nl_get_qv_max_value ( id_id , qv_max_value )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: qv_max_value
  INTEGER id_id
  qv_max_value = model_config_rec%qv_max_value
  RETURN
END SUBROUTINE nl_get_qv_max_value
SUBROUTINE nl_get_qv_min_p_safe ( id_id , qv_min_p_safe )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: qv_min_p_safe
  INTEGER id_id
  qv_min_p_safe = model_config_rec%qv_min_p_safe
  RETURN
END SUBROUTINE nl_get_qv_min_p_safe
SUBROUTINE nl_get_qv_min_flag ( id_id , qv_min_flag )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: qv_min_flag
  INTEGER id_id
  qv_min_flag = model_config_rec%qv_min_flag
  RETURN
END SUBROUTINE nl_get_qv_min_flag
SUBROUTINE nl_get_qv_min_value ( id_id , qv_min_value )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: qv_min_value
  INTEGER id_id
  qv_min_value = model_config_rec%qv_min_value
  RETURN
END SUBROUTINE nl_get_qv_min_value
SUBROUTINE nl_get_ideal_init_method ( id_id , ideal_init_method )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: ideal_init_method
  INTEGER id_id
  ideal_init_method = model_config_rec%ideal_init_method
  RETURN
END SUBROUTINE nl_get_ideal_init_method
SUBROUTINE nl_get_dx ( id_id , dx )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: dx
  INTEGER id_id
  dx = model_config_rec%dx(id_id)
  RETURN
END SUBROUTINE nl_get_dx
SUBROUTINE nl_get_dy ( id_id , dy )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: dy
  INTEGER id_id
  dy = model_config_rec%dy(id_id)
  RETURN
END SUBROUTINE nl_get_dy
SUBROUTINE nl_get_grid_id ( id_id , grid_id )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: grid_id
  INTEGER id_id
  grid_id = model_config_rec%grid_id(id_id)
  RETURN
END SUBROUTINE nl_get_grid_id
SUBROUTINE nl_get_grid_allowed ( id_id , grid_allowed )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: grid_allowed
  INTEGER id_id
  grid_allowed = model_config_rec%grid_allowed(id_id)
  RETURN
END SUBROUTINE nl_get_grid_allowed
SUBROUTINE nl_get_parent_id ( id_id , parent_id )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: parent_id
  INTEGER id_id
  parent_id = model_config_rec%parent_id(id_id)
  RETURN
END SUBROUTINE nl_get_parent_id
SUBROUTINE nl_get_i_parent_start ( id_id , i_parent_start )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: i_parent_start
  INTEGER id_id
  i_parent_start = model_config_rec%i_parent_start(id_id)
  RETURN
END SUBROUTINE nl_get_i_parent_start
SUBROUTINE nl_get_j_parent_start ( id_id , j_parent_start )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: j_parent_start
  INTEGER id_id
  j_parent_start = model_config_rec%j_parent_start(id_id)
  RETURN
END SUBROUTINE nl_get_j_parent_start
SUBROUTINE nl_get_parent_grid_ratio ( id_id , parent_grid_ratio )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: parent_grid_ratio
  INTEGER id_id
  parent_grid_ratio = model_config_rec%parent_grid_ratio(id_id)
  RETURN
END SUBROUTINE nl_get_parent_grid_ratio
SUBROUTINE nl_get_parent_time_step_ratio ( id_id , parent_time_step_ratio )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: parent_time_step_ratio
  INTEGER id_id
  parent_time_step_ratio = model_config_rec%parent_time_step_ratio(id_id)
  RETURN
END SUBROUTINE nl_get_parent_time_step_ratio
SUBROUTINE nl_get_feedback ( id_id , feedback )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: feedback
  INTEGER id_id
  feedback = model_config_rec%feedback
  RETURN
END SUBROUTINE nl_get_feedback
SUBROUTINE nl_get_smooth_option ( id_id , smooth_option )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: smooth_option
  INTEGER id_id
  smooth_option = model_config_rec%smooth_option
  RETURN
END SUBROUTINE nl_get_smooth_option
SUBROUTINE nl_get_blend_width ( id_id , blend_width )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: blend_width
  INTEGER id_id
  blend_width = model_config_rec%blend_width
  RETURN
END SUBROUTINE nl_get_blend_width
SUBROUTINE nl_get_ztop ( id_id , ztop )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: ztop
  INTEGER id_id
  ztop = model_config_rec%ztop(id_id)
  RETURN
END SUBROUTINE nl_get_ztop
SUBROUTINE nl_get_moad_grid_ratio ( id_id , moad_grid_ratio )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: moad_grid_ratio
  INTEGER id_id
  moad_grid_ratio = model_config_rec%moad_grid_ratio(id_id)
  RETURN
END SUBROUTINE nl_get_moad_grid_ratio
SUBROUTINE nl_get_moad_time_step_ratio ( id_id , moad_time_step_ratio )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: moad_time_step_ratio
  INTEGER id_id
  moad_time_step_ratio = model_config_rec%moad_time_step_ratio(id_id)
  RETURN
END SUBROUTINE nl_get_moad_time_step_ratio
SUBROUTINE nl_get_shw ( id_id , shw )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: shw
  INTEGER id_id
  shw = model_config_rec%shw(id_id)
  RETURN
END SUBROUTINE nl_get_shw
SUBROUTINE nl_get_tile_sz_x ( id_id , tile_sz_x )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: tile_sz_x
  INTEGER id_id
  tile_sz_x = model_config_rec%tile_sz_x
  RETURN
END SUBROUTINE nl_get_tile_sz_x
SUBROUTINE nl_get_tile_sz_y ( id_id , tile_sz_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: tile_sz_y
  INTEGER id_id
  tile_sz_y = model_config_rec%tile_sz_y
  RETURN
END SUBROUTINE nl_get_tile_sz_y
SUBROUTINE nl_get_numtiles ( id_id , numtiles )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: numtiles
  INTEGER id_id
  numtiles = model_config_rec%numtiles
  RETURN
END SUBROUTINE nl_get_numtiles
SUBROUTINE nl_get_numtiles_inc ( id_id , numtiles_inc )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: numtiles_inc
  INTEGER id_id
  numtiles_inc = model_config_rec%numtiles_inc
  RETURN
END SUBROUTINE nl_get_numtiles_inc
SUBROUTINE nl_get_numtiles_x ( id_id , numtiles_x )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: numtiles_x
  INTEGER id_id
  numtiles_x = model_config_rec%numtiles_x
  RETURN
END SUBROUTINE nl_get_numtiles_x
SUBROUTINE nl_get_numtiles_y ( id_id , numtiles_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: numtiles_y
  INTEGER id_id
  numtiles_y = model_config_rec%numtiles_y
  RETURN
END SUBROUTINE nl_get_numtiles_y
SUBROUTINE nl_get_tile_strategy ( id_id , tile_strategy )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: tile_strategy
  INTEGER id_id
  tile_strategy = model_config_rec%tile_strategy
  RETURN
END SUBROUTINE nl_get_tile_strategy
SUBROUTINE nl_get_nproc_x ( id_id , nproc_x )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: nproc_x
  INTEGER id_id
  nproc_x = model_config_rec%nproc_x
  RETURN
END SUBROUTINE nl_get_nproc_x
SUBROUTINE nl_get_nproc_y ( id_id , nproc_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: nproc_y
  INTEGER id_id
  nproc_y = model_config_rec%nproc_y
  RETURN
END SUBROUTINE nl_get_nproc_y
SUBROUTINE nl_get_irand ( id_id , irand )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: irand
  INTEGER id_id
  irand = model_config_rec%irand
  RETURN
END SUBROUTINE nl_get_irand
SUBROUTINE nl_get_dt ( id_id , dt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: dt
  INTEGER id_id
  dt = model_config_rec%dt(id_id)
  RETURN
END SUBROUTINE nl_get_dt
SUBROUTINE nl_get_fft_used ( id_id , fft_used )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: fft_used
  INTEGER id_id
  fft_used = model_config_rec%fft_used
  RETURN
END SUBROUTINE nl_get_fft_used
SUBROUTINE nl_get_cu_used ( id_id , cu_used )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: cu_used
  INTEGER id_id
  cu_used = model_config_rec%cu_used
  RETURN
END SUBROUTINE nl_get_cu_used
SUBROUTINE nl_get_shcu_used ( id_id , shcu_used )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: shcu_used
  INTEGER id_id
  shcu_used = model_config_rec%shcu_used
  RETURN
END SUBROUTINE nl_get_shcu_used
SUBROUTINE nl_get_cam_used ( id_id , cam_used )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: cam_used
  INTEGER id_id
  cam_used = model_config_rec%cam_used
  RETURN
END SUBROUTINE nl_get_cam_used
SUBROUTINE nl_get_gwd_used ( id_id , gwd_used )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: gwd_used
  INTEGER id_id
  gwd_used = model_config_rec%gwd_used
  RETURN
END SUBROUTINE nl_get_gwd_used
SUBROUTINE nl_get_alloc_qndropsource ( id_id , alloc_qndropsource )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: alloc_qndropsource
  INTEGER id_id
  alloc_qndropsource = model_config_rec%alloc_qndropsource
  RETURN
END SUBROUTINE nl_get_alloc_qndropsource
SUBROUTINE nl_get_num_moves ( id_id , num_moves )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: num_moves
  INTEGER id_id
  num_moves = model_config_rec%num_moves
  RETURN
END SUBROUTINE nl_get_num_moves
SUBROUTINE nl_get_ts_buf_size ( id_id , ts_buf_size )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: ts_buf_size
  INTEGER id_id
  ts_buf_size = model_config_rec%ts_buf_size
  RETURN
END SUBROUTINE nl_get_ts_buf_size
SUBROUTINE nl_get_max_ts_locs ( id_id , max_ts_locs )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: max_ts_locs
  INTEGER id_id
  max_ts_locs = model_config_rec%max_ts_locs
  RETURN
END SUBROUTINE nl_get_max_ts_locs
SUBROUTINE nl_get_tslist_ij ( id_id , tslist_ij )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: tslist_ij
  INTEGER id_id
  tslist_ij = model_config_rec%tslist_ij
  RETURN
END SUBROUTINE nl_get_tslist_ij
SUBROUTINE nl_get_tslist_unstagger_winds ( id_id , tslist_unstagger_winds )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: tslist_unstagger_winds
  INTEGER id_id
  tslist_unstagger_winds = model_config_rec%tslist_unstagger_winds
  RETURN
END SUBROUTINE nl_get_tslist_unstagger_winds
SUBROUTINE nl_get_vortex_interval ( id_id , vortex_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: vortex_interval
  INTEGER id_id
  vortex_interval = model_config_rec%vortex_interval(id_id)
  RETURN
END SUBROUTINE nl_get_vortex_interval
SUBROUTINE nl_get_max_vortex_speed ( id_id , max_vortex_speed )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: max_vortex_speed
  INTEGER id_id
  max_vortex_speed = model_config_rec%max_vortex_speed(id_id)
  RETURN
END SUBROUTINE nl_get_max_vortex_speed
SUBROUTINE nl_get_corral_dist ( id_id , corral_dist )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: corral_dist
  INTEGER id_id
  corral_dist = model_config_rec%corral_dist(id_id)
  RETURN
END SUBROUTINE nl_get_corral_dist
SUBROUTINE nl_get_track_level ( id_id , track_level )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: track_level
  INTEGER id_id
  track_level = model_config_rec%track_level
  RETURN
END SUBROUTINE nl_get_track_level
SUBROUTINE nl_get_time_to_move ( id_id , time_to_move )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: time_to_move
  INTEGER id_id
  time_to_move = model_config_rec%time_to_move(id_id)
  RETURN
END SUBROUTINE nl_get_time_to_move
SUBROUTINE nl_get_move_id ( id_id , move_id )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: move_id
  INTEGER id_id
  move_id = model_config_rec%move_id(id_id)
  RETURN
END SUBROUTINE nl_get_move_id
SUBROUTINE nl_get_move_interval ( id_id , move_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: move_interval
  INTEGER id_id
  move_interval = model_config_rec%move_interval(id_id)
  RETURN
END SUBROUTINE nl_get_move_interval
SUBROUTINE nl_get_move_cd_x ( id_id , move_cd_x )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: move_cd_x
  INTEGER id_id
  move_cd_x = model_config_rec%move_cd_x(id_id)
  RETURN
END SUBROUTINE nl_get_move_cd_x
SUBROUTINE nl_get_move_cd_y ( id_id , move_cd_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: move_cd_y
  INTEGER id_id
  move_cd_y = model_config_rec%move_cd_y(id_id)
  RETURN
END SUBROUTINE nl_get_move_cd_y
SUBROUTINE nl_get_swap_x ( id_id , swap_x )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: swap_x
  INTEGER id_id
  swap_x = model_config_rec%swap_x(id_id)
  RETURN
END SUBROUTINE nl_get_swap_x
SUBROUTINE nl_get_swap_y ( id_id , swap_y )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: swap_y
  INTEGER id_id
  swap_y = model_config_rec%swap_y(id_id)
  RETURN
END SUBROUTINE nl_get_swap_y
SUBROUTINE nl_get_cycle_x ( id_id , cycle_x )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: cycle_x
  INTEGER id_id
  cycle_x = model_config_rec%cycle_x(id_id)
  RETURN
END SUBROUTINE nl_get_cycle_x
SUBROUTINE nl_get_cycle_y ( id_id , cycle_y )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: cycle_y
  INTEGER id_id
  cycle_y = model_config_rec%cycle_y(id_id)
  RETURN
END SUBROUTINE nl_get_cycle_y
SUBROUTINE nl_get_reorder_mesh ( id_id , reorder_mesh )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: reorder_mesh
  INTEGER id_id
  reorder_mesh = model_config_rec%reorder_mesh
  RETURN
END SUBROUTINE nl_get_reorder_mesh
SUBROUTINE nl_get_perturb_input ( id_id , perturb_input )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: perturb_input
  INTEGER id_id
  perturb_input = model_config_rec%perturb_input
  RETURN
END SUBROUTINE nl_get_perturb_input
SUBROUTINE nl_get_eta_levels ( id_id , eta_levels )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: eta_levels
  INTEGER id_id
  eta_levels = model_config_rec%eta_levels(id_id)
  RETURN
END SUBROUTINE nl_get_eta_levels
SUBROUTINE nl_get_auto_levels_opt ( id_id , auto_levels_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auto_levels_opt
  INTEGER id_id
  auto_levels_opt = model_config_rec%auto_levels_opt
  RETURN
END SUBROUTINE nl_get_auto_levels_opt
SUBROUTINE nl_get_max_dz ( id_id , max_dz )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: max_dz
  INTEGER id_id
  max_dz = model_config_rec%max_dz
  RETURN
END SUBROUTINE nl_get_max_dz
SUBROUTINE nl_get_dzbot ( id_id , dzbot )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: dzbot
  INTEGER id_id
  dzbot = model_config_rec%dzbot
  RETURN
END SUBROUTINE nl_get_dzbot
SUBROUTINE nl_get_dzstretch_s ( id_id , dzstretch_s )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: dzstretch_s
  INTEGER id_id
  dzstretch_s = model_config_rec%dzstretch_s
  RETURN
END SUBROUTINE nl_get_dzstretch_s
SUBROUTINE nl_get_dzstretch_u ( id_id , dzstretch_u )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: dzstretch_u
  INTEGER id_id
  dzstretch_u = model_config_rec%dzstretch_u
  RETURN
END SUBROUTINE nl_get_dzstretch_u
SUBROUTINE nl_get_ocean_levels ( id_id , ocean_levels )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: ocean_levels
  INTEGER id_id
  ocean_levels = model_config_rec%ocean_levels
  RETURN
END SUBROUTINE nl_get_ocean_levels
SUBROUTINE nl_get_ocean_z ( id_id , ocean_z )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: ocean_z
  INTEGER id_id
  ocean_z = model_config_rec%ocean_z(id_id)
  RETURN
END SUBROUTINE nl_get_ocean_z
SUBROUTINE nl_get_ocean_t ( id_id , ocean_t )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: ocean_t
  INTEGER id_id
  ocean_t = model_config_rec%ocean_t(id_id)
  RETURN
END SUBROUTINE nl_get_ocean_t
SUBROUTINE nl_get_ocean_s ( id_id , ocean_s )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: ocean_s
  INTEGER id_id
  ocean_s = model_config_rec%ocean_s(id_id)
  RETURN
END SUBROUTINE nl_get_ocean_s
SUBROUTINE nl_get_num_traj ( id_id , num_traj )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: num_traj
  INTEGER id_id
  num_traj = model_config_rec%num_traj
  RETURN
END SUBROUTINE nl_get_num_traj
SUBROUTINE nl_get_max_ts_level ( id_id , max_ts_level )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: max_ts_level
  INTEGER id_id
  max_ts_level = model_config_rec%max_ts_level
  RETURN
END SUBROUTINE nl_get_max_ts_level
SUBROUTINE nl_get_track_loc_in ( id_id , track_loc_in )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: track_loc_in
  INTEGER id_id
  track_loc_in = model_config_rec%track_loc_in
  RETURN
END SUBROUTINE nl_get_track_loc_in
SUBROUTINE nl_get_num_ext_model_couple_dom ( id_id , num_ext_model_couple_dom )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: num_ext_model_couple_dom
  INTEGER id_id
  num_ext_model_couple_dom = model_config_rec%num_ext_model_couple_dom
  RETURN
END SUBROUTINE nl_get_num_ext_model_couple_dom
SUBROUTINE nl_get_insert_bogus_storm ( id_id , insert_bogus_storm )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: insert_bogus_storm
  INTEGER id_id
  insert_bogus_storm = model_config_rec%insert_bogus_storm
  RETURN
END SUBROUTINE nl_get_insert_bogus_storm
SUBROUTINE nl_get_remove_storm ( id_id , remove_storm )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: remove_storm
  INTEGER id_id
  remove_storm = model_config_rec%remove_storm
  RETURN
END SUBROUTINE nl_get_remove_storm
SUBROUTINE nl_get_num_storm ( id_id , num_storm )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: num_storm
  INTEGER id_id
  num_storm = model_config_rec%num_storm
  RETURN
END SUBROUTINE nl_get_num_storm
SUBROUTINE nl_get_latc_loc ( id_id , latc_loc )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: latc_loc
  INTEGER id_id
  latc_loc = model_config_rec%latc_loc(id_id)
  RETURN
END SUBROUTINE nl_get_latc_loc
SUBROUTINE nl_get_lonc_loc ( id_id , lonc_loc )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: lonc_loc
  INTEGER id_id
  lonc_loc = model_config_rec%lonc_loc(id_id)
  RETURN
END SUBROUTINE nl_get_lonc_loc
SUBROUTINE nl_get_vmax_meters_per_second ( id_id , vmax_meters_per_second )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: vmax_meters_per_second
  INTEGER id_id
  vmax_meters_per_second = model_config_rec%vmax_meters_per_second(id_id)
  RETURN
END SUBROUTINE nl_get_vmax_meters_per_second
SUBROUTINE nl_get_rmax ( id_id , rmax )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: rmax
  INTEGER id_id
  rmax = model_config_rec%rmax(id_id)
  RETURN
END SUBROUTINE nl_get_rmax
SUBROUTINE nl_get_vmax_ratio ( id_id , vmax_ratio )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: vmax_ratio
  INTEGER id_id
  vmax_ratio = model_config_rec%vmax_ratio(id_id)
  RETURN
END SUBROUTINE nl_get_vmax_ratio
SUBROUTINE nl_get_rankine_lid ( id_id , rankine_lid )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: rankine_lid
  INTEGER id_id
  rankine_lid = model_config_rec%rankine_lid
  RETURN
END SUBROUTINE nl_get_rankine_lid
SUBROUTINE nl_get_physics_suite ( id_id , physics_suite )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: physics_suite
  INTEGER id_id
  physics_suite = trim(model_config_rec%physics_suite)
  RETURN
END SUBROUTINE nl_get_physics_suite
SUBROUTINE nl_get_force_read_thompson ( id_id , force_read_thompson )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: force_read_thompson
  INTEGER id_id
  force_read_thompson = model_config_rec%force_read_thompson
  RETURN
END SUBROUTINE nl_get_force_read_thompson
SUBROUTINE nl_get_write_thompson_tables ( id_id , write_thompson_tables )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: write_thompson_tables
  INTEGER id_id
  write_thompson_tables = model_config_rec%write_thompson_tables
  RETURN
END SUBROUTINE nl_get_write_thompson_tables
SUBROUTINE nl_get_mp_physics ( id_id , mp_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: mp_physics
  INTEGER id_id
  mp_physics = model_config_rec%mp_physics(id_id)
  RETURN
END SUBROUTINE nl_get_mp_physics
SUBROUTINE nl_get_nssl_cccn ( id_id , nssl_cccn )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: nssl_cccn
  INTEGER id_id
  nssl_cccn = model_config_rec%nssl_cccn(id_id)
  RETURN
END SUBROUTINE nl_get_nssl_cccn
SUBROUTINE nl_get_nssl_alphah ( id_id , nssl_alphah )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: nssl_alphah
  INTEGER id_id
  nssl_alphah = model_config_rec%nssl_alphah(id_id)
  RETURN
END SUBROUTINE nl_get_nssl_alphah
SUBROUTINE nl_get_nssl_alphahl ( id_id , nssl_alphahl )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: nssl_alphahl
  INTEGER id_id
  nssl_alphahl = model_config_rec%nssl_alphahl(id_id)
  RETURN
END SUBROUTINE nl_get_nssl_alphahl
SUBROUTINE nl_get_nssl_cnoh ( id_id , nssl_cnoh )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: nssl_cnoh
  INTEGER id_id
  nssl_cnoh = model_config_rec%nssl_cnoh(id_id)
  RETURN
END SUBROUTINE nl_get_nssl_cnoh
SUBROUTINE nl_get_nssl_cnohl ( id_id , nssl_cnohl )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: nssl_cnohl
  INTEGER id_id
  nssl_cnohl = model_config_rec%nssl_cnohl(id_id)
  RETURN
END SUBROUTINE nl_get_nssl_cnohl
SUBROUTINE nl_get_nssl_cnor ( id_id , nssl_cnor )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: nssl_cnor
  INTEGER id_id
  nssl_cnor = model_config_rec%nssl_cnor(id_id)
  RETURN
END SUBROUTINE nl_get_nssl_cnor
SUBROUTINE nl_get_nssl_cnos ( id_id , nssl_cnos )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: nssl_cnos
  INTEGER id_id
  nssl_cnos = model_config_rec%nssl_cnos(id_id)
  RETURN
END SUBROUTINE nl_get_nssl_cnos
SUBROUTINE nl_get_nssl_rho_qh ( id_id , nssl_rho_qh )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: nssl_rho_qh
  INTEGER id_id
  nssl_rho_qh = model_config_rec%nssl_rho_qh(id_id)
  RETURN
END SUBROUTINE nl_get_nssl_rho_qh
SUBROUTINE nl_get_nssl_rho_qhl ( id_id , nssl_rho_qhl )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: nssl_rho_qhl
  INTEGER id_id
  nssl_rho_qhl = model_config_rec%nssl_rho_qhl(id_id)
  RETURN
END SUBROUTINE nl_get_nssl_rho_qhl
SUBROUTINE nl_get_nssl_rho_qs ( id_id , nssl_rho_qs )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: nssl_rho_qs
  INTEGER id_id
  nssl_rho_qs = model_config_rec%nssl_rho_qs(id_id)
  RETURN
END SUBROUTINE nl_get_nssl_rho_qs
SUBROUTINE nl_get_nudge_lightning ( id_id , nudge_lightning )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: nudge_lightning
  INTEGER id_id
  nudge_lightning = model_config_rec%nudge_lightning(id_id)
  RETURN
END SUBROUTINE nl_get_nudge_lightning
SUBROUTINE nl_get_nudge_light_times ( id_id , nudge_light_times )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: nudge_light_times
  INTEGER id_id
  nudge_light_times = model_config_rec%nudge_light_times(id_id)
  RETURN
END SUBROUTINE nl_get_nudge_light_times
SUBROUTINE nl_get_nudge_light_timee ( id_id , nudge_light_timee )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: nudge_light_timee
  INTEGER id_id
  nudge_light_timee = model_config_rec%nudge_light_timee(id_id)
  RETURN
END SUBROUTINE nl_get_nudge_light_timee
SUBROUTINE nl_get_nudge_light_int ( id_id , nudge_light_int )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: nudge_light_int
  INTEGER id_id
  nudge_light_int = model_config_rec%nudge_light_int(id_id)
  RETURN
END SUBROUTINE nl_get_nudge_light_int
SUBROUTINE nl_get_path_to_files ( id_id , path_to_files )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: path_to_files
  INTEGER id_id
  path_to_files = trim(model_config_rec%path_to_files)
  RETURN
END SUBROUTINE nl_get_path_to_files
SUBROUTINE nl_get_gsfcgce_hail ( id_id , gsfcgce_hail )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: gsfcgce_hail
  INTEGER id_id
  gsfcgce_hail = model_config_rec%gsfcgce_hail
  RETURN
END SUBROUTINE nl_get_gsfcgce_hail
SUBROUTINE nl_get_gsfcgce_2ice ( id_id , gsfcgce_2ice )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: gsfcgce_2ice
  INTEGER id_id
  gsfcgce_2ice = model_config_rec%gsfcgce_2ice
  RETURN
END SUBROUTINE nl_get_gsfcgce_2ice
SUBROUTINE nl_get_progn ( id_id , progn )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: progn
  INTEGER id_id
  progn = model_config_rec%progn(id_id)
  RETURN
END SUBROUTINE nl_get_progn
SUBROUTINE nl_get_accum_mode ( id_id , accum_mode )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: accum_mode
  INTEGER id_id
  accum_mode = model_config_rec%accum_mode
  RETURN
END SUBROUTINE nl_get_accum_mode
SUBROUTINE nl_get_aitken_mode ( id_id , aitken_mode )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: aitken_mode
  INTEGER id_id
  aitken_mode = model_config_rec%aitken_mode
  RETURN
END SUBROUTINE nl_get_aitken_mode
SUBROUTINE nl_get_coarse_mode ( id_id , coarse_mode )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: coarse_mode
  INTEGER id_id
  coarse_mode = model_config_rec%coarse_mode
  RETURN
END SUBROUTINE nl_get_coarse_mode
SUBROUTINE nl_get_do_radar_ref ( id_id , do_radar_ref )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: do_radar_ref
  INTEGER id_id
  do_radar_ref = model_config_rec%do_radar_ref
  RETURN
END SUBROUTINE nl_get_do_radar_ref
SUBROUTINE nl_get_compute_radar_ref ( id_id , compute_radar_ref )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: compute_radar_ref
  INTEGER id_id
  compute_radar_ref = model_config_rec%compute_radar_ref
  RETURN
END SUBROUTINE nl_get_compute_radar_ref
SUBROUTINE nl_get_ra_lw_physics ( id_id , ra_lw_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: ra_lw_physics
  INTEGER id_id
  ra_lw_physics = model_config_rec%ra_lw_physics(id_id)
  RETURN
END SUBROUTINE nl_get_ra_lw_physics
SUBROUTINE nl_get_ra_sw_physics ( id_id , ra_sw_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: ra_sw_physics
  INTEGER id_id
  ra_sw_physics = model_config_rec%ra_sw_physics(id_id)
  RETURN
END SUBROUTINE nl_get_ra_sw_physics
SUBROUTINE nl_get_radt ( id_id , radt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: radt
  INTEGER id_id
  radt = model_config_rec%radt(id_id)
  RETURN
END SUBROUTINE nl_get_radt
SUBROUTINE nl_get_naer ( id_id , naer )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: naer
  INTEGER id_id
  naer = model_config_rec%naer(id_id)
  RETURN
END SUBROUTINE nl_get_naer
SUBROUTINE nl_get_sf_sfclay_physics ( id_id , sf_sfclay_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sf_sfclay_physics
  INTEGER id_id
  sf_sfclay_physics = model_config_rec%sf_sfclay_physics(id_id)
  RETURN
END SUBROUTINE nl_get_sf_sfclay_physics
SUBROUTINE nl_get_sf_surf_irr_scheme ( id_id , sf_surf_irr_scheme )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sf_surf_irr_scheme
  INTEGER id_id
  sf_surf_irr_scheme = model_config_rec%sf_surf_irr_scheme(id_id)
  RETURN
END SUBROUTINE nl_get_sf_surf_irr_scheme
SUBROUTINE nl_get_sf_surf_irr_alloc ( id_id , sf_surf_irr_alloc )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sf_surf_irr_alloc
  INTEGER id_id
  sf_surf_irr_alloc = model_config_rec%sf_surf_irr_alloc
  RETURN
END SUBROUTINE nl_get_sf_surf_irr_alloc
SUBROUTINE nl_get_irr_daily_amount ( id_id , irr_daily_amount )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: irr_daily_amount
  INTEGER id_id
  irr_daily_amount = model_config_rec%irr_daily_amount(id_id)
  RETURN
END SUBROUTINE nl_get_irr_daily_amount
SUBROUTINE nl_get_irr_start_hour ( id_id , irr_start_hour )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: irr_start_hour
  INTEGER id_id
  irr_start_hour = model_config_rec%irr_start_hour(id_id)
  RETURN
END SUBROUTINE nl_get_irr_start_hour
SUBROUTINE nl_get_irr_num_hours ( id_id , irr_num_hours )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: irr_num_hours
  INTEGER id_id
  irr_num_hours = model_config_rec%irr_num_hours(id_id)
  RETURN
END SUBROUTINE nl_get_irr_num_hours
SUBROUTINE nl_get_irr_start_julianday ( id_id , irr_start_julianday )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: irr_start_julianday
  INTEGER id_id
  irr_start_julianday = model_config_rec%irr_start_julianday(id_id)
  RETURN
END SUBROUTINE nl_get_irr_start_julianday
SUBROUTINE nl_get_irr_end_julianday ( id_id , irr_end_julianday )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: irr_end_julianday
  INTEGER id_id
  irr_end_julianday = model_config_rec%irr_end_julianday(id_id)
  RETURN
END SUBROUTINE nl_get_irr_end_julianday
SUBROUTINE nl_get_irr_freq ( id_id , irr_freq )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: irr_freq
  INTEGER id_id
  irr_freq = model_config_rec%irr_freq(id_id)
  RETURN
END SUBROUTINE nl_get_irr_freq
SUBROUTINE nl_get_irr_ph ( id_id , irr_ph )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: irr_ph
  INTEGER id_id
  irr_ph = model_config_rec%irr_ph(id_id)
  RETURN
END SUBROUTINE nl_get_irr_ph
SUBROUTINE nl_get_sf_surface_physics ( id_id , sf_surface_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sf_surface_physics
  INTEGER id_id
  sf_surface_physics = model_config_rec%sf_surface_physics(id_id)
  RETURN
END SUBROUTINE nl_get_sf_surface_physics
SUBROUTINE nl_get_bl_pbl_physics ( id_id , bl_pbl_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: bl_pbl_physics
  INTEGER id_id
  bl_pbl_physics = model_config_rec%bl_pbl_physics(id_id)
  RETURN
END SUBROUTINE nl_get_bl_pbl_physics
SUBROUTINE nl_get_bl_mynn_tkebudget ( id_id , bl_mynn_tkebudget )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: bl_mynn_tkebudget
  INTEGER id_id
  bl_mynn_tkebudget = model_config_rec%bl_mynn_tkebudget(id_id)
  RETURN
END SUBROUTINE nl_get_bl_mynn_tkebudget
SUBROUTINE nl_get_ysu_topdown_pblmix ( id_id , ysu_topdown_pblmix )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: ysu_topdown_pblmix
  INTEGER id_id
  ysu_topdown_pblmix = model_config_rec%ysu_topdown_pblmix
  RETURN
END SUBROUTINE nl_get_ysu_topdown_pblmix
SUBROUTINE nl_get_shinhong_tke_diag ( id_id , shinhong_tke_diag )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: shinhong_tke_diag
  INTEGER id_id
  shinhong_tke_diag = model_config_rec%shinhong_tke_diag(id_id)
  RETURN
END SUBROUTINE nl_get_shinhong_tke_diag
SUBROUTINE nl_get_bl_mynn_tkeadvect ( id_id , bl_mynn_tkeadvect )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: bl_mynn_tkeadvect
  INTEGER id_id
  bl_mynn_tkeadvect = model_config_rec%bl_mynn_tkeadvect(id_id)
  RETURN
END SUBROUTINE nl_get_bl_mynn_tkeadvect
SUBROUTINE nl_get_bl_mynn_cloudpdf ( id_id , bl_mynn_cloudpdf )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: bl_mynn_cloudpdf
  INTEGER id_id
  bl_mynn_cloudpdf = model_config_rec%bl_mynn_cloudpdf
  RETURN
END SUBROUTINE nl_get_bl_mynn_cloudpdf
SUBROUTINE nl_get_bl_mynn_mixlength ( id_id , bl_mynn_mixlength )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: bl_mynn_mixlength
  INTEGER id_id
  bl_mynn_mixlength = model_config_rec%bl_mynn_mixlength
  RETURN
END SUBROUTINE nl_get_bl_mynn_mixlength
SUBROUTINE nl_get_bl_mynn_edmf ( id_id , bl_mynn_edmf )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: bl_mynn_edmf
  INTEGER id_id
  bl_mynn_edmf = model_config_rec%bl_mynn_edmf(id_id)
  RETURN
END SUBROUTINE nl_get_bl_mynn_edmf
SUBROUTINE nl_get_bl_mynn_edmf_mom ( id_id , bl_mynn_edmf_mom )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: bl_mynn_edmf_mom
  INTEGER id_id
  bl_mynn_edmf_mom = model_config_rec%bl_mynn_edmf_mom(id_id)
  RETURN
END SUBROUTINE nl_get_bl_mynn_edmf_mom
SUBROUTINE nl_get_bl_mynn_edmf_tke ( id_id , bl_mynn_edmf_tke )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: bl_mynn_edmf_tke
  INTEGER id_id
  bl_mynn_edmf_tke = model_config_rec%bl_mynn_edmf_tke(id_id)
  RETURN
END SUBROUTINE nl_get_bl_mynn_edmf_tke
SUBROUTINE nl_get_bl_mynn_mixscalars ( id_id , bl_mynn_mixscalars )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: bl_mynn_mixscalars
  INTEGER id_id
  bl_mynn_mixscalars = model_config_rec%bl_mynn_mixscalars(id_id)
  RETURN
END SUBROUTINE nl_get_bl_mynn_mixscalars
SUBROUTINE nl_get_bl_mynn_output ( id_id , bl_mynn_output )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: bl_mynn_output
  INTEGER id_id
  bl_mynn_output = model_config_rec%bl_mynn_output(id_id)
  RETURN
END SUBROUTINE nl_get_bl_mynn_output
SUBROUTINE nl_get_bl_mynn_cloudmix ( id_id , bl_mynn_cloudmix )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: bl_mynn_cloudmix
  INTEGER id_id
  bl_mynn_cloudmix = model_config_rec%bl_mynn_cloudmix(id_id)
  RETURN
END SUBROUTINE nl_get_bl_mynn_cloudmix
SUBROUTINE nl_get_bl_mynn_mixqt ( id_id , bl_mynn_mixqt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: bl_mynn_mixqt
  INTEGER id_id
  bl_mynn_mixqt = model_config_rec%bl_mynn_mixqt(id_id)
  RETURN
END SUBROUTINE nl_get_bl_mynn_mixqt
SUBROUTINE nl_get_icloud_bl ( id_id , icloud_bl )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: icloud_bl
  INTEGER id_id
  icloud_bl = model_config_rec%icloud_bl
  RETURN
END SUBROUTINE nl_get_icloud_bl
SUBROUTINE nl_get_mfshconv ( id_id , mfshconv )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: mfshconv
  INTEGER id_id
  mfshconv = model_config_rec%mfshconv(id_id)
  RETURN
END SUBROUTINE nl_get_mfshconv
SUBROUTINE nl_get_sf_urban_physics ( id_id , sf_urban_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sf_urban_physics
  INTEGER id_id
  sf_urban_physics = model_config_rec%sf_urban_physics(id_id)
  RETURN
END SUBROUTINE nl_get_sf_urban_physics
SUBROUTINE nl_get_bldt ( id_id , bldt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: bldt
  INTEGER id_id
  bldt = model_config_rec%bldt(id_id)
  RETURN
END SUBROUTINE nl_get_bldt
SUBROUTINE nl_get_cu_physics ( id_id , cu_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: cu_physics
  INTEGER id_id
  cu_physics = model_config_rec%cu_physics(id_id)
  RETURN
END SUBROUTINE nl_get_cu_physics
!ENDOFREGISTRYGENERATEDINCLUDE
