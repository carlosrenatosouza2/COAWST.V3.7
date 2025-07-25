










!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nl_config.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
SUBROUTINE nl_set_scm_lon ( id_id , scm_lon )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: scm_lon
  INTEGER id_id
  model_config_rec%scm_lon = scm_lon 
  RETURN
END SUBROUTINE nl_set_scm_lon
SUBROUTINE nl_set_scm_th_t_tend ( id_id , scm_th_t_tend )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: scm_th_t_tend
  INTEGER id_id
  model_config_rec%scm_th_t_tend = scm_th_t_tend 
  RETURN
END SUBROUTINE nl_set_scm_th_t_tend
SUBROUTINE nl_set_scm_qv_t_tend ( id_id , scm_qv_t_tend )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: scm_qv_t_tend
  INTEGER id_id
  model_config_rec%scm_qv_t_tend = scm_qv_t_tend 
  RETURN
END SUBROUTINE nl_set_scm_qv_t_tend
SUBROUTINE nl_set_scm_th_adv ( id_id , scm_th_adv )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: scm_th_adv
  INTEGER id_id
  model_config_rec%scm_th_adv = scm_th_adv 
  RETURN
END SUBROUTINE nl_set_scm_th_adv
SUBROUTINE nl_set_scm_wind_adv ( id_id , scm_wind_adv )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: scm_wind_adv
  INTEGER id_id
  model_config_rec%scm_wind_adv = scm_wind_adv 
  RETURN
END SUBROUTINE nl_set_scm_wind_adv
SUBROUTINE nl_set_scm_qv_adv ( id_id , scm_qv_adv )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: scm_qv_adv
  INTEGER id_id
  model_config_rec%scm_qv_adv = scm_qv_adv 
  RETURN
END SUBROUTINE nl_set_scm_qv_adv
SUBROUTINE nl_set_scm_ql_adv ( id_id , scm_ql_adv )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: scm_ql_adv
  INTEGER id_id
  model_config_rec%scm_ql_adv = scm_ql_adv 
  RETURN
END SUBROUTINE nl_set_scm_ql_adv
SUBROUTINE nl_set_scm_vert_adv ( id_id , scm_vert_adv )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: scm_vert_adv
  INTEGER id_id
  model_config_rec%scm_vert_adv = scm_vert_adv 
  RETURN
END SUBROUTINE nl_set_scm_vert_adv
SUBROUTINE nl_set_num_force_soil_layers ( id_id , num_force_soil_layers )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_force_soil_layers
  INTEGER id_id
  model_config_rec%num_force_soil_layers = num_force_soil_layers 
  RETURN
END SUBROUTINE nl_set_num_force_soil_layers
SUBROUTINE nl_set_scm_soilt_force ( id_id , scm_soilt_force )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: scm_soilt_force
  INTEGER id_id
  model_config_rec%scm_soilt_force = scm_soilt_force 
  RETURN
END SUBROUTINE nl_set_scm_soilt_force
SUBROUTINE nl_set_scm_soilq_force ( id_id , scm_soilq_force )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: scm_soilq_force
  INTEGER id_id
  model_config_rec%scm_soilq_force = scm_soilq_force 
  RETURN
END SUBROUTINE nl_set_scm_soilq_force
SUBROUTINE nl_set_scm_force_th_largescale ( id_id , scm_force_th_largescale )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: scm_force_th_largescale
  INTEGER id_id
  model_config_rec%scm_force_th_largescale = scm_force_th_largescale 
  RETURN
END SUBROUTINE nl_set_scm_force_th_largescale
SUBROUTINE nl_set_scm_force_qv_largescale ( id_id , scm_force_qv_largescale )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: scm_force_qv_largescale
  INTEGER id_id
  model_config_rec%scm_force_qv_largescale = scm_force_qv_largescale 
  RETURN
END SUBROUTINE nl_set_scm_force_qv_largescale
SUBROUTINE nl_set_scm_force_ql_largescale ( id_id , scm_force_ql_largescale )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: scm_force_ql_largescale
  INTEGER id_id
  model_config_rec%scm_force_ql_largescale = scm_force_ql_largescale 
  RETURN
END SUBROUTINE nl_set_scm_force_ql_largescale
SUBROUTINE nl_set_scm_force_wind_largescale ( id_id , scm_force_wind_largescale )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: scm_force_wind_largescale
  INTEGER id_id
  model_config_rec%scm_force_wind_largescale = scm_force_wind_largescale 
  RETURN
END SUBROUTINE nl_set_scm_force_wind_largescale
SUBROUTINE nl_set_scm_force_skintemp ( id_id , scm_force_skintemp )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: scm_force_skintemp
  INTEGER id_id
  model_config_rec%scm_force_skintemp = scm_force_skintemp 
  RETURN
END SUBROUTINE nl_set_scm_force_skintemp
SUBROUTINE nl_set_scm_force_flux ( id_id , scm_force_flux )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: scm_force_flux
  INTEGER id_id
  model_config_rec%scm_force_flux = scm_force_flux 
  RETURN
END SUBROUTINE nl_set_scm_force_flux
SUBROUTINE nl_set_dyn_opt ( id_id , dyn_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dyn_opt
  INTEGER id_id
  model_config_rec%dyn_opt = dyn_opt 
  RETURN
END SUBROUTINE nl_set_dyn_opt
SUBROUTINE nl_set_rk_ord ( id_id , rk_ord )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: rk_ord
  INTEGER id_id
  model_config_rec%rk_ord = rk_ord 
  RETURN
END SUBROUTINE nl_set_rk_ord
SUBROUTINE nl_set_w_damping ( id_id , w_damping )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: w_damping
  INTEGER id_id
  model_config_rec%w_damping = w_damping 
  RETURN
END SUBROUTINE nl_set_w_damping
SUBROUTINE nl_set_diff_opt ( id_id , diff_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: diff_opt
  INTEGER id_id
  model_config_rec%diff_opt(id_id) = diff_opt
  RETURN
END SUBROUTINE nl_set_diff_opt
SUBROUTINE nl_set_diff_opt_dfi ( id_id , diff_opt_dfi )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: diff_opt_dfi
  INTEGER id_id
  model_config_rec%diff_opt_dfi(id_id) = diff_opt_dfi
  RETURN
END SUBROUTINE nl_set_diff_opt_dfi
SUBROUTINE nl_set_km_opt ( id_id , km_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: km_opt
  INTEGER id_id
  model_config_rec%km_opt(id_id) = km_opt
  RETURN
END SUBROUTINE nl_set_km_opt
SUBROUTINE nl_set_km_opt_dfi ( id_id , km_opt_dfi )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: km_opt_dfi
  INTEGER id_id
  model_config_rec%km_opt_dfi(id_id) = km_opt_dfi
  RETURN
END SUBROUTINE nl_set_km_opt_dfi
SUBROUTINE nl_set_damp_opt ( id_id , damp_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: damp_opt
  INTEGER id_id
  model_config_rec%damp_opt = damp_opt 
  RETURN
END SUBROUTINE nl_set_damp_opt
SUBROUTINE nl_set_rad_nudge ( id_id , rad_nudge )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: rad_nudge
  INTEGER id_id
  model_config_rec%rad_nudge = rad_nudge 
  RETURN
END SUBROUTINE nl_set_rad_nudge
SUBROUTINE nl_set_gwd_opt ( id_id , gwd_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gwd_opt
  INTEGER id_id
  model_config_rec%gwd_opt(id_id) = gwd_opt
  RETURN
END SUBROUTINE nl_set_gwd_opt
SUBROUTINE nl_set_max_rot_angle_gwd ( id_id , max_rot_angle_gwd )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: max_rot_angle_gwd
  INTEGER id_id
  model_config_rec%max_rot_angle_gwd = max_rot_angle_gwd 
  RETURN
END SUBROUTINE nl_set_max_rot_angle_gwd
SUBROUTINE nl_set_zdamp ( id_id , zdamp )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: zdamp
  INTEGER id_id
  model_config_rec%zdamp(id_id) = zdamp
  RETURN
END SUBROUTINE nl_set_zdamp
SUBROUTINE nl_set_dampcoef ( id_id , dampcoef )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: dampcoef
  INTEGER id_id
  model_config_rec%dampcoef(id_id) = dampcoef
  RETURN
END SUBROUTINE nl_set_dampcoef
SUBROUTINE nl_set_khdif ( id_id , khdif )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: khdif
  INTEGER id_id
  model_config_rec%khdif(id_id) = khdif
  RETURN
END SUBROUTINE nl_set_khdif
SUBROUTINE nl_set_kvdif ( id_id , kvdif )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: kvdif
  INTEGER id_id
  model_config_rec%kvdif(id_id) = kvdif
  RETURN
END SUBROUTINE nl_set_kvdif
SUBROUTINE nl_set_diff_6th_factor ( id_id , diff_6th_factor )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: diff_6th_factor
  INTEGER id_id
  model_config_rec%diff_6th_factor(id_id) = diff_6th_factor
  RETURN
END SUBROUTINE nl_set_diff_6th_factor
SUBROUTINE nl_set_diff_6th_opt ( id_id , diff_6th_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: diff_6th_opt
  INTEGER id_id
  model_config_rec%diff_6th_opt(id_id) = diff_6th_opt
  RETURN
END SUBROUTINE nl_set_diff_6th_opt
SUBROUTINE nl_set_diff_6th_slopeopt ( id_id , diff_6th_slopeopt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: diff_6th_slopeopt
  INTEGER id_id
  model_config_rec%diff_6th_slopeopt(id_id) = diff_6th_slopeopt
  RETURN
END SUBROUTINE nl_set_diff_6th_slopeopt
SUBROUTINE nl_set_diff_6th_thresh ( id_id , diff_6th_thresh )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: diff_6th_thresh
  INTEGER id_id
  model_config_rec%diff_6th_thresh(id_id) = diff_6th_thresh
  RETURN
END SUBROUTINE nl_set_diff_6th_thresh
SUBROUTINE nl_set_use_theta_m ( id_id , use_theta_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: use_theta_m
  INTEGER id_id
  model_config_rec%use_theta_m = use_theta_m 
  RETURN
END SUBROUTINE nl_set_use_theta_m
SUBROUTINE nl_set_use_q_diabatic ( id_id , use_q_diabatic )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: use_q_diabatic
  INTEGER id_id
  model_config_rec%use_q_diabatic = use_q_diabatic 
  RETURN
END SUBROUTINE nl_set_use_q_diabatic
SUBROUTINE nl_set_c_s ( id_id , c_s )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: c_s
  INTEGER id_id
  model_config_rec%c_s(id_id) = c_s
  RETURN
END SUBROUTINE nl_set_c_s
SUBROUTINE nl_set_c_k ( id_id , c_k )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: c_k
  INTEGER id_id
  model_config_rec%c_k(id_id) = c_k
  RETURN
END SUBROUTINE nl_set_c_k
SUBROUTINE nl_set_smdiv ( id_id , smdiv )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: smdiv
  INTEGER id_id
  model_config_rec%smdiv(id_id) = smdiv
  RETURN
END SUBROUTINE nl_set_smdiv
SUBROUTINE nl_set_emdiv ( id_id , emdiv )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: emdiv
  INTEGER id_id
  model_config_rec%emdiv(id_id) = emdiv
  RETURN
END SUBROUTINE nl_set_emdiv
SUBROUTINE nl_set_epssm ( id_id , epssm )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: epssm
  INTEGER id_id
  model_config_rec%epssm(id_id) = epssm
  RETURN
END SUBROUTINE nl_set_epssm
SUBROUTINE nl_set_non_hydrostatic ( id_id , non_hydrostatic )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: non_hydrostatic
  INTEGER id_id
  model_config_rec%non_hydrostatic(id_id) = non_hydrostatic
  RETURN
END SUBROUTINE nl_set_non_hydrostatic
SUBROUTINE nl_set_use_input_w ( id_id , use_input_w )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: use_input_w
  INTEGER id_id
  model_config_rec%use_input_w = use_input_w 
  RETURN
END SUBROUTINE nl_set_use_input_w
SUBROUTINE nl_set_time_step_sound ( id_id , time_step_sound )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: time_step_sound
  INTEGER id_id
  model_config_rec%time_step_sound(id_id) = time_step_sound
  RETURN
END SUBROUTINE nl_set_time_step_sound
SUBROUTINE nl_set_h_mom_adv_order ( id_id , h_mom_adv_order )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: h_mom_adv_order
  INTEGER id_id
  model_config_rec%h_mom_adv_order(id_id) = h_mom_adv_order
  RETURN
END SUBROUTINE nl_set_h_mom_adv_order
SUBROUTINE nl_set_v_mom_adv_order ( id_id , v_mom_adv_order )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: v_mom_adv_order
  INTEGER id_id
  model_config_rec%v_mom_adv_order(id_id) = v_mom_adv_order
  RETURN
END SUBROUTINE nl_set_v_mom_adv_order
SUBROUTINE nl_set_h_sca_adv_order ( id_id , h_sca_adv_order )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: h_sca_adv_order
  INTEGER id_id
  model_config_rec%h_sca_adv_order(id_id) = h_sca_adv_order
  RETURN
END SUBROUTINE nl_set_h_sca_adv_order
SUBROUTINE nl_set_v_sca_adv_order ( id_id , v_sca_adv_order )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: v_sca_adv_order
  INTEGER id_id
  model_config_rec%v_sca_adv_order(id_id) = v_sca_adv_order
  RETURN
END SUBROUTINE nl_set_v_sca_adv_order
SUBROUTINE nl_set_momentum_adv_opt ( id_id , momentum_adv_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: momentum_adv_opt
  INTEGER id_id
  model_config_rec%momentum_adv_opt(id_id) = momentum_adv_opt
  RETURN
END SUBROUTINE nl_set_momentum_adv_opt
SUBROUTINE nl_set_moist_adv_opt ( id_id , moist_adv_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: moist_adv_opt
  INTEGER id_id
  model_config_rec%moist_adv_opt(id_id) = moist_adv_opt
  RETURN
END SUBROUTINE nl_set_moist_adv_opt
SUBROUTINE nl_set_moist_adv_dfi_opt ( id_id , moist_adv_dfi_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: moist_adv_dfi_opt
  INTEGER id_id
  model_config_rec%moist_adv_dfi_opt(id_id) = moist_adv_dfi_opt
  RETURN
END SUBROUTINE nl_set_moist_adv_dfi_opt
SUBROUTINE nl_set_chem_adv_opt ( id_id , chem_adv_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: chem_adv_opt
  INTEGER id_id
  model_config_rec%chem_adv_opt(id_id) = chem_adv_opt
  RETURN
END SUBROUTINE nl_set_chem_adv_opt
SUBROUTINE nl_set_tracer_adv_opt ( id_id , tracer_adv_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: tracer_adv_opt
  INTEGER id_id
  model_config_rec%tracer_adv_opt(id_id) = tracer_adv_opt
  RETURN
END SUBROUTINE nl_set_tracer_adv_opt
SUBROUTINE nl_set_scalar_adv_opt ( id_id , scalar_adv_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: scalar_adv_opt
  INTEGER id_id
  model_config_rec%scalar_adv_opt(id_id) = scalar_adv_opt
  RETURN
END SUBROUTINE nl_set_scalar_adv_opt
SUBROUTINE nl_set_tke_adv_opt ( id_id , tke_adv_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: tke_adv_opt
  INTEGER id_id
  model_config_rec%tke_adv_opt(id_id) = tke_adv_opt
  RETURN
END SUBROUTINE nl_set_tke_adv_opt
SUBROUTINE nl_set_moist_mix2_off ( id_id , moist_mix2_off )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: moist_mix2_off
  INTEGER id_id
  model_config_rec%moist_mix2_off(id_id) = moist_mix2_off
  RETURN
END SUBROUTINE nl_set_moist_mix2_off
SUBROUTINE nl_set_chem_mix2_off ( id_id , chem_mix2_off )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: chem_mix2_off
  INTEGER id_id
  model_config_rec%chem_mix2_off(id_id) = chem_mix2_off
  RETURN
END SUBROUTINE nl_set_chem_mix2_off
SUBROUTINE nl_set_tracer_mix2_off ( id_id , tracer_mix2_off )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: tracer_mix2_off
  INTEGER id_id
  model_config_rec%tracer_mix2_off(id_id) = tracer_mix2_off
  RETURN
END SUBROUTINE nl_set_tracer_mix2_off
SUBROUTINE nl_set_scalar_mix2_off ( id_id , scalar_mix2_off )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: scalar_mix2_off
  INTEGER id_id
  model_config_rec%scalar_mix2_off(id_id) = scalar_mix2_off
  RETURN
END SUBROUTINE nl_set_scalar_mix2_off
SUBROUTINE nl_set_tke_mix2_off ( id_id , tke_mix2_off )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: tke_mix2_off
  INTEGER id_id
  model_config_rec%tke_mix2_off(id_id) = tke_mix2_off
  RETURN
END SUBROUTINE nl_set_tke_mix2_off
SUBROUTINE nl_set_moist_mix6_off ( id_id , moist_mix6_off )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: moist_mix6_off
  INTEGER id_id
  model_config_rec%moist_mix6_off(id_id) = moist_mix6_off
  RETURN
END SUBROUTINE nl_set_moist_mix6_off
SUBROUTINE nl_set_chem_mix6_off ( id_id , chem_mix6_off )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: chem_mix6_off
  INTEGER id_id
  model_config_rec%chem_mix6_off(id_id) = chem_mix6_off
  RETURN
END SUBROUTINE nl_set_chem_mix6_off
SUBROUTINE nl_set_tracer_mix6_off ( id_id , tracer_mix6_off )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: tracer_mix6_off
  INTEGER id_id
  model_config_rec%tracer_mix6_off(id_id) = tracer_mix6_off
  RETURN
END SUBROUTINE nl_set_tracer_mix6_off
SUBROUTINE nl_set_scalar_mix6_off ( id_id , scalar_mix6_off )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: scalar_mix6_off
  INTEGER id_id
  model_config_rec%scalar_mix6_off(id_id) = scalar_mix6_off
  RETURN
END SUBROUTINE nl_set_scalar_mix6_off
SUBROUTINE nl_set_tke_mix6_off ( id_id , tke_mix6_off )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: tke_mix6_off
  INTEGER id_id
  model_config_rec%tke_mix6_off(id_id) = tke_mix6_off
  RETURN
END SUBROUTINE nl_set_tke_mix6_off
SUBROUTINE nl_set_top_radiation ( id_id , top_radiation )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: top_radiation
  INTEGER id_id
  model_config_rec%top_radiation(id_id) = top_radiation
  RETURN
END SUBROUTINE nl_set_top_radiation
SUBROUTINE nl_set_mix_isotropic ( id_id , mix_isotropic )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: mix_isotropic
  INTEGER id_id
  model_config_rec%mix_isotropic(id_id) = mix_isotropic
  RETURN
END SUBROUTINE nl_set_mix_isotropic
SUBROUTINE nl_set_mix_upper_bound ( id_id , mix_upper_bound )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: mix_upper_bound
  INTEGER id_id
  model_config_rec%mix_upper_bound(id_id) = mix_upper_bound
  RETURN
END SUBROUTINE nl_set_mix_upper_bound
SUBROUTINE nl_set_top_lid ( id_id , top_lid )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: top_lid
  INTEGER id_id
  model_config_rec%top_lid(id_id) = top_lid
  RETURN
END SUBROUTINE nl_set_top_lid
SUBROUTINE nl_set_tke_upper_bound ( id_id , tke_upper_bound )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: tke_upper_bound
  INTEGER id_id
  model_config_rec%tke_upper_bound(id_id) = tke_upper_bound
  RETURN
END SUBROUTINE nl_set_tke_upper_bound
SUBROUTINE nl_set_tke_drag_coefficient ( id_id , tke_drag_coefficient )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: tke_drag_coefficient
  INTEGER id_id
  model_config_rec%tke_drag_coefficient(id_id) = tke_drag_coefficient
  RETURN
END SUBROUTINE nl_set_tke_drag_coefficient
SUBROUTINE nl_set_tke_heat_flux ( id_id , tke_heat_flux )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: tke_heat_flux
  INTEGER id_id
  model_config_rec%tke_heat_flux(id_id) = tke_heat_flux
  RETURN
END SUBROUTINE nl_set_tke_heat_flux
SUBROUTINE nl_set_pert_coriolis ( id_id , pert_coriolis )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: pert_coriolis
  INTEGER id_id
  model_config_rec%pert_coriolis(id_id) = pert_coriolis
  RETURN
END SUBROUTINE nl_set_pert_coriolis
SUBROUTINE nl_set_coriolis2d ( id_id , coriolis2d )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: coriolis2d
  INTEGER id_id
  model_config_rec%coriolis2d(id_id) = coriolis2d
  RETURN
END SUBROUTINE nl_set_coriolis2d
SUBROUTINE nl_set_mix_full_fields ( id_id , mix_full_fields )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: mix_full_fields
  INTEGER id_id
  model_config_rec%mix_full_fields(id_id) = mix_full_fields
  RETURN
END SUBROUTINE nl_set_mix_full_fields
SUBROUTINE nl_set_base_pres ( id_id , base_pres )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: base_pres
  INTEGER id_id
  model_config_rec%base_pres = base_pres 
  RETURN
END SUBROUTINE nl_set_base_pres
SUBROUTINE nl_set_base_temp ( id_id , base_temp )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: base_temp
  INTEGER id_id
  model_config_rec%base_temp = base_temp 
  RETURN
END SUBROUTINE nl_set_base_temp
SUBROUTINE nl_set_base_lapse ( id_id , base_lapse )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: base_lapse
  INTEGER id_id
  model_config_rec%base_lapse = base_lapse 
  RETURN
END SUBROUTINE nl_set_base_lapse
SUBROUTINE nl_set_iso_temp ( id_id , iso_temp )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: iso_temp
  INTEGER id_id
  model_config_rec%iso_temp = iso_temp 
  RETURN
END SUBROUTINE nl_set_iso_temp
SUBROUTINE nl_set_base_pres_strat ( id_id , base_pres_strat )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: base_pres_strat
  INTEGER id_id
  model_config_rec%base_pres_strat = base_pres_strat 
  RETURN
END SUBROUTINE nl_set_base_pres_strat
SUBROUTINE nl_set_base_lapse_strat ( id_id , base_lapse_strat )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: base_lapse_strat
  INTEGER id_id
  model_config_rec%base_lapse_strat = base_lapse_strat 
  RETURN
END SUBROUTINE nl_set_base_lapse_strat
SUBROUTINE nl_set_use_baseparam_fr_nml ( id_id , use_baseparam_fr_nml )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: use_baseparam_fr_nml
  INTEGER id_id
  model_config_rec%use_baseparam_fr_nml = use_baseparam_fr_nml 
  RETURN
END SUBROUTINE nl_set_use_baseparam_fr_nml
SUBROUTINE nl_set_fft_filter_lat ( id_id , fft_filter_lat )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: fft_filter_lat
  INTEGER id_id
  model_config_rec%fft_filter_lat = fft_filter_lat 
  RETURN
END SUBROUTINE nl_set_fft_filter_lat
SUBROUTINE nl_set_coupled_filtering ( id_id , coupled_filtering )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: coupled_filtering
  INTEGER id_id
  model_config_rec%coupled_filtering = coupled_filtering 
  RETURN
END SUBROUTINE nl_set_coupled_filtering
SUBROUTINE nl_set_pos_def ( id_id , pos_def )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: pos_def
  INTEGER id_id
  model_config_rec%pos_def = pos_def 
  RETURN
END SUBROUTINE nl_set_pos_def
SUBROUTINE nl_set_swap_pole_with_next_j ( id_id , swap_pole_with_next_j )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: swap_pole_with_next_j
  INTEGER id_id
  model_config_rec%swap_pole_with_next_j = swap_pole_with_next_j 
  RETURN
END SUBROUTINE nl_set_swap_pole_with_next_j
SUBROUTINE nl_set_actual_distance_average ( id_id , actual_distance_average )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: actual_distance_average
  INTEGER id_id
  model_config_rec%actual_distance_average = actual_distance_average 
  RETURN
END SUBROUTINE nl_set_actual_distance_average
SUBROUTINE nl_set_rotated_pole ( id_id , rotated_pole )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: rotated_pole
  INTEGER id_id
  model_config_rec%rotated_pole = rotated_pole 
  RETURN
END SUBROUTINE nl_set_rotated_pole
SUBROUTINE nl_set_do_coriolis ( id_id , do_coriolis )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: do_coriolis
  INTEGER id_id
  model_config_rec%do_coriolis(id_id) = do_coriolis
  RETURN
END SUBROUTINE nl_set_do_coriolis
SUBROUTINE nl_set_do_curvature ( id_id , do_curvature )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: do_curvature
  INTEGER id_id
  model_config_rec%do_curvature(id_id) = do_curvature
  RETURN
END SUBROUTINE nl_set_do_curvature
SUBROUTINE nl_set_do_gradp ( id_id , do_gradp )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: do_gradp
  INTEGER id_id
  model_config_rec%do_gradp(id_id) = do_gradp
  RETURN
END SUBROUTINE nl_set_do_gradp
SUBROUTINE nl_set_tracer_opt ( id_id , tracer_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: tracer_opt
  INTEGER id_id
  model_config_rec%tracer_opt(id_id) = tracer_opt
  RETURN
END SUBROUTINE nl_set_tracer_opt
SUBROUTINE nl_set_tenddiag ( id_id , tenddiag )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: tenddiag
  INTEGER id_id
  model_config_rec%tenddiag(id_id) = tenddiag
  RETURN
END SUBROUTINE nl_set_tenddiag
SUBROUTINE nl_set_spec_bdy_width ( id_id , spec_bdy_width )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: spec_bdy_width
  INTEGER id_id
  model_config_rec%spec_bdy_width = spec_bdy_width 
  RETURN
END SUBROUTINE nl_set_spec_bdy_width
SUBROUTINE nl_set_spec_zone ( id_id , spec_zone )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: spec_zone
  INTEGER id_id
  model_config_rec%spec_zone = spec_zone 
  RETURN
END SUBROUTINE nl_set_spec_zone
SUBROUTINE nl_set_relax_zone ( id_id , relax_zone )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: relax_zone
  INTEGER id_id
  model_config_rec%relax_zone = relax_zone 
  RETURN
END SUBROUTINE nl_set_relax_zone
SUBROUTINE nl_set_specified ( id_id , specified )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: specified
  INTEGER id_id
  model_config_rec%specified(id_id) = specified
  RETURN
END SUBROUTINE nl_set_specified
SUBROUTINE nl_set_constant_bc ( id_id , constant_bc )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: constant_bc
  INTEGER id_id
  model_config_rec%constant_bc = constant_bc 
  RETURN
END SUBROUTINE nl_set_constant_bc
SUBROUTINE nl_set_periodic_x ( id_id , periodic_x )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: periodic_x
  INTEGER id_id
  model_config_rec%periodic_x(id_id) = periodic_x
  RETURN
END SUBROUTINE nl_set_periodic_x
SUBROUTINE nl_set_symmetric_xs ( id_id , symmetric_xs )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: symmetric_xs
  INTEGER id_id
  model_config_rec%symmetric_xs(id_id) = symmetric_xs
  RETURN
END SUBROUTINE nl_set_symmetric_xs
SUBROUTINE nl_set_symmetric_xe ( id_id , symmetric_xe )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: symmetric_xe
  INTEGER id_id
  model_config_rec%symmetric_xe(id_id) = symmetric_xe
  RETURN
END SUBROUTINE nl_set_symmetric_xe
SUBROUTINE nl_set_open_xs ( id_id , open_xs )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: open_xs
  INTEGER id_id
  model_config_rec%open_xs(id_id) = open_xs
  RETURN
END SUBROUTINE nl_set_open_xs
SUBROUTINE nl_set_open_xe ( id_id , open_xe )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: open_xe
  INTEGER id_id
  model_config_rec%open_xe(id_id) = open_xe
  RETURN
END SUBROUTINE nl_set_open_xe
SUBROUTINE nl_set_periodic_y ( id_id , periodic_y )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: periodic_y
  INTEGER id_id
  model_config_rec%periodic_y(id_id) = periodic_y
  RETURN
END SUBROUTINE nl_set_periodic_y
SUBROUTINE nl_set_symmetric_ys ( id_id , symmetric_ys )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: symmetric_ys
  INTEGER id_id
  model_config_rec%symmetric_ys(id_id) = symmetric_ys
  RETURN
END SUBROUTINE nl_set_symmetric_ys
SUBROUTINE nl_set_symmetric_ye ( id_id , symmetric_ye )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: symmetric_ye
  INTEGER id_id
  model_config_rec%symmetric_ye(id_id) = symmetric_ye
  RETURN
END SUBROUTINE nl_set_symmetric_ye
SUBROUTINE nl_set_open_ys ( id_id , open_ys )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: open_ys
  INTEGER id_id
  model_config_rec%open_ys(id_id) = open_ys
  RETURN
END SUBROUTINE nl_set_open_ys
SUBROUTINE nl_set_open_ye ( id_id , open_ye )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: open_ye
  INTEGER id_id
  model_config_rec%open_ye(id_id) = open_ye
  RETURN
END SUBROUTINE nl_set_open_ye
SUBROUTINE nl_set_polar ( id_id , polar )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: polar
  INTEGER id_id
  model_config_rec%polar(id_id) = polar
  RETURN
END SUBROUTINE nl_set_polar
SUBROUTINE nl_set_nested ( id_id , nested )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: nested
  INTEGER id_id
  model_config_rec%nested(id_id) = nested
  RETURN
END SUBROUTINE nl_set_nested
SUBROUTINE nl_set_spec_exp ( id_id , spec_exp )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: spec_exp
  INTEGER id_id
  model_config_rec%spec_exp = spec_exp 
  RETURN
END SUBROUTINE nl_set_spec_exp
SUBROUTINE nl_set_spec_bdy_final_mu ( id_id , spec_bdy_final_mu )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: spec_bdy_final_mu
  INTEGER id_id
  model_config_rec%spec_bdy_final_mu = spec_bdy_final_mu 
  RETURN
END SUBROUTINE nl_set_spec_bdy_final_mu
SUBROUTINE nl_set_real_data_init_type ( id_id , real_data_init_type )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: real_data_init_type
  INTEGER id_id
  model_config_rec%real_data_init_type = real_data_init_type 
  RETURN
END SUBROUTINE nl_set_real_data_init_type
SUBROUTINE nl_set_have_bcs_moist ( id_id , have_bcs_moist )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: have_bcs_moist
  INTEGER id_id
  model_config_rec%have_bcs_moist(id_id) = have_bcs_moist
  RETURN
END SUBROUTINE nl_set_have_bcs_moist
SUBROUTINE nl_set_have_bcs_scalar ( id_id , have_bcs_scalar )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: have_bcs_scalar
  INTEGER id_id
  model_config_rec%have_bcs_scalar(id_id) = have_bcs_scalar
  RETURN
END SUBROUTINE nl_set_have_bcs_scalar
SUBROUTINE nl_set_multi_bdy_files ( id_id , multi_bdy_files )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: multi_bdy_files
  INTEGER id_id
  model_config_rec%multi_bdy_files = multi_bdy_files 
  RETURN
END SUBROUTINE nl_set_multi_bdy_files
SUBROUTINE nl_set_background_proc_id ( id_id , background_proc_id )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: background_proc_id
  INTEGER id_id
  model_config_rec%background_proc_id = background_proc_id 
  RETURN
END SUBROUTINE nl_set_background_proc_id
SUBROUTINE nl_set_forecast_proc_id ( id_id , forecast_proc_id )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: forecast_proc_id
  INTEGER id_id
  model_config_rec%forecast_proc_id = forecast_proc_id 
  RETURN
END SUBROUTINE nl_set_forecast_proc_id
SUBROUTINE nl_set_production_status ( id_id , production_status )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: production_status
  INTEGER id_id
  model_config_rec%production_status = production_status 
  RETURN
END SUBROUTINE nl_set_production_status
SUBROUTINE nl_set_compression ( id_id , compression )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: compression
  INTEGER id_id
  model_config_rec%compression = compression 
  RETURN
END SUBROUTINE nl_set_compression
SUBROUTINE nl_set_nobs_ndg_vars ( id_id , nobs_ndg_vars )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: nobs_ndg_vars
  INTEGER id_id
  model_config_rec%nobs_ndg_vars = nobs_ndg_vars 
  RETURN
END SUBROUTINE nl_set_nobs_ndg_vars
SUBROUTINE nl_set_nobs_err_flds ( id_id , nobs_err_flds )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: nobs_err_flds
  INTEGER id_id
  model_config_rec%nobs_err_flds = nobs_err_flds 
  RETURN
END SUBROUTINE nl_set_nobs_err_flds
SUBROUTINE nl_set_cen_lat ( id_id , cen_lat )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: cen_lat
  INTEGER id_id
  model_config_rec%cen_lat(id_id) = cen_lat
  RETURN
END SUBROUTINE nl_set_cen_lat
SUBROUTINE nl_set_cen_lon ( id_id , cen_lon )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: cen_lon
  INTEGER id_id
  model_config_rec%cen_lon(id_id) = cen_lon
  RETURN
END SUBROUTINE nl_set_cen_lon
SUBROUTINE nl_set_truelat1 ( id_id , truelat1 )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: truelat1
  INTEGER id_id
  model_config_rec%truelat1(id_id) = truelat1
  RETURN
END SUBROUTINE nl_set_truelat1
SUBROUTINE nl_set_truelat2 ( id_id , truelat2 )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: truelat2
  INTEGER id_id
  model_config_rec%truelat2(id_id) = truelat2
  RETURN
END SUBROUTINE nl_set_truelat2
SUBROUTINE nl_set_moad_cen_lat ( id_id , moad_cen_lat )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: moad_cen_lat
  INTEGER id_id
  model_config_rec%moad_cen_lat(id_id) = moad_cen_lat
  RETURN
END SUBROUTINE nl_set_moad_cen_lat
SUBROUTINE nl_set_stand_lon ( id_id , stand_lon )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: stand_lon
  INTEGER id_id
  model_config_rec%stand_lon(id_id) = stand_lon
  RETURN
END SUBROUTINE nl_set_stand_lon
SUBROUTINE nl_set_pole_lat ( id_id , pole_lat )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: pole_lat
  INTEGER id_id
  model_config_rec%pole_lat(id_id) = pole_lat
  RETURN
END SUBROUTINE nl_set_pole_lat
SUBROUTINE nl_set_pole_lon ( id_id , pole_lon )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: pole_lon
  INTEGER id_id
  model_config_rec%pole_lon(id_id) = pole_lon
  RETURN
END SUBROUTINE nl_set_pole_lon
SUBROUTINE nl_set_flag_metgrid ( id_id , flag_metgrid )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_metgrid
  INTEGER id_id
  model_config_rec%flag_metgrid = flag_metgrid 
  RETURN
END SUBROUTINE nl_set_flag_metgrid
SUBROUTINE nl_set_flag_snow ( id_id , flag_snow )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_snow
  INTEGER id_id
  model_config_rec%flag_snow = flag_snow 
  RETURN
END SUBROUTINE nl_set_flag_snow
SUBROUTINE nl_set_flag_psfc ( id_id , flag_psfc )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_psfc
  INTEGER id_id
  model_config_rec%flag_psfc = flag_psfc 
  RETURN
END SUBROUTINE nl_set_flag_psfc
SUBROUTINE nl_set_flag_sm000010 ( id_id , flag_sm000010 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_sm000010
  INTEGER id_id
  model_config_rec%flag_sm000010 = flag_sm000010 
  RETURN
END SUBROUTINE nl_set_flag_sm000010
SUBROUTINE nl_set_flag_sm010040 ( id_id , flag_sm010040 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_sm010040
  INTEGER id_id
  model_config_rec%flag_sm010040 = flag_sm010040 
  RETURN
END SUBROUTINE nl_set_flag_sm010040
SUBROUTINE nl_set_flag_sm040100 ( id_id , flag_sm040100 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_sm040100
  INTEGER id_id
  model_config_rec%flag_sm040100 = flag_sm040100 
  RETURN
END SUBROUTINE nl_set_flag_sm040100
SUBROUTINE nl_set_flag_sm100200 ( id_id , flag_sm100200 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_sm100200
  INTEGER id_id
  model_config_rec%flag_sm100200 = flag_sm100200 
  RETURN
END SUBROUTINE nl_set_flag_sm100200
SUBROUTINE nl_set_flag_st000010 ( id_id , flag_st000010 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_st000010
  INTEGER id_id
  model_config_rec%flag_st000010 = flag_st000010 
  RETURN
END SUBROUTINE nl_set_flag_st000010
SUBROUTINE nl_set_flag_st010040 ( id_id , flag_st010040 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_st010040
  INTEGER id_id
  model_config_rec%flag_st010040 = flag_st010040 
  RETURN
END SUBROUTINE nl_set_flag_st010040
SUBROUTINE nl_set_flag_st040100 ( id_id , flag_st040100 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_st040100
  INTEGER id_id
  model_config_rec%flag_st040100 = flag_st040100 
  RETURN
END SUBROUTINE nl_set_flag_st040100
SUBROUTINE nl_set_flag_st100200 ( id_id , flag_st100200 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_st100200
  INTEGER id_id
  model_config_rec%flag_st100200 = flag_st100200 
  RETURN
END SUBROUTINE nl_set_flag_st100200
SUBROUTINE nl_set_flag_soil_layers ( id_id , flag_soil_layers )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_soil_layers
  INTEGER id_id
  model_config_rec%flag_soil_layers = flag_soil_layers 
  RETURN
END SUBROUTINE nl_set_flag_soil_layers
SUBROUTINE nl_set_flag_slp ( id_id , flag_slp )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_slp
  INTEGER id_id
  model_config_rec%flag_slp = flag_slp 
  RETURN
END SUBROUTINE nl_set_flag_slp
SUBROUTINE nl_set_flag_soilhgt ( id_id , flag_soilhgt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_soilhgt
  INTEGER id_id
  model_config_rec%flag_soilhgt = flag_soilhgt 
  RETURN
END SUBROUTINE nl_set_flag_soilhgt
SUBROUTINE nl_set_flag_mf_xy ( id_id , flag_mf_xy )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_mf_xy
  INTEGER id_id
  model_config_rec%flag_mf_xy = flag_mf_xy 
  RETURN
END SUBROUTINE nl_set_flag_mf_xy
SUBROUTINE nl_set_flag_um_soil ( id_id , flag_um_soil )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_um_soil
  INTEGER id_id
  model_config_rec%flag_um_soil = flag_um_soil 
  RETURN
END SUBROUTINE nl_set_flag_um_soil
SUBROUTINE nl_set_bdyfrq ( id_id , bdyfrq )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: bdyfrq
  INTEGER id_id
  model_config_rec%bdyfrq(id_id) = bdyfrq
  RETURN
END SUBROUTINE nl_set_bdyfrq
SUBROUTINE nl_set_mminlu ( id_id , mminlu )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: mminlu
  INTEGER id_id
  model_config_rec%mminlu(id_id) = mminlu
  RETURN
END SUBROUTINE nl_set_mminlu
SUBROUTINE nl_set_iswater ( id_id , iswater )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: iswater
  INTEGER id_id
  model_config_rec%iswater(id_id) = iswater
  RETURN
END SUBROUTINE nl_set_iswater
SUBROUTINE nl_set_islake ( id_id , islake )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: islake
  INTEGER id_id
  model_config_rec%islake(id_id) = islake
  RETURN
END SUBROUTINE nl_set_islake
SUBROUTINE nl_set_isice ( id_id , isice )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: isice
  INTEGER id_id
  model_config_rec%isice(id_id) = isice
  RETURN
END SUBROUTINE nl_set_isice
SUBROUTINE nl_set_isurban ( id_id , isurban )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: isurban
  INTEGER id_id
  model_config_rec%isurban(id_id) = isurban
  RETURN
END SUBROUTINE nl_set_isurban
SUBROUTINE nl_set_isoilwater ( id_id , isoilwater )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: isoilwater
  INTEGER id_id
  model_config_rec%isoilwater(id_id) = isoilwater
  RETURN
END SUBROUTINE nl_set_isoilwater
SUBROUTINE nl_set_map_proj ( id_id , map_proj )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: map_proj
  INTEGER id_id
  model_config_rec%map_proj(id_id) = map_proj
  RETURN
END SUBROUTINE nl_set_map_proj
SUBROUTINE nl_set_use_wps_input ( id_id , use_wps_input )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: use_wps_input
  INTEGER id_id
  model_config_rec%use_wps_input = use_wps_input 
  RETURN
END SUBROUTINE nl_set_use_wps_input
SUBROUTINE nl_set_dfi_stage ( id_id , dfi_stage )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_stage
  INTEGER id_id
  model_config_rec%dfi_stage(id_id) = dfi_stage
  RETURN
END SUBROUTINE nl_set_dfi_stage
SUBROUTINE nl_set_mp_physics_dfi ( id_id , mp_physics_dfi )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: mp_physics_dfi
  INTEGER id_id
  model_config_rec%mp_physics_dfi(id_id) = mp_physics_dfi
  RETURN
END SUBROUTINE nl_set_mp_physics_dfi
SUBROUTINE nl_set_bl_pbl_physics_dfi ( id_id , bl_pbl_physics_dfi )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: bl_pbl_physics_dfi
  INTEGER id_id
  model_config_rec%bl_pbl_physics_dfi(id_id) = bl_pbl_physics_dfi
  RETURN
END SUBROUTINE nl_set_bl_pbl_physics_dfi
SUBROUTINE nl_set_windfarm_opt ( id_id , windfarm_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: windfarm_opt
  INTEGER id_id
  model_config_rec%windfarm_opt(id_id) = windfarm_opt
  RETURN
END SUBROUTINE nl_set_windfarm_opt
SUBROUTINE nl_set_windfarm_ij ( id_id , windfarm_ij )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: windfarm_ij
  INTEGER id_id
  model_config_rec%windfarm_ij = windfarm_ij 
  RETURN
END SUBROUTINE nl_set_windfarm_ij
SUBROUTINE nl_set_windfarm_tke_factor ( id_id , windfarm_tke_factor )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: windfarm_tke_factor
  INTEGER id_id
  model_config_rec%windfarm_tke_factor = windfarm_tke_factor 
  RETURN
END SUBROUTINE nl_set_windfarm_tke_factor
SUBROUTINE nl_set_ideal_case ( id_id , ideal_case )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: ideal_case
  INTEGER id_id
  model_config_rec%ideal_case = ideal_case 
  RETURN
END SUBROUTINE nl_set_ideal_case
SUBROUTINE nl_set_hailcast_opt ( id_id , hailcast_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: hailcast_opt
  INTEGER id_id
  model_config_rec%hailcast_opt(id_id) = hailcast_opt
  RETURN
END SUBROUTINE nl_set_hailcast_opt
SUBROUTINE nl_set_haildt ( id_id , haildt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: haildt
  INTEGER id_id
  model_config_rec%haildt(id_id) = haildt
  RETURN
END SUBROUTINE nl_set_haildt
SUBROUTINE nl_set_lightning_option ( id_id , lightning_option )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: lightning_option
  INTEGER id_id
  model_config_rec%lightning_option(id_id) = lightning_option
  RETURN
END SUBROUTINE nl_set_lightning_option
SUBROUTINE nl_set_lightning_dt ( id_id , lightning_dt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: lightning_dt
  INTEGER id_id
  model_config_rec%lightning_dt(id_id) = lightning_dt
  RETURN
END SUBROUTINE nl_set_lightning_dt
SUBROUTINE nl_set_lightning_start_seconds ( id_id , lightning_start_seconds )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: lightning_start_seconds
  INTEGER id_id
  model_config_rec%lightning_start_seconds(id_id) = lightning_start_seconds
  RETURN
END SUBROUTINE nl_set_lightning_start_seconds
SUBROUTINE nl_set_flashrate_factor ( id_id , flashrate_factor )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: flashrate_factor
  INTEGER id_id
  model_config_rec%flashrate_factor(id_id) = flashrate_factor
  RETURN
END SUBROUTINE nl_set_flashrate_factor
SUBROUTINE nl_set_iccg_method ( id_id , iccg_method )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: iccg_method
  INTEGER id_id
  model_config_rec%iccg_method(id_id) = iccg_method
  RETURN
END SUBROUTINE nl_set_iccg_method
SUBROUTINE nl_set_iccg_prescribed_num ( id_id , iccg_prescribed_num )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: iccg_prescribed_num
  INTEGER id_id
  model_config_rec%iccg_prescribed_num(id_id) = iccg_prescribed_num
  RETURN
END SUBROUTINE nl_set_iccg_prescribed_num
SUBROUTINE nl_set_iccg_prescribed_den ( id_id , iccg_prescribed_den )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: iccg_prescribed_den
  INTEGER id_id
  model_config_rec%iccg_prescribed_den(id_id) = iccg_prescribed_den
  RETURN
END SUBROUTINE nl_set_iccg_prescribed_den
SUBROUTINE nl_set_cellcount_method ( id_id , cellcount_method )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: cellcount_method
  INTEGER id_id
  model_config_rec%cellcount_method(id_id) = cellcount_method
  RETURN
END SUBROUTINE nl_set_cellcount_method
SUBROUTINE nl_set_cldtop_adjustment ( id_id , cldtop_adjustment )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: cldtop_adjustment
  INTEGER id_id
  model_config_rec%cldtop_adjustment(id_id) = cldtop_adjustment
  RETURN
END SUBROUTINE nl_set_cldtop_adjustment
SUBROUTINE nl_set_sf_lake_physics ( id_id , sf_lake_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sf_lake_physics
  INTEGER id_id
  model_config_rec%sf_lake_physics(id_id) = sf_lake_physics
  RETURN
END SUBROUTINE nl_set_sf_lake_physics
SUBROUTINE nl_set_auxinput1_inname ( id_id , auxinput1_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput1_inname
  INTEGER id_id
  model_config_rec%auxinput1_inname = trim(auxinput1_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput1_inname
SUBROUTINE nl_set_io_form_auxinput1 ( id_id , io_form_auxinput1 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput1
  INTEGER id_id
  model_config_rec%io_form_auxinput1 = io_form_auxinput1 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput1
SUBROUTINE nl_set_override_restart_timers ( id_id , override_restart_timers )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: override_restart_timers
  INTEGER id_id
  model_config_rec%override_restart_timers = override_restart_timers 
  RETURN
END SUBROUTINE nl_set_override_restart_timers
SUBROUTINE nl_set_auxhist1_inname ( id_id , auxhist1_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist1_inname
  INTEGER id_id
  model_config_rec%auxhist1_inname = trim(auxhist1_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist1_inname
SUBROUTINE nl_set_auxhist1_outname ( id_id , auxhist1_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist1_outname
  INTEGER id_id
  model_config_rec%auxhist1_outname = trim(auxhist1_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist1_outname
SUBROUTINE nl_set_auxhist1_interval_y ( id_id , auxhist1_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_interval_y
  INTEGER id_id
  model_config_rec%auxhist1_interval_y(id_id) = auxhist1_interval_y
  RETURN
END SUBROUTINE nl_set_auxhist1_interval_y
SUBROUTINE nl_set_auxhist1_interval_d ( id_id , auxhist1_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_interval_d
  INTEGER id_id
  model_config_rec%auxhist1_interval_d(id_id) = auxhist1_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist1_interval_d
SUBROUTINE nl_set_auxhist1_interval_h ( id_id , auxhist1_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_interval_h
  INTEGER id_id
  model_config_rec%auxhist1_interval_h(id_id) = auxhist1_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist1_interval_h
SUBROUTINE nl_set_auxhist1_interval_m ( id_id , auxhist1_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_interval_m
  INTEGER id_id
  model_config_rec%auxhist1_interval_m(id_id) = auxhist1_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist1_interval_m
SUBROUTINE nl_set_auxhist1_interval_s ( id_id , auxhist1_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_interval_s
  INTEGER id_id
  model_config_rec%auxhist1_interval_s(id_id) = auxhist1_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist1_interval_s
SUBROUTINE nl_set_auxhist1_interval ( id_id , auxhist1_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_interval
  INTEGER id_id
  model_config_rec%auxhist1_interval(id_id) = auxhist1_interval
  RETURN
END SUBROUTINE nl_set_auxhist1_interval
SUBROUTINE nl_set_auxhist1_begin_y ( id_id , auxhist1_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_begin_y
  INTEGER id_id
  model_config_rec%auxhist1_begin_y(id_id) = auxhist1_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist1_begin_y
SUBROUTINE nl_set_auxhist1_begin_d ( id_id , auxhist1_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_begin_d
  INTEGER id_id
  model_config_rec%auxhist1_begin_d(id_id) = auxhist1_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist1_begin_d
SUBROUTINE nl_set_auxhist1_begin_h ( id_id , auxhist1_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_begin_h
  INTEGER id_id
  model_config_rec%auxhist1_begin_h(id_id) = auxhist1_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist1_begin_h
SUBROUTINE nl_set_auxhist1_begin_m ( id_id , auxhist1_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_begin_m
  INTEGER id_id
  model_config_rec%auxhist1_begin_m(id_id) = auxhist1_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist1_begin_m
SUBROUTINE nl_set_auxhist1_begin_s ( id_id , auxhist1_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_begin_s
  INTEGER id_id
  model_config_rec%auxhist1_begin_s(id_id) = auxhist1_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist1_begin_s
SUBROUTINE nl_set_auxhist1_begin ( id_id , auxhist1_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_begin
  INTEGER id_id
  model_config_rec%auxhist1_begin(id_id) = auxhist1_begin
  RETURN
END SUBROUTINE nl_set_auxhist1_begin
SUBROUTINE nl_set_auxhist1_end_y ( id_id , auxhist1_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_end_y
  INTEGER id_id
  model_config_rec%auxhist1_end_y(id_id) = auxhist1_end_y
  RETURN
END SUBROUTINE nl_set_auxhist1_end_y
SUBROUTINE nl_set_auxhist1_end_d ( id_id , auxhist1_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_end_d
  INTEGER id_id
  model_config_rec%auxhist1_end_d(id_id) = auxhist1_end_d
  RETURN
END SUBROUTINE nl_set_auxhist1_end_d
SUBROUTINE nl_set_auxhist1_end_h ( id_id , auxhist1_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_end_h
  INTEGER id_id
  model_config_rec%auxhist1_end_h(id_id) = auxhist1_end_h
  RETURN
END SUBROUTINE nl_set_auxhist1_end_h
SUBROUTINE nl_set_auxhist1_end_m ( id_id , auxhist1_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_end_m
  INTEGER id_id
  model_config_rec%auxhist1_end_m(id_id) = auxhist1_end_m
  RETURN
END SUBROUTINE nl_set_auxhist1_end_m
SUBROUTINE nl_set_auxhist1_end_s ( id_id , auxhist1_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_end_s
  INTEGER id_id
  model_config_rec%auxhist1_end_s(id_id) = auxhist1_end_s
  RETURN
END SUBROUTINE nl_set_auxhist1_end_s
SUBROUTINE nl_set_auxhist1_end ( id_id , auxhist1_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_end
  INTEGER id_id
  model_config_rec%auxhist1_end(id_id) = auxhist1_end
  RETURN
END SUBROUTINE nl_set_auxhist1_end
SUBROUTINE nl_set_io_form_auxhist1 ( id_id , io_form_auxhist1 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist1
  INTEGER id_id
  model_config_rec%io_form_auxhist1 = io_form_auxhist1 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist1
SUBROUTINE nl_set_frames_per_auxhist1 ( id_id , frames_per_auxhist1 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist1
  INTEGER id_id
  model_config_rec%frames_per_auxhist1(id_id) = frames_per_auxhist1
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist1
SUBROUTINE nl_set_auxhist2_inname ( id_id , auxhist2_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist2_inname
  INTEGER id_id
  model_config_rec%auxhist2_inname = trim(auxhist2_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist2_inname
SUBROUTINE nl_set_auxhist2_outname ( id_id , auxhist2_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist2_outname
  INTEGER id_id
  model_config_rec%auxhist2_outname = trim(auxhist2_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist2_outname
SUBROUTINE nl_set_auxhist2_interval_y ( id_id , auxhist2_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_interval_y
  INTEGER id_id
  model_config_rec%auxhist2_interval_y(id_id) = auxhist2_interval_y
  RETURN
END SUBROUTINE nl_set_auxhist2_interval_y
SUBROUTINE nl_set_auxhist2_interval_d ( id_id , auxhist2_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_interval_d
  INTEGER id_id
  model_config_rec%auxhist2_interval_d(id_id) = auxhist2_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist2_interval_d
SUBROUTINE nl_set_auxhist2_interval_h ( id_id , auxhist2_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_interval_h
  INTEGER id_id
  model_config_rec%auxhist2_interval_h(id_id) = auxhist2_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist2_interval_h
SUBROUTINE nl_set_auxhist2_interval_m ( id_id , auxhist2_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_interval_m
  INTEGER id_id
  model_config_rec%auxhist2_interval_m(id_id) = auxhist2_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist2_interval_m
SUBROUTINE nl_set_auxhist2_interval_s ( id_id , auxhist2_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_interval_s
  INTEGER id_id
  model_config_rec%auxhist2_interval_s(id_id) = auxhist2_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist2_interval_s
SUBROUTINE nl_set_auxhist2_interval ( id_id , auxhist2_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_interval
  INTEGER id_id
  model_config_rec%auxhist2_interval(id_id) = auxhist2_interval
  RETURN
END SUBROUTINE nl_set_auxhist2_interval
SUBROUTINE nl_set_auxhist2_begin_y ( id_id , auxhist2_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_begin_y
  INTEGER id_id
  model_config_rec%auxhist2_begin_y(id_id) = auxhist2_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist2_begin_y
SUBROUTINE nl_set_auxhist2_begin_d ( id_id , auxhist2_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_begin_d
  INTEGER id_id
  model_config_rec%auxhist2_begin_d(id_id) = auxhist2_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist2_begin_d
SUBROUTINE nl_set_auxhist2_begin_h ( id_id , auxhist2_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_begin_h
  INTEGER id_id
  model_config_rec%auxhist2_begin_h(id_id) = auxhist2_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist2_begin_h
SUBROUTINE nl_set_auxhist2_begin_m ( id_id , auxhist2_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_begin_m
  INTEGER id_id
  model_config_rec%auxhist2_begin_m(id_id) = auxhist2_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist2_begin_m
SUBROUTINE nl_set_auxhist2_begin_s ( id_id , auxhist2_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_begin_s
  INTEGER id_id
  model_config_rec%auxhist2_begin_s(id_id) = auxhist2_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist2_begin_s
SUBROUTINE nl_set_auxhist2_begin ( id_id , auxhist2_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_begin
  INTEGER id_id
  model_config_rec%auxhist2_begin(id_id) = auxhist2_begin
  RETURN
END SUBROUTINE nl_set_auxhist2_begin
SUBROUTINE nl_set_auxhist2_end_y ( id_id , auxhist2_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_end_y
  INTEGER id_id
  model_config_rec%auxhist2_end_y(id_id) = auxhist2_end_y
  RETURN
END SUBROUTINE nl_set_auxhist2_end_y
SUBROUTINE nl_set_auxhist2_end_d ( id_id , auxhist2_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_end_d
  INTEGER id_id
  model_config_rec%auxhist2_end_d(id_id) = auxhist2_end_d
  RETURN
END SUBROUTINE nl_set_auxhist2_end_d
SUBROUTINE nl_set_auxhist2_end_h ( id_id , auxhist2_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_end_h
  INTEGER id_id
  model_config_rec%auxhist2_end_h(id_id) = auxhist2_end_h
  RETURN
END SUBROUTINE nl_set_auxhist2_end_h
SUBROUTINE nl_set_auxhist2_end_m ( id_id , auxhist2_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_end_m
  INTEGER id_id
  model_config_rec%auxhist2_end_m(id_id) = auxhist2_end_m
  RETURN
END SUBROUTINE nl_set_auxhist2_end_m
SUBROUTINE nl_set_auxhist2_end_s ( id_id , auxhist2_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_end_s
  INTEGER id_id
  model_config_rec%auxhist2_end_s(id_id) = auxhist2_end_s
  RETURN
END SUBROUTINE nl_set_auxhist2_end_s
SUBROUTINE nl_set_auxhist2_end ( id_id , auxhist2_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_end
  INTEGER id_id
  model_config_rec%auxhist2_end(id_id) = auxhist2_end
  RETURN
END SUBROUTINE nl_set_auxhist2_end
SUBROUTINE nl_set_io_form_auxhist2 ( id_id , io_form_auxhist2 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist2
  INTEGER id_id
  model_config_rec%io_form_auxhist2 = io_form_auxhist2 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist2
SUBROUTINE nl_set_frames_per_auxhist2 ( id_id , frames_per_auxhist2 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist2
  INTEGER id_id
  model_config_rec%frames_per_auxhist2(id_id) = frames_per_auxhist2
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist2
SUBROUTINE nl_set_auxhist3_inname ( id_id , auxhist3_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist3_inname
  INTEGER id_id
  model_config_rec%auxhist3_inname = trim(auxhist3_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist3_inname
SUBROUTINE nl_set_auxhist3_outname ( id_id , auxhist3_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist3_outname
  INTEGER id_id
  model_config_rec%auxhist3_outname = trim(auxhist3_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist3_outname
SUBROUTINE nl_set_auxhist3_interval_y ( id_id , auxhist3_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_interval_y
  INTEGER id_id
  model_config_rec%auxhist3_interval_y(id_id) = auxhist3_interval_y
  RETURN
END SUBROUTINE nl_set_auxhist3_interval_y
SUBROUTINE nl_set_auxhist3_interval_d ( id_id , auxhist3_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_interval_d
  INTEGER id_id
  model_config_rec%auxhist3_interval_d(id_id) = auxhist3_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist3_interval_d
SUBROUTINE nl_set_auxhist3_interval_h ( id_id , auxhist3_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_interval_h
  INTEGER id_id
  model_config_rec%auxhist3_interval_h(id_id) = auxhist3_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist3_interval_h
SUBROUTINE nl_set_auxhist3_interval_m ( id_id , auxhist3_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_interval_m
  INTEGER id_id
  model_config_rec%auxhist3_interval_m(id_id) = auxhist3_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist3_interval_m
SUBROUTINE nl_set_auxhist3_interval_s ( id_id , auxhist3_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_interval_s
  INTEGER id_id
  model_config_rec%auxhist3_interval_s(id_id) = auxhist3_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist3_interval_s
SUBROUTINE nl_set_auxhist3_interval ( id_id , auxhist3_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_interval
  INTEGER id_id
  model_config_rec%auxhist3_interval(id_id) = auxhist3_interval
  RETURN
END SUBROUTINE nl_set_auxhist3_interval
SUBROUTINE nl_set_auxhist3_begin_y ( id_id , auxhist3_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_begin_y
  INTEGER id_id
  model_config_rec%auxhist3_begin_y(id_id) = auxhist3_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist3_begin_y
SUBROUTINE nl_set_auxhist3_begin_d ( id_id , auxhist3_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_begin_d
  INTEGER id_id
  model_config_rec%auxhist3_begin_d(id_id) = auxhist3_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist3_begin_d
SUBROUTINE nl_set_auxhist3_begin_h ( id_id , auxhist3_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_begin_h
  INTEGER id_id
  model_config_rec%auxhist3_begin_h(id_id) = auxhist3_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist3_begin_h
SUBROUTINE nl_set_auxhist3_begin_m ( id_id , auxhist3_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_begin_m
  INTEGER id_id
  model_config_rec%auxhist3_begin_m(id_id) = auxhist3_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist3_begin_m
SUBROUTINE nl_set_auxhist3_begin_s ( id_id , auxhist3_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_begin_s
  INTEGER id_id
  model_config_rec%auxhist3_begin_s(id_id) = auxhist3_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist3_begin_s
SUBROUTINE nl_set_auxhist3_begin ( id_id , auxhist3_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_begin
  INTEGER id_id
  model_config_rec%auxhist3_begin(id_id) = auxhist3_begin
  RETURN
END SUBROUTINE nl_set_auxhist3_begin
SUBROUTINE nl_set_auxhist3_end_y ( id_id , auxhist3_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_end_y
  INTEGER id_id
  model_config_rec%auxhist3_end_y(id_id) = auxhist3_end_y
  RETURN
END SUBROUTINE nl_set_auxhist3_end_y
SUBROUTINE nl_set_auxhist3_end_d ( id_id , auxhist3_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_end_d
  INTEGER id_id
  model_config_rec%auxhist3_end_d(id_id) = auxhist3_end_d
  RETURN
END SUBROUTINE nl_set_auxhist3_end_d
SUBROUTINE nl_set_auxhist3_end_h ( id_id , auxhist3_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_end_h
  INTEGER id_id
  model_config_rec%auxhist3_end_h(id_id) = auxhist3_end_h
  RETURN
END SUBROUTINE nl_set_auxhist3_end_h
SUBROUTINE nl_set_auxhist3_end_m ( id_id , auxhist3_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_end_m
  INTEGER id_id
  model_config_rec%auxhist3_end_m(id_id) = auxhist3_end_m
  RETURN
END SUBROUTINE nl_set_auxhist3_end_m
SUBROUTINE nl_set_auxhist3_end_s ( id_id , auxhist3_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_end_s
  INTEGER id_id
  model_config_rec%auxhist3_end_s(id_id) = auxhist3_end_s
  RETURN
END SUBROUTINE nl_set_auxhist3_end_s
SUBROUTINE nl_set_auxhist3_end ( id_id , auxhist3_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_end
  INTEGER id_id
  model_config_rec%auxhist3_end(id_id) = auxhist3_end
  RETURN
END SUBROUTINE nl_set_auxhist3_end
SUBROUTINE nl_set_io_form_auxhist3 ( id_id , io_form_auxhist3 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist3
  INTEGER id_id
  model_config_rec%io_form_auxhist3 = io_form_auxhist3 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist3
SUBROUTINE nl_set_frames_per_auxhist3 ( id_id , frames_per_auxhist3 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist3
  INTEGER id_id
  model_config_rec%frames_per_auxhist3(id_id) = frames_per_auxhist3
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist3
SUBROUTINE nl_set_auxhist4_inname ( id_id , auxhist4_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist4_inname
  INTEGER id_id
  model_config_rec%auxhist4_inname = trim(auxhist4_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist4_inname
SUBROUTINE nl_set_auxhist4_outname ( id_id , auxhist4_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist4_outname
  INTEGER id_id
  model_config_rec%auxhist4_outname = trim(auxhist4_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist4_outname
SUBROUTINE nl_set_auxhist4_interval_y ( id_id , auxhist4_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_interval_y
  INTEGER id_id
  model_config_rec%auxhist4_interval_y(id_id) = auxhist4_interval_y
  RETURN
END SUBROUTINE nl_set_auxhist4_interval_y
SUBROUTINE nl_set_auxhist4_interval_d ( id_id , auxhist4_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_interval_d
  INTEGER id_id
  model_config_rec%auxhist4_interval_d(id_id) = auxhist4_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist4_interval_d
SUBROUTINE nl_set_auxhist4_interval_h ( id_id , auxhist4_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_interval_h
  INTEGER id_id
  model_config_rec%auxhist4_interval_h(id_id) = auxhist4_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist4_interval_h
SUBROUTINE nl_set_auxhist4_interval_m ( id_id , auxhist4_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_interval_m
  INTEGER id_id
  model_config_rec%auxhist4_interval_m(id_id) = auxhist4_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist4_interval_m
SUBROUTINE nl_set_auxhist4_interval_s ( id_id , auxhist4_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_interval_s
  INTEGER id_id
  model_config_rec%auxhist4_interval_s(id_id) = auxhist4_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist4_interval_s
SUBROUTINE nl_set_auxhist4_interval ( id_id , auxhist4_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_interval
  INTEGER id_id
  model_config_rec%auxhist4_interval(id_id) = auxhist4_interval
  RETURN
END SUBROUTINE nl_set_auxhist4_interval
SUBROUTINE nl_set_auxhist4_begin_y ( id_id , auxhist4_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_begin_y
  INTEGER id_id
  model_config_rec%auxhist4_begin_y(id_id) = auxhist4_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist4_begin_y
SUBROUTINE nl_set_auxhist4_begin_d ( id_id , auxhist4_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_begin_d
  INTEGER id_id
  model_config_rec%auxhist4_begin_d(id_id) = auxhist4_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist4_begin_d
SUBROUTINE nl_set_auxhist4_begin_h ( id_id , auxhist4_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_begin_h
  INTEGER id_id
  model_config_rec%auxhist4_begin_h(id_id) = auxhist4_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist4_begin_h
SUBROUTINE nl_set_auxhist4_begin_m ( id_id , auxhist4_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_begin_m
  INTEGER id_id
  model_config_rec%auxhist4_begin_m(id_id) = auxhist4_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist4_begin_m
SUBROUTINE nl_set_auxhist4_begin_s ( id_id , auxhist4_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_begin_s
  INTEGER id_id
  model_config_rec%auxhist4_begin_s(id_id) = auxhist4_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist4_begin_s
SUBROUTINE nl_set_auxhist4_begin ( id_id , auxhist4_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_begin
  INTEGER id_id
  model_config_rec%auxhist4_begin(id_id) = auxhist4_begin
  RETURN
END SUBROUTINE nl_set_auxhist4_begin
SUBROUTINE nl_set_auxhist4_end_y ( id_id , auxhist4_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_end_y
  INTEGER id_id
  model_config_rec%auxhist4_end_y(id_id) = auxhist4_end_y
  RETURN
END SUBROUTINE nl_set_auxhist4_end_y
SUBROUTINE nl_set_auxhist4_end_d ( id_id , auxhist4_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_end_d
  INTEGER id_id
  model_config_rec%auxhist4_end_d(id_id) = auxhist4_end_d
  RETURN
END SUBROUTINE nl_set_auxhist4_end_d
SUBROUTINE nl_set_auxhist4_end_h ( id_id , auxhist4_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_end_h
  INTEGER id_id
  model_config_rec%auxhist4_end_h(id_id) = auxhist4_end_h
  RETURN
END SUBROUTINE nl_set_auxhist4_end_h
SUBROUTINE nl_set_auxhist4_end_m ( id_id , auxhist4_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_end_m
  INTEGER id_id
  model_config_rec%auxhist4_end_m(id_id) = auxhist4_end_m
  RETURN
END SUBROUTINE nl_set_auxhist4_end_m
SUBROUTINE nl_set_auxhist4_end_s ( id_id , auxhist4_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_end_s
  INTEGER id_id
  model_config_rec%auxhist4_end_s(id_id) = auxhist4_end_s
  RETURN
END SUBROUTINE nl_set_auxhist4_end_s
SUBROUTINE nl_set_auxhist4_end ( id_id , auxhist4_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_end
  INTEGER id_id
  model_config_rec%auxhist4_end(id_id) = auxhist4_end
  RETURN
END SUBROUTINE nl_set_auxhist4_end
SUBROUTINE nl_set_io_form_auxhist4 ( id_id , io_form_auxhist4 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist4
  INTEGER id_id
  model_config_rec%io_form_auxhist4 = io_form_auxhist4 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist4
SUBROUTINE nl_set_frames_per_auxhist4 ( id_id , frames_per_auxhist4 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist4
  INTEGER id_id
  model_config_rec%frames_per_auxhist4(id_id) = frames_per_auxhist4
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist4
SUBROUTINE nl_set_auxhist5_inname ( id_id , auxhist5_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist5_inname
  INTEGER id_id
  model_config_rec%auxhist5_inname = trim(auxhist5_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist5_inname
SUBROUTINE nl_set_auxhist5_outname ( id_id , auxhist5_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist5_outname
  INTEGER id_id
  model_config_rec%auxhist5_outname = trim(auxhist5_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist5_outname
!ENDOFREGISTRYGENERATEDINCLUDE
