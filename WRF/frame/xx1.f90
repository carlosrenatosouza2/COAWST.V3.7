










!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nl_config.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
SUBROUTINE nl_set_shcu_physics ( id_id , shcu_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: shcu_physics
  INTEGER id_id
  model_config_rec%shcu_physics(id_id) = shcu_physics
  RETURN
END SUBROUTINE nl_set_shcu_physics
SUBROUTINE nl_set_cu_diag ( id_id , cu_diag )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: cu_diag
  INTEGER id_id
  model_config_rec%cu_diag(id_id) = cu_diag
  RETURN
END SUBROUTINE nl_set_cu_diag
SUBROUTINE nl_set_kf_edrates ( id_id , kf_edrates )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: kf_edrates
  INTEGER id_id
  model_config_rec%kf_edrates(id_id) = kf_edrates
  RETURN
END SUBROUTINE nl_set_kf_edrates
SUBROUTINE nl_set_kfeta_trigger ( id_id , kfeta_trigger )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: kfeta_trigger
  INTEGER id_id
  model_config_rec%kfeta_trigger = kfeta_trigger 
  RETURN
END SUBROUTINE nl_set_kfeta_trigger
SUBROUTINE nl_set_nsas_dx_factor ( id_id , nsas_dx_factor )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: nsas_dx_factor
  INTEGER id_id
  model_config_rec%nsas_dx_factor = nsas_dx_factor 
  RETURN
END SUBROUTINE nl_set_nsas_dx_factor
SUBROUTINE nl_set_cudt ( id_id , cudt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: cudt
  INTEGER id_id
  model_config_rec%cudt(id_id) = cudt
  RETURN
END SUBROUTINE nl_set_cudt
SUBROUTINE nl_set_gsmdt ( id_id , gsmdt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: gsmdt
  INTEGER id_id
  model_config_rec%gsmdt(id_id) = gsmdt
  RETURN
END SUBROUTINE nl_set_gsmdt
SUBROUTINE nl_set_isfflx ( id_id , isfflx )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: isfflx
  INTEGER id_id
  model_config_rec%isfflx = isfflx 
  RETURN
END SUBROUTINE nl_set_isfflx
SUBROUTINE nl_set_ifsnow ( id_id , ifsnow )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: ifsnow
  INTEGER id_id
  model_config_rec%ifsnow = ifsnow 
  RETURN
END SUBROUTINE nl_set_ifsnow
SUBROUTINE nl_set_icloud ( id_id , icloud )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: icloud
  INTEGER id_id
  model_config_rec%icloud = icloud 
  RETURN
END SUBROUTINE nl_set_icloud
SUBROUTINE nl_set_cldovrlp ( id_id , cldovrlp )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: cldovrlp
  INTEGER id_id
  model_config_rec%cldovrlp = cldovrlp 
  RETURN
END SUBROUTINE nl_set_cldovrlp
SUBROUTINE nl_set_ideal_xland ( id_id , ideal_xland )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: ideal_xland
  INTEGER id_id
  model_config_rec%ideal_xland = ideal_xland 
  RETURN
END SUBROUTINE nl_set_ideal_xland
SUBROUTINE nl_set_swrad_scat ( id_id , swrad_scat )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: swrad_scat
  INTEGER id_id
  model_config_rec%swrad_scat = swrad_scat 
  RETURN
END SUBROUTINE nl_set_swrad_scat
SUBROUTINE nl_set_surface_input_source ( id_id , surface_input_source )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: surface_input_source
  INTEGER id_id
  model_config_rec%surface_input_source = surface_input_source 
  RETURN
END SUBROUTINE nl_set_surface_input_source
SUBROUTINE nl_set_num_soil_layers ( id_id , num_soil_layers )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_soil_layers
  INTEGER id_id
  model_config_rec%num_soil_layers = num_soil_layers 
  RETURN
END SUBROUTINE nl_set_num_soil_layers
SUBROUTINE nl_set_maxpatch ( id_id , maxpatch )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: maxpatch
  INTEGER id_id
  model_config_rec%maxpatch = maxpatch 
  RETURN
END SUBROUTINE nl_set_maxpatch
SUBROUTINE nl_set_num_snow_layers ( id_id , num_snow_layers )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_snow_layers
  INTEGER id_id
  model_config_rec%num_snow_layers = num_snow_layers 
  RETURN
END SUBROUTINE nl_set_num_snow_layers
SUBROUTINE nl_set_num_snso_layers ( id_id , num_snso_layers )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_snso_layers
  INTEGER id_id
  model_config_rec%num_snso_layers = num_snso_layers 
  RETURN
END SUBROUTINE nl_set_num_snso_layers
SUBROUTINE nl_set_num_urban_ndm ( id_id , num_urban_ndm )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_urban_ndm
  INTEGER id_id
  model_config_rec%num_urban_ndm = num_urban_ndm 
  RETURN
END SUBROUTINE nl_set_num_urban_ndm
SUBROUTINE nl_set_num_urban_ng ( id_id , num_urban_ng )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_urban_ng
  INTEGER id_id
  model_config_rec%num_urban_ng = num_urban_ng 
  RETURN
END SUBROUTINE nl_set_num_urban_ng
SUBROUTINE nl_set_num_urban_nwr ( id_id , num_urban_nwr )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_urban_nwr
  INTEGER id_id
  model_config_rec%num_urban_nwr = num_urban_nwr 
  RETURN
END SUBROUTINE nl_set_num_urban_nwr
SUBROUTINE nl_set_num_urban_ngb ( id_id , num_urban_ngb )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_urban_ngb
  INTEGER id_id
  model_config_rec%num_urban_ngb = num_urban_ngb 
  RETURN
END SUBROUTINE nl_set_num_urban_ngb
SUBROUTINE nl_set_num_urban_nf ( id_id , num_urban_nf )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_urban_nf
  INTEGER id_id
  model_config_rec%num_urban_nf = num_urban_nf 
  RETURN
END SUBROUTINE nl_set_num_urban_nf
SUBROUTINE nl_set_num_urban_nz ( id_id , num_urban_nz )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_urban_nz
  INTEGER id_id
  model_config_rec%num_urban_nz = num_urban_nz 
  RETURN
END SUBROUTINE nl_set_num_urban_nz
SUBROUTINE nl_set_num_urban_nbui ( id_id , num_urban_nbui )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_urban_nbui
  INTEGER id_id
  model_config_rec%num_urban_nbui = num_urban_nbui 
  RETURN
END SUBROUTINE nl_set_num_urban_nbui
SUBROUTINE nl_set_urban_map_zrd ( id_id , urban_map_zrd )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: urban_map_zrd
  INTEGER id_id
  model_config_rec%urban_map_zrd = urban_map_zrd 
  RETURN
END SUBROUTINE nl_set_urban_map_zrd
SUBROUTINE nl_set_urban_map_zwd ( id_id , urban_map_zwd )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: urban_map_zwd
  INTEGER id_id
  model_config_rec%urban_map_zwd = urban_map_zwd 
  RETURN
END SUBROUTINE nl_set_urban_map_zwd
SUBROUTINE nl_set_urban_map_gd ( id_id , urban_map_gd )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: urban_map_gd
  INTEGER id_id
  model_config_rec%urban_map_gd = urban_map_gd 
  RETURN
END SUBROUTINE nl_set_urban_map_gd
SUBROUTINE nl_set_urban_map_zd ( id_id , urban_map_zd )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: urban_map_zd
  INTEGER id_id
  model_config_rec%urban_map_zd = urban_map_zd 
  RETURN
END SUBROUTINE nl_set_urban_map_zd
SUBROUTINE nl_set_urban_map_zdf ( id_id , urban_map_zdf )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: urban_map_zdf
  INTEGER id_id
  model_config_rec%urban_map_zdf = urban_map_zdf 
  RETURN
END SUBROUTINE nl_set_urban_map_zdf
SUBROUTINE nl_set_urban_map_bd ( id_id , urban_map_bd )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: urban_map_bd
  INTEGER id_id
  model_config_rec%urban_map_bd = urban_map_bd 
  RETURN
END SUBROUTINE nl_set_urban_map_bd
SUBROUTINE nl_set_urban_map_wd ( id_id , urban_map_wd )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: urban_map_wd
  INTEGER id_id
  model_config_rec%urban_map_wd = urban_map_wd 
  RETURN
END SUBROUTINE nl_set_urban_map_wd
SUBROUTINE nl_set_urban_map_gbd ( id_id , urban_map_gbd )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: urban_map_gbd
  INTEGER id_id
  model_config_rec%urban_map_gbd = urban_map_gbd 
  RETURN
END SUBROUTINE nl_set_urban_map_gbd
SUBROUTINE nl_set_urban_map_fbd ( id_id , urban_map_fbd )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: urban_map_fbd
  INTEGER id_id
  model_config_rec%urban_map_fbd = urban_map_fbd 
  RETURN
END SUBROUTINE nl_set_urban_map_fbd
SUBROUTINE nl_set_num_urban_hi ( id_id , num_urban_hi )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_urban_hi
  INTEGER id_id
  model_config_rec%num_urban_hi = num_urban_hi 
  RETURN
END SUBROUTINE nl_set_num_urban_hi
SUBROUTINE nl_set_num_months ( id_id , num_months )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_months
  INTEGER id_id
  model_config_rec%num_months = num_months 
  RETURN
END SUBROUTINE nl_set_num_months
SUBROUTINE nl_set_sf_surface_mosaic ( id_id , sf_surface_mosaic )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sf_surface_mosaic
  INTEGER id_id
  model_config_rec%sf_surface_mosaic = sf_surface_mosaic 
  RETURN
END SUBROUTINE nl_set_sf_surface_mosaic
SUBROUTINE nl_set_mosaic_cat ( id_id , mosaic_cat )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: mosaic_cat
  INTEGER id_id
  model_config_rec%mosaic_cat = mosaic_cat 
  RETURN
END SUBROUTINE nl_set_mosaic_cat
SUBROUTINE nl_set_mosaic_cat_soil ( id_id , mosaic_cat_soil )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: mosaic_cat_soil
  INTEGER id_id
  model_config_rec%mosaic_cat_soil = mosaic_cat_soil 
  RETURN
END SUBROUTINE nl_set_mosaic_cat_soil
SUBROUTINE nl_set_mosaic_lu ( id_id , mosaic_lu )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: mosaic_lu
  INTEGER id_id
  model_config_rec%mosaic_lu = mosaic_lu 
  RETURN
END SUBROUTINE nl_set_mosaic_lu
SUBROUTINE nl_set_mosaic_soil ( id_id , mosaic_soil )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: mosaic_soil
  INTEGER id_id
  model_config_rec%mosaic_soil = mosaic_soil 
  RETURN
END SUBROUTINE nl_set_mosaic_soil
SUBROUTINE nl_set_flag_sm_adj ( id_id , flag_sm_adj )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_sm_adj
  INTEGER id_id
  model_config_rec%flag_sm_adj = flag_sm_adj 
  RETURN
END SUBROUTINE nl_set_flag_sm_adj
SUBROUTINE nl_set_maxiens ( id_id , maxiens )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: maxiens
  INTEGER id_id
  model_config_rec%maxiens = maxiens 
  RETURN
END SUBROUTINE nl_set_maxiens
SUBROUTINE nl_set_maxens ( id_id , maxens )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: maxens
  INTEGER id_id
  model_config_rec%maxens = maxens 
  RETURN
END SUBROUTINE nl_set_maxens
SUBROUTINE nl_set_maxens2 ( id_id , maxens2 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: maxens2
  INTEGER id_id
  model_config_rec%maxens2 = maxens2 
  RETURN
END SUBROUTINE nl_set_maxens2
SUBROUTINE nl_set_maxens3 ( id_id , maxens3 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: maxens3
  INTEGER id_id
  model_config_rec%maxens3 = maxens3 
  RETURN
END SUBROUTINE nl_set_maxens3
SUBROUTINE nl_set_ensdim ( id_id , ensdim )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: ensdim
  INTEGER id_id
  model_config_rec%ensdim = ensdim 
  RETURN
END SUBROUTINE nl_set_ensdim
SUBROUTINE nl_set_cugd_avedx ( id_id , cugd_avedx )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: cugd_avedx
  INTEGER id_id
  model_config_rec%cugd_avedx = cugd_avedx 
  RETURN
END SUBROUTINE nl_set_cugd_avedx
SUBROUTINE nl_set_clos_choice ( id_id , clos_choice )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: clos_choice
  INTEGER id_id
  model_config_rec%clos_choice = clos_choice 
  RETURN
END SUBROUTINE nl_set_clos_choice
SUBROUTINE nl_set_imomentum ( id_id , imomentum )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: imomentum
  INTEGER id_id
  model_config_rec%imomentum = imomentum 
  RETURN
END SUBROUTINE nl_set_imomentum
SUBROUTINE nl_set_ishallow ( id_id , ishallow )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: ishallow
  INTEGER id_id
  model_config_rec%ishallow = ishallow 
  RETURN
END SUBROUTINE nl_set_ishallow
SUBROUTINE nl_set_convtrans_avglen_m ( id_id , convtrans_avglen_m )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: convtrans_avglen_m
  INTEGER id_id
  model_config_rec%convtrans_avglen_m = convtrans_avglen_m 
  RETURN
END SUBROUTINE nl_set_convtrans_avglen_m
SUBROUTINE nl_set_num_land_cat ( id_id , num_land_cat )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_land_cat
  INTEGER id_id
  model_config_rec%num_land_cat = num_land_cat 
  RETURN
END SUBROUTINE nl_set_num_land_cat
SUBROUTINE nl_set_num_soil_cat ( id_id , num_soil_cat )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_soil_cat
  INTEGER id_id
  model_config_rec%num_soil_cat = num_soil_cat 
  RETURN
END SUBROUTINE nl_set_num_soil_cat
SUBROUTINE nl_set_mp_zero_out ( id_id , mp_zero_out )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: mp_zero_out
  INTEGER id_id
  model_config_rec%mp_zero_out = mp_zero_out 
  RETURN
END SUBROUTINE nl_set_mp_zero_out
SUBROUTINE nl_set_mp_zero_out_thresh ( id_id , mp_zero_out_thresh )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: mp_zero_out_thresh
  INTEGER id_id
  model_config_rec%mp_zero_out_thresh = mp_zero_out_thresh 
  RETURN
END SUBROUTINE nl_set_mp_zero_out_thresh
SUBROUTINE nl_set_seaice_threshold ( id_id , seaice_threshold )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: seaice_threshold
  INTEGER id_id
  model_config_rec%seaice_threshold = seaice_threshold 
  RETURN
END SUBROUTINE nl_set_seaice_threshold
SUBROUTINE nl_set_bmj_rad_feedback ( id_id , bmj_rad_feedback )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: bmj_rad_feedback
  INTEGER id_id
  model_config_rec%bmj_rad_feedback(id_id) = bmj_rad_feedback
  RETURN
END SUBROUTINE nl_set_bmj_rad_feedback
SUBROUTINE nl_set_sst_update ( id_id , sst_update )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sst_update
  INTEGER id_id
  model_config_rec%sst_update = sst_update 
  RETURN
END SUBROUTINE nl_set_sst_update
SUBROUTINE nl_set_sst_skin ( id_id , sst_skin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sst_skin
  INTEGER id_id
  model_config_rec%sst_skin = sst_skin 
  RETURN
END SUBROUTINE nl_set_sst_skin
SUBROUTINE nl_set_tmn_update ( id_id , tmn_update )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: tmn_update
  INTEGER id_id
  model_config_rec%tmn_update = tmn_update 
  RETURN
END SUBROUTINE nl_set_tmn_update
SUBROUTINE nl_set_usemonalb ( id_id , usemonalb )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: usemonalb
  INTEGER id_id
  model_config_rec%usemonalb = usemonalb 
  RETURN
END SUBROUTINE nl_set_usemonalb
SUBROUTINE nl_set_rdmaxalb ( id_id , rdmaxalb )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: rdmaxalb
  INTEGER id_id
  model_config_rec%rdmaxalb = rdmaxalb 
  RETURN
END SUBROUTINE nl_set_rdmaxalb
SUBROUTINE nl_set_rdlai2d ( id_id , rdlai2d )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: rdlai2d
  INTEGER id_id
  model_config_rec%rdlai2d = rdlai2d 
  RETURN
END SUBROUTINE nl_set_rdlai2d
SUBROUTINE nl_set_ua_phys ( id_id , ua_phys )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: ua_phys
  INTEGER id_id
  model_config_rec%ua_phys = ua_phys 
  RETURN
END SUBROUTINE nl_set_ua_phys
SUBROUTINE nl_set_opt_thcnd ( id_id , opt_thcnd )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: opt_thcnd
  INTEGER id_id
  model_config_rec%opt_thcnd = opt_thcnd 
  RETURN
END SUBROUTINE nl_set_opt_thcnd
SUBROUTINE nl_set_co2tf ( id_id , co2tf )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: co2tf
  INTEGER id_id
  model_config_rec%co2tf = co2tf 
  RETURN
END SUBROUTINE nl_set_co2tf
SUBROUTINE nl_set_ra_call_offset ( id_id , ra_call_offset )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: ra_call_offset
  INTEGER id_id
  model_config_rec%ra_call_offset = ra_call_offset 
  RETURN
END SUBROUTINE nl_set_ra_call_offset
SUBROUTINE nl_set_cam_abs_freq_s ( id_id , cam_abs_freq_s )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: cam_abs_freq_s
  INTEGER id_id
  model_config_rec%cam_abs_freq_s = cam_abs_freq_s 
  RETURN
END SUBROUTINE nl_set_cam_abs_freq_s
SUBROUTINE nl_set_levsiz ( id_id , levsiz )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: levsiz
  INTEGER id_id
  model_config_rec%levsiz = levsiz 
  RETURN
END SUBROUTINE nl_set_levsiz
SUBROUTINE nl_set_paerlev ( id_id , paerlev )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: paerlev
  INTEGER id_id
  model_config_rec%paerlev = paerlev 
  RETURN
END SUBROUTINE nl_set_paerlev
SUBROUTINE nl_set_cam_abs_dim1 ( id_id , cam_abs_dim1 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: cam_abs_dim1
  INTEGER id_id
  model_config_rec%cam_abs_dim1 = cam_abs_dim1 
  RETURN
END SUBROUTINE nl_set_cam_abs_dim1
SUBROUTINE nl_set_cam_abs_dim2 ( id_id , cam_abs_dim2 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: cam_abs_dim2
  INTEGER id_id
  model_config_rec%cam_abs_dim2 = cam_abs_dim2 
  RETURN
END SUBROUTINE nl_set_cam_abs_dim2
SUBROUTINE nl_set_lagday ( id_id , lagday )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: lagday
  INTEGER id_id
  model_config_rec%lagday = lagday 
  RETURN
END SUBROUTINE nl_set_lagday
SUBROUTINE nl_set_no_src_types ( id_id , no_src_types )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: no_src_types
  INTEGER id_id
  model_config_rec%no_src_types = no_src_types 
  RETURN
END SUBROUTINE nl_set_no_src_types
SUBROUTINE nl_set_alevsiz ( id_id , alevsiz )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: alevsiz
  INTEGER id_id
  model_config_rec%alevsiz = alevsiz 
  RETURN
END SUBROUTINE nl_set_alevsiz
SUBROUTINE nl_set_o3input ( id_id , o3input )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: o3input
  INTEGER id_id
  model_config_rec%o3input = o3input 
  RETURN
END SUBROUTINE nl_set_o3input
SUBROUTINE nl_set_aer_opt ( id_id , aer_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: aer_opt
  INTEGER id_id
  model_config_rec%aer_opt = aer_opt 
  RETURN
END SUBROUTINE nl_set_aer_opt
SUBROUTINE nl_set_swint_opt ( id_id , swint_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: swint_opt
  INTEGER id_id
  model_config_rec%swint_opt = swint_opt 
  RETURN
END SUBROUTINE nl_set_swint_opt
SUBROUTINE nl_set_aer_type ( id_id , aer_type )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: aer_type
  INTEGER id_id
  model_config_rec%aer_type(id_id) = aer_type
  RETURN
END SUBROUTINE nl_set_aer_type
SUBROUTINE nl_set_aer_aod550_opt ( id_id , aer_aod550_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: aer_aod550_opt
  INTEGER id_id
  model_config_rec%aer_aod550_opt(id_id) = aer_aod550_opt
  RETURN
END SUBROUTINE nl_set_aer_aod550_opt
SUBROUTINE nl_set_aer_angexp_opt ( id_id , aer_angexp_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: aer_angexp_opt
  INTEGER id_id
  model_config_rec%aer_angexp_opt(id_id) = aer_angexp_opt
  RETURN
END SUBROUTINE nl_set_aer_angexp_opt
SUBROUTINE nl_set_aer_ssa_opt ( id_id , aer_ssa_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: aer_ssa_opt
  INTEGER id_id
  model_config_rec%aer_ssa_opt(id_id) = aer_ssa_opt
  RETURN
END SUBROUTINE nl_set_aer_ssa_opt
SUBROUTINE nl_set_aer_asy_opt ( id_id , aer_asy_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: aer_asy_opt
  INTEGER id_id
  model_config_rec%aer_asy_opt(id_id) = aer_asy_opt
  RETURN
END SUBROUTINE nl_set_aer_asy_opt
SUBROUTINE nl_set_aer_aod550_val ( id_id , aer_aod550_val )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: aer_aod550_val
  INTEGER id_id
  model_config_rec%aer_aod550_val(id_id) = aer_aod550_val
  RETURN
END SUBROUTINE nl_set_aer_aod550_val
SUBROUTINE nl_set_aer_angexp_val ( id_id , aer_angexp_val )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: aer_angexp_val
  INTEGER id_id
  model_config_rec%aer_angexp_val(id_id) = aer_angexp_val
  RETURN
END SUBROUTINE nl_set_aer_angexp_val
SUBROUTINE nl_set_aer_ssa_val ( id_id , aer_ssa_val )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: aer_ssa_val
  INTEGER id_id
  model_config_rec%aer_ssa_val(id_id) = aer_ssa_val
  RETURN
END SUBROUTINE nl_set_aer_ssa_val
SUBROUTINE nl_set_aer_asy_val ( id_id , aer_asy_val )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: aer_asy_val
  INTEGER id_id
  model_config_rec%aer_asy_val(id_id) = aer_asy_val
  RETURN
END SUBROUTINE nl_set_aer_asy_val
SUBROUTINE nl_set_cu_rad_feedback ( id_id , cu_rad_feedback )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: cu_rad_feedback
  INTEGER id_id
  model_config_rec%cu_rad_feedback(id_id) = cu_rad_feedback
  RETURN
END SUBROUTINE nl_set_cu_rad_feedback
SUBROUTINE nl_set_dust_emis ( id_id , dust_emis )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dust_emis
  INTEGER id_id
  model_config_rec%dust_emis = dust_emis 
  RETURN
END SUBROUTINE nl_set_dust_emis
SUBROUTINE nl_set_erosion_dim ( id_id , erosion_dim )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: erosion_dim
  INTEGER id_id
  model_config_rec%erosion_dim = erosion_dim 
  RETURN
END SUBROUTINE nl_set_erosion_dim
SUBROUTINE nl_set_no_src_types_cu ( id_id , no_src_types_cu )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: no_src_types_cu
  INTEGER id_id
  model_config_rec%no_src_types_cu = no_src_types_cu 
  RETURN
END SUBROUTINE nl_set_no_src_types_cu
SUBROUTINE nl_set_alevsiz_cu ( id_id , alevsiz_cu )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: alevsiz_cu
  INTEGER id_id
  model_config_rec%alevsiz_cu = alevsiz_cu 
  RETURN
END SUBROUTINE nl_set_alevsiz_cu
SUBROUTINE nl_set_aercu_opt ( id_id , aercu_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: aercu_opt
  INTEGER id_id
  model_config_rec%aercu_opt = aercu_opt 
  RETURN
END SUBROUTINE nl_set_aercu_opt
SUBROUTINE nl_set_aercu_fct ( id_id , aercu_fct )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: aercu_fct
  INTEGER id_id
  model_config_rec%aercu_fct = aercu_fct 
  RETURN
END SUBROUTINE nl_set_aercu_fct
SUBROUTINE nl_set_aercu_used ( id_id , aercu_used )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: aercu_used
  INTEGER id_id
  model_config_rec%aercu_used = aercu_used 
  RETURN
END SUBROUTINE nl_set_aercu_used
SUBROUTINE nl_set_shallowcu_forced_ra ( id_id , shallowcu_forced_ra )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: shallowcu_forced_ra
  INTEGER id_id
  model_config_rec%shallowcu_forced_ra(id_id) = shallowcu_forced_ra
  RETURN
END SUBROUTINE nl_set_shallowcu_forced_ra
SUBROUTINE nl_set_numbins ( id_id , numbins )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: numbins
  INTEGER id_id
  model_config_rec%numbins(id_id) = numbins
  RETURN
END SUBROUTINE nl_set_numbins
SUBROUTINE nl_set_thbinsize ( id_id , thbinsize )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: thbinsize
  INTEGER id_id
  model_config_rec%thbinsize(id_id) = thbinsize
  RETURN
END SUBROUTINE nl_set_thbinsize
SUBROUTINE nl_set_rbinsize ( id_id , rbinsize )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: rbinsize
  INTEGER id_id
  model_config_rec%rbinsize(id_id) = rbinsize
  RETURN
END SUBROUTINE nl_set_rbinsize
SUBROUTINE nl_set_mindeepfreq ( id_id , mindeepfreq )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: mindeepfreq
  INTEGER id_id
  model_config_rec%mindeepfreq(id_id) = mindeepfreq
  RETURN
END SUBROUTINE nl_set_mindeepfreq
SUBROUTINE nl_set_minshallowfreq ( id_id , minshallowfreq )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: minshallowfreq
  INTEGER id_id
  model_config_rec%minshallowfreq(id_id) = minshallowfreq
  RETURN
END SUBROUTINE nl_set_minshallowfreq
SUBROUTINE nl_set_shcu_aerosols_opt ( id_id , shcu_aerosols_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: shcu_aerosols_opt
  INTEGER id_id
  model_config_rec%shcu_aerosols_opt(id_id) = shcu_aerosols_opt
  RETURN
END SUBROUTINE nl_set_shcu_aerosols_opt
SUBROUTINE nl_set_icloud_cu ( id_id , icloud_cu )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: icloud_cu
  INTEGER id_id
  model_config_rec%icloud_cu(id_id) = icloud_cu
  RETURN
END SUBROUTINE nl_set_icloud_cu
SUBROUTINE nl_set_pxlsm_smois_init ( id_id , pxlsm_smois_init )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: pxlsm_smois_init
  INTEGER id_id
  model_config_rec%pxlsm_smois_init(id_id) = pxlsm_smois_init
  RETURN
END SUBROUTINE nl_set_pxlsm_smois_init
SUBROUTINE nl_set_pxlsm_modis_veg ( id_id , pxlsm_modis_veg )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: pxlsm_modis_veg
  INTEGER id_id
  model_config_rec%pxlsm_modis_veg(id_id) = pxlsm_modis_veg
  RETURN
END SUBROUTINE nl_set_pxlsm_modis_veg
SUBROUTINE nl_set_omlcall ( id_id , omlcall )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: omlcall
  INTEGER id_id
  model_config_rec%omlcall = omlcall 
  RETURN
END SUBROUTINE nl_set_omlcall
SUBROUTINE nl_set_sf_ocean_physics ( id_id , sf_ocean_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sf_ocean_physics
  INTEGER id_id
  model_config_rec%sf_ocean_physics = sf_ocean_physics 
  RETURN
END SUBROUTINE nl_set_sf_ocean_physics
SUBROUTINE nl_set_traj_opt ( id_id , traj_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: traj_opt
  INTEGER id_id
  model_config_rec%traj_opt = traj_opt 
  RETURN
END SUBROUTINE nl_set_traj_opt
SUBROUTINE nl_set_dm_has_traj ( id_id , dm_has_traj )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: dm_has_traj
  INTEGER id_id
  model_config_rec%dm_has_traj(id_id) = dm_has_traj
  RETURN
END SUBROUTINE nl_set_dm_has_traj
SUBROUTINE nl_set_tracercall ( id_id , tracercall )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: tracercall
  INTEGER id_id
  model_config_rec%tracercall = tracercall 
  RETURN
END SUBROUTINE nl_set_tracercall
SUBROUTINE nl_set_omdt ( id_id , omdt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: omdt
  INTEGER id_id
  model_config_rec%omdt = omdt 
  RETURN
END SUBROUTINE nl_set_omdt
SUBROUTINE nl_set_oml_hml0 ( id_id , oml_hml0 )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: oml_hml0
  INTEGER id_id
  model_config_rec%oml_hml0 = oml_hml0 
  RETURN
END SUBROUTINE nl_set_oml_hml0
SUBROUTINE nl_set_oml_gamma ( id_id , oml_gamma )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: oml_gamma
  INTEGER id_id
  model_config_rec%oml_gamma = oml_gamma 
  RETURN
END SUBROUTINE nl_set_oml_gamma
SUBROUTINE nl_set_oml_relaxation_time ( id_id , oml_relaxation_time )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: oml_relaxation_time
  INTEGER id_id
  model_config_rec%oml_relaxation_time = oml_relaxation_time 
  RETURN
END SUBROUTINE nl_set_oml_relaxation_time
SUBROUTINE nl_set_isftcflx ( id_id , isftcflx )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: isftcflx
  INTEGER id_id
  model_config_rec%isftcflx = isftcflx 
  RETURN
END SUBROUTINE nl_set_isftcflx
SUBROUTINE nl_set_iz0tlnd ( id_id , iz0tlnd )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: iz0tlnd
  INTEGER id_id
  model_config_rec%iz0tlnd = iz0tlnd 
  RETURN
END SUBROUTINE nl_set_iz0tlnd
SUBROUTINE nl_set_shadlen ( id_id , shadlen )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: shadlen
  INTEGER id_id
  model_config_rec%shadlen = shadlen 
  RETURN
END SUBROUTINE nl_set_shadlen
SUBROUTINE nl_set_slope_rad ( id_id , slope_rad )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: slope_rad
  INTEGER id_id
  model_config_rec%slope_rad(id_id) = slope_rad
  RETURN
END SUBROUTINE nl_set_slope_rad
SUBROUTINE nl_set_topo_shading ( id_id , topo_shading )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: topo_shading
  INTEGER id_id
  model_config_rec%topo_shading(id_id) = topo_shading
  RETURN
END SUBROUTINE nl_set_topo_shading
SUBROUTINE nl_set_topo_wind ( id_id , topo_wind )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: topo_wind
  INTEGER id_id
  model_config_rec%topo_wind(id_id) = topo_wind
  RETURN
END SUBROUTINE nl_set_topo_wind
SUBROUTINE nl_set_no_mp_heating ( id_id , no_mp_heating )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: no_mp_heating
  INTEGER id_id
  model_config_rec%no_mp_heating = no_mp_heating 
  RETURN
END SUBROUTINE nl_set_no_mp_heating
SUBROUTINE nl_set_fractional_seaice ( id_id , fractional_seaice )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: fractional_seaice
  INTEGER id_id
  model_config_rec%fractional_seaice = fractional_seaice 
  RETURN
END SUBROUTINE nl_set_fractional_seaice
SUBROUTINE nl_set_seaice_snowdepth_opt ( id_id , seaice_snowdepth_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: seaice_snowdepth_opt
  INTEGER id_id
  model_config_rec%seaice_snowdepth_opt = seaice_snowdepth_opt 
  RETURN
END SUBROUTINE nl_set_seaice_snowdepth_opt
SUBROUTINE nl_set_seaice_snowdepth_max ( id_id , seaice_snowdepth_max )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: seaice_snowdepth_max
  INTEGER id_id
  model_config_rec%seaice_snowdepth_max = seaice_snowdepth_max 
  RETURN
END SUBROUTINE nl_set_seaice_snowdepth_max
SUBROUTINE nl_set_seaice_snowdepth_min ( id_id , seaice_snowdepth_min )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: seaice_snowdepth_min
  INTEGER id_id
  model_config_rec%seaice_snowdepth_min = seaice_snowdepth_min 
  RETURN
END SUBROUTINE nl_set_seaice_snowdepth_min
SUBROUTINE nl_set_seaice_albedo_opt ( id_id , seaice_albedo_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: seaice_albedo_opt
  INTEGER id_id
  model_config_rec%seaice_albedo_opt = seaice_albedo_opt 
  RETURN
END SUBROUTINE nl_set_seaice_albedo_opt
SUBROUTINE nl_set_seaice_albedo_default ( id_id , seaice_albedo_default )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: seaice_albedo_default
  INTEGER id_id
  model_config_rec%seaice_albedo_default = seaice_albedo_default 
  RETURN
END SUBROUTINE nl_set_seaice_albedo_default
SUBROUTINE nl_set_seaice_thickness_opt ( id_id , seaice_thickness_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: seaice_thickness_opt
  INTEGER id_id
  model_config_rec%seaice_thickness_opt = seaice_thickness_opt 
  RETURN
END SUBROUTINE nl_set_seaice_thickness_opt
SUBROUTINE nl_set_seaice_thickness_default ( id_id , seaice_thickness_default )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: seaice_thickness_default
  INTEGER id_id
  model_config_rec%seaice_thickness_default = seaice_thickness_default 
  RETURN
END SUBROUTINE nl_set_seaice_thickness_default
SUBROUTINE nl_set_tice2tsk_if2cold ( id_id , tice2tsk_if2cold )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: tice2tsk_if2cold
  INTEGER id_id
  model_config_rec%tice2tsk_if2cold = tice2tsk_if2cold 
  RETURN
END SUBROUTINE nl_set_tice2tsk_if2cold
SUBROUTINE nl_set_bucket_mm ( id_id , bucket_mm )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: bucket_mm
  INTEGER id_id
  model_config_rec%bucket_mm = bucket_mm 
  RETURN
END SUBROUTINE nl_set_bucket_mm
SUBROUTINE nl_set_bucket_j ( id_id , bucket_j )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: bucket_j
  INTEGER id_id
  model_config_rec%bucket_j = bucket_j 
  RETURN
END SUBROUTINE nl_set_bucket_j
SUBROUTINE nl_set_mp_tend_lim ( id_id , mp_tend_lim )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: mp_tend_lim
  INTEGER id_id
  model_config_rec%mp_tend_lim = mp_tend_lim 
  RETURN
END SUBROUTINE nl_set_mp_tend_lim
SUBROUTINE nl_set_prec_acc_dt ( id_id , prec_acc_dt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: prec_acc_dt
  INTEGER id_id
  model_config_rec%prec_acc_dt(id_id) = prec_acc_dt
  RETURN
END SUBROUTINE nl_set_prec_acc_dt
SUBROUTINE nl_set_prec_acc_opt ( id_id , prec_acc_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: prec_acc_opt
  INTEGER id_id
  model_config_rec%prec_acc_opt = prec_acc_opt 
  RETURN
END SUBROUTINE nl_set_prec_acc_opt
SUBROUTINE nl_set_bucketr_opt ( id_id , bucketr_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: bucketr_opt
  INTEGER id_id
  model_config_rec%bucketr_opt = bucketr_opt 
  RETURN
END SUBROUTINE nl_set_bucketr_opt
SUBROUTINE nl_set_bucketf_opt ( id_id , bucketf_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: bucketf_opt
  INTEGER id_id
  model_config_rec%bucketf_opt = bucketf_opt 
  RETURN
END SUBROUTINE nl_set_bucketf_opt
SUBROUTINE nl_set_process_time_series ( id_id , process_time_series )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: process_time_series
  INTEGER id_id
  model_config_rec%process_time_series = process_time_series 
  RETURN
END SUBROUTINE nl_set_process_time_series
SUBROUTINE nl_set_grav_settling ( id_id , grav_settling )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: grav_settling
  INTEGER id_id
  model_config_rec%grav_settling(id_id) = grav_settling
  RETURN
END SUBROUTINE nl_set_grav_settling
SUBROUTINE nl_set_sas_pgcon ( id_id , sas_pgcon )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: sas_pgcon
  INTEGER id_id
  model_config_rec%sas_pgcon(id_id) = sas_pgcon
  RETURN
END SUBROUTINE nl_set_sas_pgcon
SUBROUTINE nl_set_scalar_pblmix ( id_id , scalar_pblmix )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: scalar_pblmix
  INTEGER id_id
  model_config_rec%scalar_pblmix(id_id) = scalar_pblmix
  RETURN
END SUBROUTINE nl_set_scalar_pblmix
SUBROUTINE nl_set_tracer_pblmix ( id_id , tracer_pblmix )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: tracer_pblmix
  INTEGER id_id
  model_config_rec%tracer_pblmix(id_id) = tracer_pblmix
  RETURN
END SUBROUTINE nl_set_tracer_pblmix
SUBROUTINE nl_set_use_aero_icbc ( id_id , use_aero_icbc )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: use_aero_icbc
  INTEGER id_id
  model_config_rec%use_aero_icbc = use_aero_icbc 
  RETURN
END SUBROUTINE nl_set_use_aero_icbc
SUBROUTINE nl_set_use_rap_aero_icbc ( id_id , use_rap_aero_icbc )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: use_rap_aero_icbc
  INTEGER id_id
  model_config_rec%use_rap_aero_icbc = use_rap_aero_icbc 
  RETURN
END SUBROUTINE nl_set_use_rap_aero_icbc
SUBROUTINE nl_set_use_mp_re ( id_id , use_mp_re )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: use_mp_re
  INTEGER id_id
  model_config_rec%use_mp_re = use_mp_re 
  RETURN
END SUBROUTINE nl_set_use_mp_re
SUBROUTINE nl_set_ccn_conc ( id_id , ccn_conc )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: ccn_conc
  INTEGER id_id
  model_config_rec%ccn_conc = ccn_conc 
  RETURN
END SUBROUTINE nl_set_ccn_conc
SUBROUTINE nl_set_hail_opt ( id_id , hail_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: hail_opt
  INTEGER id_id
  model_config_rec%hail_opt = hail_opt 
  RETURN
END SUBROUTINE nl_set_hail_opt
SUBROUTINE nl_set_morr_rimed_ice ( id_id , morr_rimed_ice )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: morr_rimed_ice
  INTEGER id_id
  model_config_rec%morr_rimed_ice = morr_rimed_ice 
  RETURN
END SUBROUTINE nl_set_morr_rimed_ice
SUBROUTINE nl_set_clean_atm_diag ( id_id , clean_atm_diag )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: clean_atm_diag
  INTEGER id_id
  model_config_rec%clean_atm_diag = clean_atm_diag 
  RETURN
END SUBROUTINE nl_set_clean_atm_diag
SUBROUTINE nl_set_calc_clean_atm_diag ( id_id , calc_clean_atm_diag )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: calc_clean_atm_diag
  INTEGER id_id
  model_config_rec%calc_clean_atm_diag = calc_clean_atm_diag 
  RETURN
END SUBROUTINE nl_set_calc_clean_atm_diag
SUBROUTINE nl_set_dveg ( id_id , dveg )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dveg
  INTEGER id_id
  model_config_rec%dveg = dveg 
  RETURN
END SUBROUTINE nl_set_dveg
SUBROUTINE nl_set_opt_crs ( id_id , opt_crs )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: opt_crs
  INTEGER id_id
  model_config_rec%opt_crs = opt_crs 
  RETURN
END SUBROUTINE nl_set_opt_crs
SUBROUTINE nl_set_opt_btr ( id_id , opt_btr )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: opt_btr
  INTEGER id_id
  model_config_rec%opt_btr = opt_btr 
  RETURN
END SUBROUTINE nl_set_opt_btr
SUBROUTINE nl_set_opt_run ( id_id , opt_run )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: opt_run
  INTEGER id_id
  model_config_rec%opt_run = opt_run 
  RETURN
END SUBROUTINE nl_set_opt_run
SUBROUTINE nl_set_opt_sfc ( id_id , opt_sfc )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: opt_sfc
  INTEGER id_id
  model_config_rec%opt_sfc = opt_sfc 
  RETURN
END SUBROUTINE nl_set_opt_sfc
SUBROUTINE nl_set_opt_frz ( id_id , opt_frz )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: opt_frz
  INTEGER id_id
  model_config_rec%opt_frz = opt_frz 
  RETURN
END SUBROUTINE nl_set_opt_frz
SUBROUTINE nl_set_opt_inf ( id_id , opt_inf )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: opt_inf
  INTEGER id_id
  model_config_rec%opt_inf = opt_inf 
  RETURN
END SUBROUTINE nl_set_opt_inf
SUBROUTINE nl_set_opt_rad ( id_id , opt_rad )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: opt_rad
  INTEGER id_id
  model_config_rec%opt_rad = opt_rad 
  RETURN
END SUBROUTINE nl_set_opt_rad
SUBROUTINE nl_set_opt_alb ( id_id , opt_alb )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: opt_alb
  INTEGER id_id
  model_config_rec%opt_alb = opt_alb 
  RETURN
END SUBROUTINE nl_set_opt_alb
SUBROUTINE nl_set_opt_snf ( id_id , opt_snf )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: opt_snf
  INTEGER id_id
  model_config_rec%opt_snf = opt_snf 
  RETURN
END SUBROUTINE nl_set_opt_snf
SUBROUTINE nl_set_opt_tbot ( id_id , opt_tbot )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: opt_tbot
  INTEGER id_id
  model_config_rec%opt_tbot = opt_tbot 
  RETURN
END SUBROUTINE nl_set_opt_tbot
SUBROUTINE nl_set_opt_stc ( id_id , opt_stc )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: opt_stc
  INTEGER id_id
  model_config_rec%opt_stc = opt_stc 
  RETURN
END SUBROUTINE nl_set_opt_stc
SUBROUTINE nl_set_opt_gla ( id_id , opt_gla )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: opt_gla
  INTEGER id_id
  model_config_rec%opt_gla = opt_gla 
  RETURN
END SUBROUTINE nl_set_opt_gla
SUBROUTINE nl_set_opt_rsf ( id_id , opt_rsf )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: opt_rsf
  INTEGER id_id
  model_config_rec%opt_rsf = opt_rsf 
  RETURN
END SUBROUTINE nl_set_opt_rsf
SUBROUTINE nl_set_opt_soil ( id_id , opt_soil )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: opt_soil
  INTEGER id_id
  model_config_rec%opt_soil = opt_soil 
  RETURN
END SUBROUTINE nl_set_opt_soil
SUBROUTINE nl_set_opt_pedo ( id_id , opt_pedo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: opt_pedo
  INTEGER id_id
  model_config_rec%opt_pedo = opt_pedo 
  RETURN
END SUBROUTINE nl_set_opt_pedo
SUBROUTINE nl_set_opt_crop ( id_id , opt_crop )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: opt_crop
  INTEGER id_id
  model_config_rec%opt_crop = opt_crop 
  RETURN
END SUBROUTINE nl_set_opt_crop
SUBROUTINE nl_set_wtddt ( id_id , wtddt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: wtddt
  INTEGER id_id
  model_config_rec%wtddt(id_id) = wtddt
  RETURN
END SUBROUTINE nl_set_wtddt
SUBROUTINE nl_set_wrf_hydro ( id_id , wrf_hydro )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: wrf_hydro
  INTEGER id_id
  model_config_rec%wrf_hydro = wrf_hydro 
  RETURN
END SUBROUTINE nl_set_wrf_hydro
SUBROUTINE nl_set_fgdt ( id_id , fgdt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: fgdt
  INTEGER id_id
  model_config_rec%fgdt(id_id) = fgdt
  RETURN
END SUBROUTINE nl_set_fgdt
SUBROUTINE nl_set_fgdtzero ( id_id , fgdtzero )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: fgdtzero
  INTEGER id_id
  model_config_rec%fgdtzero(id_id) = fgdtzero
  RETURN
END SUBROUTINE nl_set_fgdtzero
SUBROUTINE nl_set_grid_fdda ( id_id , grid_fdda )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: grid_fdda
  INTEGER id_id
  model_config_rec%grid_fdda(id_id) = grid_fdda
  RETURN
END SUBROUTINE nl_set_grid_fdda
SUBROUTINE nl_set_grid_sfdda ( id_id , grid_sfdda )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: grid_sfdda
  INTEGER id_id
  model_config_rec%grid_sfdda(id_id) = grid_sfdda
  RETURN
END SUBROUTINE nl_set_grid_sfdda
SUBROUTINE nl_set_if_no_pbl_nudging_uv ( id_id , if_no_pbl_nudging_uv )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: if_no_pbl_nudging_uv
  INTEGER id_id
  model_config_rec%if_no_pbl_nudging_uv(id_id) = if_no_pbl_nudging_uv
  RETURN
END SUBROUTINE nl_set_if_no_pbl_nudging_uv
SUBROUTINE nl_set_if_no_pbl_nudging_t ( id_id , if_no_pbl_nudging_t )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: if_no_pbl_nudging_t
  INTEGER id_id
  model_config_rec%if_no_pbl_nudging_t(id_id) = if_no_pbl_nudging_t
  RETURN
END SUBROUTINE nl_set_if_no_pbl_nudging_t
SUBROUTINE nl_set_if_no_pbl_nudging_ph ( id_id , if_no_pbl_nudging_ph )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: if_no_pbl_nudging_ph
  INTEGER id_id
  model_config_rec%if_no_pbl_nudging_ph(id_id) = if_no_pbl_nudging_ph
  RETURN
END SUBROUTINE nl_set_if_no_pbl_nudging_ph
SUBROUTINE nl_set_if_no_pbl_nudging_q ( id_id , if_no_pbl_nudging_q )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: if_no_pbl_nudging_q
  INTEGER id_id
  model_config_rec%if_no_pbl_nudging_q(id_id) = if_no_pbl_nudging_q
  RETURN
END SUBROUTINE nl_set_if_no_pbl_nudging_q
SUBROUTINE nl_set_if_zfac_uv ( id_id , if_zfac_uv )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: if_zfac_uv
  INTEGER id_id
  model_config_rec%if_zfac_uv(id_id) = if_zfac_uv
  RETURN
END SUBROUTINE nl_set_if_zfac_uv
SUBROUTINE nl_set_k_zfac_uv ( id_id , k_zfac_uv )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: k_zfac_uv
  INTEGER id_id
  model_config_rec%k_zfac_uv(id_id) = k_zfac_uv
  RETURN
END SUBROUTINE nl_set_k_zfac_uv
SUBROUTINE nl_set_if_zfac_t ( id_id , if_zfac_t )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: if_zfac_t
  INTEGER id_id
  model_config_rec%if_zfac_t(id_id) = if_zfac_t
  RETURN
END SUBROUTINE nl_set_if_zfac_t
SUBROUTINE nl_set_k_zfac_t ( id_id , k_zfac_t )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: k_zfac_t
  INTEGER id_id
  model_config_rec%k_zfac_t(id_id) = k_zfac_t
  RETURN
END SUBROUTINE nl_set_k_zfac_t
SUBROUTINE nl_set_if_zfac_ph ( id_id , if_zfac_ph )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: if_zfac_ph
  INTEGER id_id
  model_config_rec%if_zfac_ph(id_id) = if_zfac_ph
  RETURN
END SUBROUTINE nl_set_if_zfac_ph
SUBROUTINE nl_set_k_zfac_ph ( id_id , k_zfac_ph )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: k_zfac_ph
  INTEGER id_id
  model_config_rec%k_zfac_ph(id_id) = k_zfac_ph
  RETURN
END SUBROUTINE nl_set_k_zfac_ph
SUBROUTINE nl_set_if_zfac_q ( id_id , if_zfac_q )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: if_zfac_q
  INTEGER id_id
  model_config_rec%if_zfac_q(id_id) = if_zfac_q
  RETURN
END SUBROUTINE nl_set_if_zfac_q
SUBROUTINE nl_set_k_zfac_q ( id_id , k_zfac_q )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: k_zfac_q
  INTEGER id_id
  model_config_rec%k_zfac_q(id_id) = k_zfac_q
  RETURN
END SUBROUTINE nl_set_k_zfac_q
SUBROUTINE nl_set_dk_zfac_uv ( id_id , dk_zfac_uv )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dk_zfac_uv
  INTEGER id_id
  model_config_rec%dk_zfac_uv(id_id) = dk_zfac_uv
  RETURN
END SUBROUTINE nl_set_dk_zfac_uv
SUBROUTINE nl_set_dk_zfac_t ( id_id , dk_zfac_t )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dk_zfac_t
  INTEGER id_id
  model_config_rec%dk_zfac_t(id_id) = dk_zfac_t
  RETURN
END SUBROUTINE nl_set_dk_zfac_t
SUBROUTINE nl_set_dk_zfac_ph ( id_id , dk_zfac_ph )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dk_zfac_ph
  INTEGER id_id
  model_config_rec%dk_zfac_ph(id_id) = dk_zfac_ph
  RETURN
END SUBROUTINE nl_set_dk_zfac_ph
SUBROUTINE nl_set_dk_zfac_q ( id_id , dk_zfac_q )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dk_zfac_q
  INTEGER id_id
  model_config_rec%dk_zfac_q(id_id) = dk_zfac_q
  RETURN
END SUBROUTINE nl_set_dk_zfac_q
SUBROUTINE nl_set_ktrop ( id_id , ktrop )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: ktrop
  INTEGER id_id
  model_config_rec%ktrop = ktrop 
  RETURN
END SUBROUTINE nl_set_ktrop
SUBROUTINE nl_set_guv ( id_id , guv )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: guv
  INTEGER id_id
  model_config_rec%guv(id_id) = guv
  RETURN
END SUBROUTINE nl_set_guv
SUBROUTINE nl_set_guv_sfc ( id_id , guv_sfc )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: guv_sfc
  INTEGER id_id
  model_config_rec%guv_sfc(id_id) = guv_sfc
  RETURN
END SUBROUTINE nl_set_guv_sfc
SUBROUTINE nl_set_gt ( id_id , gt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: gt
  INTEGER id_id
  model_config_rec%gt(id_id) = gt
  RETURN
END SUBROUTINE nl_set_gt
SUBROUTINE nl_set_gt_sfc ( id_id , gt_sfc )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: gt_sfc
  INTEGER id_id
  model_config_rec%gt_sfc(id_id) = gt_sfc
  RETURN
END SUBROUTINE nl_set_gt_sfc
SUBROUTINE nl_set_gq ( id_id , gq )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: gq
  INTEGER id_id
  model_config_rec%gq(id_id) = gq
  RETURN
END SUBROUTINE nl_set_gq
SUBROUTINE nl_set_gq_sfc ( id_id , gq_sfc )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: gq_sfc
  INTEGER id_id
  model_config_rec%gq_sfc(id_id) = gq_sfc
  RETURN
END SUBROUTINE nl_set_gq_sfc
SUBROUTINE nl_set_gph ( id_id , gph )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: gph
  INTEGER id_id
  model_config_rec%gph(id_id) = gph
  RETURN
END SUBROUTINE nl_set_gph
SUBROUTINE nl_set_dtramp_min ( id_id , dtramp_min )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: dtramp_min
  INTEGER id_id
  model_config_rec%dtramp_min = dtramp_min 
  RETURN
END SUBROUTINE nl_set_dtramp_min
SUBROUTINE nl_set_if_ramping ( id_id , if_ramping )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: if_ramping
  INTEGER id_id
  model_config_rec%if_ramping = if_ramping 
  RETURN
END SUBROUTINE nl_set_if_ramping
SUBROUTINE nl_set_rinblw ( id_id , rinblw )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: rinblw
  INTEGER id_id
  model_config_rec%rinblw(id_id) = rinblw
  RETURN
END SUBROUTINE nl_set_rinblw
SUBROUTINE nl_set_xwavenum ( id_id , xwavenum )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: xwavenum
  INTEGER id_id
  model_config_rec%xwavenum(id_id) = xwavenum
  RETURN
END SUBROUTINE nl_set_xwavenum
SUBROUTINE nl_set_ywavenum ( id_id , ywavenum )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: ywavenum
  INTEGER id_id
  model_config_rec%ywavenum(id_id) = ywavenum
  RETURN
END SUBROUTINE nl_set_ywavenum
SUBROUTINE nl_set_pxlsm_soil_nudge ( id_id , pxlsm_soil_nudge )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: pxlsm_soil_nudge
  INTEGER id_id
  model_config_rec%pxlsm_soil_nudge(id_id) = pxlsm_soil_nudge
  RETURN
END SUBROUTINE nl_set_pxlsm_soil_nudge
SUBROUTINE nl_set_fasdas ( id_id , fasdas )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: fasdas
  INTEGER id_id
  model_config_rec%fasdas(id_id) = fasdas
  RETURN
END SUBROUTINE nl_set_fasdas
SUBROUTINE nl_set_obs_nudge_opt ( id_id , obs_nudge_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: obs_nudge_opt
  INTEGER id_id
  model_config_rec%obs_nudge_opt(id_id) = obs_nudge_opt
  RETURN
END SUBROUTINE nl_set_obs_nudge_opt
SUBROUTINE nl_set_max_obs ( id_id , max_obs )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: max_obs
  INTEGER id_id
  model_config_rec%max_obs = max_obs 
  RETURN
END SUBROUTINE nl_set_max_obs
SUBROUTINE nl_set_fdda_start ( id_id , fdda_start )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: fdda_start
  INTEGER id_id
  model_config_rec%fdda_start(id_id) = fdda_start
  RETURN
END SUBROUTINE nl_set_fdda_start
SUBROUTINE nl_set_fdda_end ( id_id , fdda_end )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: fdda_end
  INTEGER id_id
  model_config_rec%fdda_end(id_id) = fdda_end
  RETURN
END SUBROUTINE nl_set_fdda_end
SUBROUTINE nl_set_obs_nudge_wind ( id_id , obs_nudge_wind )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: obs_nudge_wind
  INTEGER id_id
  model_config_rec%obs_nudge_wind(id_id) = obs_nudge_wind
  RETURN
END SUBROUTINE nl_set_obs_nudge_wind
SUBROUTINE nl_set_obs_coef_wind ( id_id , obs_coef_wind )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_coef_wind
  INTEGER id_id
  model_config_rec%obs_coef_wind(id_id) = obs_coef_wind
  RETURN
END SUBROUTINE nl_set_obs_coef_wind
SUBROUTINE nl_set_obs_nudge_temp ( id_id , obs_nudge_temp )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: obs_nudge_temp
  INTEGER id_id
  model_config_rec%obs_nudge_temp(id_id) = obs_nudge_temp
  RETURN
END SUBROUTINE nl_set_obs_nudge_temp
SUBROUTINE nl_set_obs_coef_temp ( id_id , obs_coef_temp )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_coef_temp
  INTEGER id_id
  model_config_rec%obs_coef_temp(id_id) = obs_coef_temp
  RETURN
END SUBROUTINE nl_set_obs_coef_temp
SUBROUTINE nl_set_obs_nudge_mois ( id_id , obs_nudge_mois )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: obs_nudge_mois
  INTEGER id_id
  model_config_rec%obs_nudge_mois(id_id) = obs_nudge_mois
  RETURN
END SUBROUTINE nl_set_obs_nudge_mois
SUBROUTINE nl_set_obs_coef_mois ( id_id , obs_coef_mois )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_coef_mois
  INTEGER id_id
  model_config_rec%obs_coef_mois(id_id) = obs_coef_mois
  RETURN
END SUBROUTINE nl_set_obs_coef_mois
SUBROUTINE nl_set_obs_nudge_pstr ( id_id , obs_nudge_pstr )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: obs_nudge_pstr
  INTEGER id_id
  model_config_rec%obs_nudge_pstr(id_id) = obs_nudge_pstr
  RETURN
END SUBROUTINE nl_set_obs_nudge_pstr
SUBROUTINE nl_set_obs_coef_pstr ( id_id , obs_coef_pstr )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_coef_pstr
  INTEGER id_id
  model_config_rec%obs_coef_pstr(id_id) = obs_coef_pstr
  RETURN
END SUBROUTINE nl_set_obs_coef_pstr
SUBROUTINE nl_set_obs_no_pbl_nudge_uv ( id_id , obs_no_pbl_nudge_uv )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: obs_no_pbl_nudge_uv
  INTEGER id_id
  model_config_rec%obs_no_pbl_nudge_uv(id_id) = obs_no_pbl_nudge_uv
  RETURN
END SUBROUTINE nl_set_obs_no_pbl_nudge_uv
SUBROUTINE nl_set_obs_no_pbl_nudge_t ( id_id , obs_no_pbl_nudge_t )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: obs_no_pbl_nudge_t
  INTEGER id_id
  model_config_rec%obs_no_pbl_nudge_t(id_id) = obs_no_pbl_nudge_t
  RETURN
END SUBROUTINE nl_set_obs_no_pbl_nudge_t
SUBROUTINE nl_set_obs_no_pbl_nudge_q ( id_id , obs_no_pbl_nudge_q )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: obs_no_pbl_nudge_q
  INTEGER id_id
  model_config_rec%obs_no_pbl_nudge_q(id_id) = obs_no_pbl_nudge_q
  RETURN
END SUBROUTINE nl_set_obs_no_pbl_nudge_q
SUBROUTINE nl_set_obs_sfc_scheme_horiz ( id_id , obs_sfc_scheme_horiz )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: obs_sfc_scheme_horiz
  INTEGER id_id
  model_config_rec%obs_sfc_scheme_horiz = obs_sfc_scheme_horiz 
  RETURN
END SUBROUTINE nl_set_obs_sfc_scheme_horiz
SUBROUTINE nl_set_obs_sfc_scheme_vert ( id_id , obs_sfc_scheme_vert )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: obs_sfc_scheme_vert
  INTEGER id_id
  model_config_rec%obs_sfc_scheme_vert = obs_sfc_scheme_vert 
  RETURN
END SUBROUTINE nl_set_obs_sfc_scheme_vert
SUBROUTINE nl_set_obs_max_sndng_gap ( id_id , obs_max_sndng_gap )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_max_sndng_gap
  INTEGER id_id
  model_config_rec%obs_max_sndng_gap = obs_max_sndng_gap 
  RETURN
END SUBROUTINE nl_set_obs_max_sndng_gap
SUBROUTINE nl_set_obs_nudgezfullr1_uv ( id_id , obs_nudgezfullr1_uv )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_nudgezfullr1_uv
  INTEGER id_id
  model_config_rec%obs_nudgezfullr1_uv = obs_nudgezfullr1_uv 
  RETURN
END SUBROUTINE nl_set_obs_nudgezfullr1_uv
SUBROUTINE nl_set_obs_nudgezrampr1_uv ( id_id , obs_nudgezrampr1_uv )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_nudgezrampr1_uv
  INTEGER id_id
  model_config_rec%obs_nudgezrampr1_uv = obs_nudgezrampr1_uv 
  RETURN
END SUBROUTINE nl_set_obs_nudgezrampr1_uv
SUBROUTINE nl_set_obs_nudgezfullr2_uv ( id_id , obs_nudgezfullr2_uv )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_nudgezfullr2_uv
  INTEGER id_id
  model_config_rec%obs_nudgezfullr2_uv = obs_nudgezfullr2_uv 
  RETURN
END SUBROUTINE nl_set_obs_nudgezfullr2_uv
SUBROUTINE nl_set_obs_nudgezrampr2_uv ( id_id , obs_nudgezrampr2_uv )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_nudgezrampr2_uv
  INTEGER id_id
  model_config_rec%obs_nudgezrampr2_uv = obs_nudgezrampr2_uv 
  RETURN
END SUBROUTINE nl_set_obs_nudgezrampr2_uv
SUBROUTINE nl_set_obs_nudgezfullr4_uv ( id_id , obs_nudgezfullr4_uv )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_nudgezfullr4_uv
  INTEGER id_id
  model_config_rec%obs_nudgezfullr4_uv = obs_nudgezfullr4_uv 
  RETURN
END SUBROUTINE nl_set_obs_nudgezfullr4_uv
SUBROUTINE nl_set_obs_nudgezrampr4_uv ( id_id , obs_nudgezrampr4_uv )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_nudgezrampr4_uv
  INTEGER id_id
  model_config_rec%obs_nudgezrampr4_uv = obs_nudgezrampr4_uv 
  RETURN
END SUBROUTINE nl_set_obs_nudgezrampr4_uv
SUBROUTINE nl_set_obs_nudgezfullr1_t ( id_id , obs_nudgezfullr1_t )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_nudgezfullr1_t
  INTEGER id_id
  model_config_rec%obs_nudgezfullr1_t = obs_nudgezfullr1_t 
  RETURN
END SUBROUTINE nl_set_obs_nudgezfullr1_t
SUBROUTINE nl_set_obs_nudgezrampr1_t ( id_id , obs_nudgezrampr1_t )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_nudgezrampr1_t
  INTEGER id_id
  model_config_rec%obs_nudgezrampr1_t = obs_nudgezrampr1_t 
  RETURN
END SUBROUTINE nl_set_obs_nudgezrampr1_t
SUBROUTINE nl_set_obs_nudgezfullr2_t ( id_id , obs_nudgezfullr2_t )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_nudgezfullr2_t
  INTEGER id_id
  model_config_rec%obs_nudgezfullr2_t = obs_nudgezfullr2_t 
  RETURN
END SUBROUTINE nl_set_obs_nudgezfullr2_t
SUBROUTINE nl_set_obs_nudgezrampr2_t ( id_id , obs_nudgezrampr2_t )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_nudgezrampr2_t
  INTEGER id_id
  model_config_rec%obs_nudgezrampr2_t = obs_nudgezrampr2_t 
  RETURN
END SUBROUTINE nl_set_obs_nudgezrampr2_t
SUBROUTINE nl_set_obs_nudgezfullr4_t ( id_id , obs_nudgezfullr4_t )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_nudgezfullr4_t
  INTEGER id_id
  model_config_rec%obs_nudgezfullr4_t = obs_nudgezfullr4_t 
  RETURN
END SUBROUTINE nl_set_obs_nudgezfullr4_t
SUBROUTINE nl_set_obs_nudgezrampr4_t ( id_id , obs_nudgezrampr4_t )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_nudgezrampr4_t
  INTEGER id_id
  model_config_rec%obs_nudgezrampr4_t = obs_nudgezrampr4_t 
  RETURN
END SUBROUTINE nl_set_obs_nudgezrampr4_t
SUBROUTINE nl_set_obs_nudgezfullr1_q ( id_id , obs_nudgezfullr1_q )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_nudgezfullr1_q
  INTEGER id_id
  model_config_rec%obs_nudgezfullr1_q = obs_nudgezfullr1_q 
  RETURN
END SUBROUTINE nl_set_obs_nudgezfullr1_q
SUBROUTINE nl_set_obs_nudgezrampr1_q ( id_id , obs_nudgezrampr1_q )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_nudgezrampr1_q
  INTEGER id_id
  model_config_rec%obs_nudgezrampr1_q = obs_nudgezrampr1_q 
  RETURN
END SUBROUTINE nl_set_obs_nudgezrampr1_q
SUBROUTINE nl_set_obs_nudgezfullr2_q ( id_id , obs_nudgezfullr2_q )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_nudgezfullr2_q
  INTEGER id_id
  model_config_rec%obs_nudgezfullr2_q = obs_nudgezfullr2_q 
  RETURN
END SUBROUTINE nl_set_obs_nudgezfullr2_q
SUBROUTINE nl_set_obs_nudgezrampr2_q ( id_id , obs_nudgezrampr2_q )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_nudgezrampr2_q
  INTEGER id_id
  model_config_rec%obs_nudgezrampr2_q = obs_nudgezrampr2_q 
  RETURN
END SUBROUTINE nl_set_obs_nudgezrampr2_q
SUBROUTINE nl_set_obs_nudgezfullr4_q ( id_id , obs_nudgezfullr4_q )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_nudgezfullr4_q
  INTEGER id_id
  model_config_rec%obs_nudgezfullr4_q = obs_nudgezfullr4_q 
  RETURN
END SUBROUTINE nl_set_obs_nudgezfullr4_q
SUBROUTINE nl_set_obs_nudgezrampr4_q ( id_id , obs_nudgezrampr4_q )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_nudgezrampr4_q
  INTEGER id_id
  model_config_rec%obs_nudgezrampr4_q = obs_nudgezrampr4_q 
  RETURN
END SUBROUTINE nl_set_obs_nudgezrampr4_q
SUBROUTINE nl_set_obs_nudgezfullmin ( id_id , obs_nudgezfullmin )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_nudgezfullmin
  INTEGER id_id
  model_config_rec%obs_nudgezfullmin = obs_nudgezfullmin 
  RETURN
END SUBROUTINE nl_set_obs_nudgezfullmin
SUBROUTINE nl_set_obs_nudgezrampmin ( id_id , obs_nudgezrampmin )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_nudgezrampmin
  INTEGER id_id
  model_config_rec%obs_nudgezrampmin = obs_nudgezrampmin 
  RETURN
END SUBROUTINE nl_set_obs_nudgezrampmin
SUBROUTINE nl_set_obs_nudgezmax ( id_id , obs_nudgezmax )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_nudgezmax
  INTEGER id_id
  model_config_rec%obs_nudgezmax = obs_nudgezmax 
  RETURN
END SUBROUTINE nl_set_obs_nudgezmax
SUBROUTINE nl_set_obs_sfcfact ( id_id , obs_sfcfact )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_sfcfact
  INTEGER id_id
  model_config_rec%obs_sfcfact = obs_sfcfact 
  RETURN
END SUBROUTINE nl_set_obs_sfcfact
SUBROUTINE nl_set_obs_sfcfacr ( id_id , obs_sfcfacr )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_sfcfacr
  INTEGER id_id
  model_config_rec%obs_sfcfacr = obs_sfcfacr 
  RETURN
END SUBROUTINE nl_set_obs_sfcfacr
SUBROUTINE nl_set_obs_dpsmx ( id_id , obs_dpsmx )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_dpsmx
  INTEGER id_id
  model_config_rec%obs_dpsmx = obs_dpsmx 
  RETURN
END SUBROUTINE nl_set_obs_dpsmx
SUBROUTINE nl_set_obs_rinxy ( id_id , obs_rinxy )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_rinxy
  INTEGER id_id
  model_config_rec%obs_rinxy(id_id) = obs_rinxy
  RETURN
END SUBROUTINE nl_set_obs_rinxy
SUBROUTINE nl_set_obs_rinsig ( id_id , obs_rinsig )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_rinsig
  INTEGER id_id
  model_config_rec%obs_rinsig = obs_rinsig 
  RETURN
END SUBROUTINE nl_set_obs_rinsig
SUBROUTINE nl_set_obs_twindo ( id_id , obs_twindo )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_twindo
  INTEGER id_id
  model_config_rec%obs_twindo(id_id) = obs_twindo
  RETURN
END SUBROUTINE nl_set_obs_twindo
SUBROUTINE nl_set_obs_npfi ( id_id , obs_npfi )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: obs_npfi
  INTEGER id_id
  model_config_rec%obs_npfi = obs_npfi 
  RETURN
END SUBROUTINE nl_set_obs_npfi
SUBROUTINE nl_set_obs_ionf ( id_id , obs_ionf )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: obs_ionf
  INTEGER id_id
  model_config_rec%obs_ionf(id_id) = obs_ionf
  RETURN
END SUBROUTINE nl_set_obs_ionf
SUBROUTINE nl_set_obs_idynin ( id_id , obs_idynin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: obs_idynin
  INTEGER id_id
  model_config_rec%obs_idynin = obs_idynin 
  RETURN
END SUBROUTINE nl_set_obs_idynin
SUBROUTINE nl_set_obs_dtramp ( id_id , obs_dtramp )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: obs_dtramp
  INTEGER id_id
  model_config_rec%obs_dtramp = obs_dtramp 
  RETURN
END SUBROUTINE nl_set_obs_dtramp
SUBROUTINE nl_set_obs_prt_max ( id_id , obs_prt_max )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: obs_prt_max
  INTEGER id_id
  model_config_rec%obs_prt_max = obs_prt_max 
  RETURN
END SUBROUTINE nl_set_obs_prt_max
SUBROUTINE nl_set_obs_prt_freq ( id_id , obs_prt_freq )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: obs_prt_freq
  INTEGER id_id
  model_config_rec%obs_prt_freq(id_id) = obs_prt_freq
  RETURN
END SUBROUTINE nl_set_obs_prt_freq
SUBROUTINE nl_set_obs_ipf_in4dob ( id_id , obs_ipf_in4dob )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: obs_ipf_in4dob
  INTEGER id_id
  model_config_rec%obs_ipf_in4dob = obs_ipf_in4dob 
  RETURN
END SUBROUTINE nl_set_obs_ipf_in4dob
SUBROUTINE nl_set_obs_ipf_errob ( id_id , obs_ipf_errob )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: obs_ipf_errob
  INTEGER id_id
  model_config_rec%obs_ipf_errob = obs_ipf_errob 
  RETURN
END SUBROUTINE nl_set_obs_ipf_errob
SUBROUTINE nl_set_obs_ipf_nudob ( id_id , obs_ipf_nudob )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: obs_ipf_nudob
  INTEGER id_id
  model_config_rec%obs_ipf_nudob = obs_ipf_nudob 
  RETURN
END SUBROUTINE nl_set_obs_ipf_nudob
SUBROUTINE nl_set_obs_ipf_init ( id_id , obs_ipf_init )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: obs_ipf_init
  INTEGER id_id
  model_config_rec%obs_ipf_init = obs_ipf_init 
  RETURN
END SUBROUTINE nl_set_obs_ipf_init
SUBROUTINE nl_set_obs_scl_neg_qv_innov ( id_id , obs_scl_neg_qv_innov )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: obs_scl_neg_qv_innov
  INTEGER id_id
  model_config_rec%obs_scl_neg_qv_innov = obs_scl_neg_qv_innov 
  RETURN
END SUBROUTINE nl_set_obs_scl_neg_qv_innov
SUBROUTINE nl_set_scm_force ( id_id , scm_force )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: scm_force
  INTEGER id_id
  model_config_rec%scm_force = scm_force 
  RETURN
END SUBROUTINE nl_set_scm_force
SUBROUTINE nl_set_scm_force_dx ( id_id , scm_force_dx )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: scm_force_dx
  INTEGER id_id
  model_config_rec%scm_force_dx = scm_force_dx 
  RETURN
END SUBROUTINE nl_set_scm_force_dx
SUBROUTINE nl_set_num_force_layers ( id_id , num_force_layers )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_force_layers
  INTEGER id_id
  model_config_rec%num_force_layers = num_force_layers 
  RETURN
END SUBROUTINE nl_set_num_force_layers
SUBROUTINE nl_set_scm_lu_index ( id_id , scm_lu_index )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: scm_lu_index
  INTEGER id_id
  model_config_rec%scm_lu_index = scm_lu_index 
  RETURN
END SUBROUTINE nl_set_scm_lu_index
SUBROUTINE nl_set_scm_isltyp ( id_id , scm_isltyp )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: scm_isltyp
  INTEGER id_id
  model_config_rec%scm_isltyp = scm_isltyp 
  RETURN
END SUBROUTINE nl_set_scm_isltyp
SUBROUTINE nl_set_scm_vegfra ( id_id , scm_vegfra )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: scm_vegfra
  INTEGER id_id
  model_config_rec%scm_vegfra = scm_vegfra 
  RETURN
END SUBROUTINE nl_set_scm_vegfra
SUBROUTINE nl_set_scm_canwat ( id_id , scm_canwat )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: scm_canwat
  INTEGER id_id
  model_config_rec%scm_canwat = scm_canwat 
  RETURN
END SUBROUTINE nl_set_scm_canwat
SUBROUTINE nl_set_scm_lat ( id_id , scm_lat )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: scm_lat
  INTEGER id_id
  model_config_rec%scm_lat = scm_lat 
  RETURN
END SUBROUTINE nl_set_scm_lat
!ENDOFREGISTRYGENERATEDINCLUDE
