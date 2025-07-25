!STARTOFREGISTRYGENERATEDINCLUDE 'inc/in_use_for_config_p.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF(TRIM(vname).EQ.'phys_tot')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'physc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'physe')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'physd')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'physs')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'physm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'physf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'preci3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'precs3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'precg3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'precr3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'prech3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'phii3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.55
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.55
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'phii3d_2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.55
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.55
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'phii3d_3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.55
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.55
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'precipfr')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'psnxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pexpxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'planting')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pgsxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'precz')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'preccdzm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pconvb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pconvt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'plcl_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pinv_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'plfc_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pbup_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ppen_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pblmax')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pblhavg')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'psl_ndg_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ps_ndg_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'psl_ndg_new')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ps_ndg_new')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'psl_ndg_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ps_ndg_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'psl_ndg_new')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ps_ndg_new')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'prec_acc_c')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%prec_acc_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%prec_acc_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'prec_acc_nc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%prec_acc_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%prec_acc_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'power')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%windfarm_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%windfarm_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ph_xxx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%fft_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%fft_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'phiwc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'psfc_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'psfc_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%fmoisti_run(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%fmoisti_run(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pattern_spp_conv')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%spp_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%spp_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pattern_spp_pbl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%spp_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%spp_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pattern_spp_lsm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%spp_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%spp_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p_pl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%p_lev_diags.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%p_lev_diags.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'psfc_mean')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mean_diag.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mean_diag.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pmsl_mean')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mean_diag.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mean_diag.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'psfc_diurn')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%diurnal_diag.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%diurnal_diag.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pot')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%elec_physics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%elec_physics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pot')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%elec_physics.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%elec_physics.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pot')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%elec_physics.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%elec_physics.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p_wif_now')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p_wif_jan')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p_wif_feb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p_wif_mar')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p_wif_apr')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p_wif_may')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p_wif_jun')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p_wif_jul')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p_wif_aug')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p_wif_sep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p_wif_oct')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p_wif_nov')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p_wif_dec')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pressure')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%diag_nwp2.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%diag_nwp2.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'potential_t')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%diag_nwp2.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%diag_nwp2.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pslv_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pd_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'psfc_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pdhs')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'prho_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pdrho_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pmaxw')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ptrop')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pslv_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pd_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'psfc_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pdhs')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.2
  ENDIF
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
