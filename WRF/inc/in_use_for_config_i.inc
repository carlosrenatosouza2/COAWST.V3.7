!STARTOFREGISTRYGENERATEDINCLUDE 'inc/in_use_for_config_i.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF(TRIM(vname).EQ.'icwmrdp3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'iradius')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'itype')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.55
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.55
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'itype_2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.55
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.55
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'itype_3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.55
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.55
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'isnowxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'irgxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ircxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'irbxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'imperv')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'isnow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'irrigation')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surf_irr_alloc.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surf_irr_alloc.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'irr_rand_field')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surf_irr_alloc.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surf_irr_alloc.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'irrigation')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surf_irr_alloc.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surf_irr_alloc.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'irr_rand_field')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surf_irr_alloc.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surf_irr_alloc.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'irrigation')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surf_irr_alloc.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surf_irr_alloc.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'irr_rand_field')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surf_irr_alloc.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surf_irr_alloc.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'icwmrdp3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ideep2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ind_delcin_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'icwmrsh')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'i_rainc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bucketr_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bucketr_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'i_rainnc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bucketr_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bucketr_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'i_acswupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bucketf_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bucketf_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'i_acswuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bucketf_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bucketf_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'i_acswdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bucketf_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bucketf_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'i_acswdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bucketf_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bucketf_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'i_acswupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bucketf_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bucketf_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'i_acswupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bucketf_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bucketf_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'i_acswdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bucketf_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bucketf_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'i_acswdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bucketf_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bucketf_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'i_aclwupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bucketf_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bucketf_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'i_aclwuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bucketf_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bucketf_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'i_aclwdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bucketf_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bucketf_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'i_aclwdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bucketf_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bucketf_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'i_aclwupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bucketf_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bucketf_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'i_aclwupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bucketf_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bucketf_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'i_aclwdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bucketf_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bucketf_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'i_aclwdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bucketf_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bucketf_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'icedepth')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%seaice_thickness_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%seaice_thickness_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ic_flashcount')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%lightning_option(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%lightning_option(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ic_flashrate')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%lightning_option(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%lightning_option(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ic_flashcount')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%lightning_option(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%lightning_option(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ic_flashrate')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%lightning_option(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%lightning_option(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ic_flashcount')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%lightning_option(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%lightning_option(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ic_flashrate')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%lightning_option(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%lightning_option(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'iboros')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ischap')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'icingtop')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_icing_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_icing_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'icingbot')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_icing_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_icing_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'icing_lg')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_icing_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_icing_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'icing_sm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_icing_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_icing_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'induc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%elec_physics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%elec_physics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'induc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%elec_physics.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%elec_physics.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'induc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%elec_physics.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%elec_physics.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'i_wif_now')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'i_wif_jan')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'i_wif_feb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'i_wif_mar')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'i_wif_apr')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'i_wif_may')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'i_wif_jun')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'i_wif_jul')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'i_wif_aug')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'i_wif_sep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'i_wif_oct')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'i_wif_nov')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'i_wif_dec')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'iwp')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%solar_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%solar_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'iwp_tot')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%solar_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%solar_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'intq_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'icefrac_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'intq_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'icefrac_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.2
  ENDIF
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
