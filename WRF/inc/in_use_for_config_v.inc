!STARTOFREGISTRYGENERATEDINCLUDE 'inc/in_use_for_config_v.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF(TRIM(vname).EQ.'vmi3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.50
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.50
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vmi3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.51
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.51
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vmi3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.52
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.52
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vmi3d_2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.52
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.52
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vmi3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.55
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.55
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vmi3d_2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.55
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.55
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vmi3d_3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.55
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.55
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v10e')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_sfclay_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_sfclay_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v10e')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_sfclay_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_sfclay_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vl_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vl_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vl_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vegf_px')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v_up')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vw_temf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.10
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.10
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vflx_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vten_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vu_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vu_emf_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vdfg')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grav_settling(id).EQ.0
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grav_settling(id).EQ.0
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vdfg')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grav_settling(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grav_settling(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vdfg')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grav_settling(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grav_settling(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v10_ndg_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v10_ndg_new')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v10_ndg_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v10_ndg_new')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v_g')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v_g_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v_upstream_x')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v_upstream_x_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v_upstream_y')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v_upstream_y_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v_largescale')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v_largescale_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v10max')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v10mean')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v10std')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v_xxx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%fft_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%fft_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vah')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vertstrucc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sppt_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sppt_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vertstrucs')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sppt_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sppt_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vertampt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sppt_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sppt_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vertstrucc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%skebs_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%skebs_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vertstrucs')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%skebs_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%skebs_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vertampt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%skebs_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%skebs_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vertampuv')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%skebs_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%skebs_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vertstrucc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%rand_perturb_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%rand_perturb_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vertstrucs')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%rand_perturb_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%rand_perturb_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vertampt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%rand_perturb_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%rand_perturb_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vertstrucc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%spp_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%spp_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vertstrucs')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%spp_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%spp_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vertampt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%spp_on.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%spp_on.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v_pl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%p_lev_diags.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%p_lev_diags.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v_zl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%z_lev_diags.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%z_lev_diags.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vil')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_vil_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_vil_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v10_mean')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mean_diag.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mean_diag.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v10_diurn')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%diurnal_diag.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%diurnal_diag.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vmet')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%diag_nwp2.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%diag_nwp2.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vmaxw')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vtrop')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.2
  ENDIF
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
