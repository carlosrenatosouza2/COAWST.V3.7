!STARTOFREGISTRYGENERATEDINCLUDE 'inc/in_use_for_config_w.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF(TRIM(vname).EQ.'wsedl3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wact')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.40
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.40
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wm_temf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_sfclay_physics(id).EQ.10
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_sfclay_physics(id).EQ.10
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wslakexy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'waxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wtxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'woodxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wgapxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wtc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wtp')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wwlt_px')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wfc_px')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wsat_px')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wo1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wo2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wo3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wo4')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wu_tur')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wv_tur')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wt_tur')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wq_tur')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wpert2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.9
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.9
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wsedl3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.9
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.9
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wupd_temf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.10
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.10
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w0avg')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wup_cup')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.10
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.10
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w0avg')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.10
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.10
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w0avg')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w_up')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w0avg')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.99
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.99
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'winvbase_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wlcl_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wu_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wub')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w0avg')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w_subs')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w_subs_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wspd10max')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%nwp_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%nwp_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w_up_max')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%nwp_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%nwp_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w_dn_max')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%nwp_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%nwp_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w_mean')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%nwp_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%nwp_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w_xxx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%fft_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%fft_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ww_xxx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%fft_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%fft_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'watsat3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w_up_max')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_severe_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_severe_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w_dn_max')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_severe_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_severe_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w_mean')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_severe_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_severe_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wspd10max')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_severe_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_severe_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w_wif_now')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w_wif_jan')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w_wif_feb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w_wif_mar')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w_wif_apr')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w_wif_may')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w_wif_jun')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w_wif_jul')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w_wif_aug')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w_wif_sep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w_wif_oct')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w_wif_nov')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w_wif_dec')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wif_input_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wif_input_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wvp')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%solar_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%solar_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wp_sum')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%solar_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%solar_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wp_tot_sum')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%solar_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%solar_diagnostics.EQ.1
  ENDIF
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
