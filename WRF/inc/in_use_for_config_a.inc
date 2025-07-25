!STARTOFREGISTRYGENERATEDINCLUDE 'inc/in_use_for_config_a.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF(TRIM(vname).EQ.'acphys_tot')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acphysc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acphyse')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acphysd')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acphyss')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acphysm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acphysf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'abstot')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'absnxt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.14
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.14
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.14
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.14
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.14
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.14
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.14
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.14
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.14
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.14
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.14
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.14
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.14
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.14
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.14
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.14
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.24
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.24
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.24
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.24
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.24
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.24
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.24
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.24
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.24
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.24
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.24
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.24
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.24
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.24
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.24
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.24
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'abstot')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'absnxt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.14
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.14
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.14
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.14
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.14
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.14
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.14
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.14
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.14
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.14
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.14
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.14
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.14
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.14
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.14
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.14
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.24
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.24
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.24
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.24
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.24
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.24
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.24
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.24
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.24
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.24
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.24
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.24
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.24
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.24
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.24
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.24
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'a_u_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'a_v_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'a_t_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'a_q_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'a_e_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'a_u_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'a_v_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'a_t_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'a_q_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'a_e_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'a_u_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'a_v_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'a_t_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'a_q_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'a_e_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acrunoff')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'alboldxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aparxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'areaxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'albedosubgrid')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'albedo_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'albbck_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aquad_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ainckfsa')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aerodm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%aer_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%aer_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aod5503d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%aer_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%aer_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aeromcu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%aercu_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%aercu_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aerovar')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%aercu_used.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%aercu_used.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'albsi')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%seaice_albedo_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%seaice_albedo_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'avg_fuel_frac')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'avgflx_rum')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%do_avgflx_em(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%do_avgflx_em(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'avgflx_rvm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%do_avgflx_em(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%do_avgflx_em(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'avgflx_wwm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%do_avgflx_em(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%do_avgflx_em(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'avgflx_cfu1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%do_avgflx_cugd(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%do_avgflx_cugd(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'avgflx_cfd1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%do_avgflx_cugd(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%do_avgflx_cugd(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'avgflx_dfu1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%do_avgflx_cugd(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%do_avgflx_cugd(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'avgflx_efu1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%do_avgflx_cugd(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%do_avgflx_cugd(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'avgflx_dfd1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%do_avgflx_cugd(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%do_avgflx_cugd(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'avgflx_efd1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%do_avgflx_cugd(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%do_avgflx_cugd(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_mslp')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_diag_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_diag_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_pwat')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_diag_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_diag_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_precip')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_diag_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_diag_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_totprecip')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_diag_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_diag_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_rain')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_ptype_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_ptype_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_ptype_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_ptype_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_ptype_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_ptype_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_fzra')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_ptype_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_ptype_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_snowfall')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_ptype_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_ptype_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_hail')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_severe_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_severe_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_cape')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_severe_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_severe_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_zlfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_severe_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_severe_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_plfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_severe_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_severe_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_lidx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_severe_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_severe_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_cin')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_severe_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_severe_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_tornado')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_severe_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_severe_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_llws')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_severe_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_severe_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_cloud_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_cloud_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_cloud_ceil')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_cloud_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_cloud_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_vis')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_vis_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_vis_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_vis_dust')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_vis_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_vis_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_vis_alpha')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_vis_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_vis_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_heatidx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_therm_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_therm_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_wchill')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_therm_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_therm_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_fits')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_therm_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_therm_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_turb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_turb_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_turb_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_llturb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_turb_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_turb_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_llturblgt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_turb_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_turb_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_llturbmdt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_turb_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_turb_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_llturbsvr')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_turb_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_turb_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_tlyrbot')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_turb_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_turb_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_tlyrtop')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_turb_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_turb_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_cape')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_buoy_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_buoy_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_zlfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_buoy_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_buoy_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_plfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_buoy_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_buoy_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_lidx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_buoy_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_buoy_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_cape_mu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_buoy_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_buoy_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_cin')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_buoy_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_buoy_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_cin_mu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_buoy_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_buoy_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'albedo12m')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'albedo12m')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.2
  ENDIF
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
