
























MODULE module_diag_solar

CONTAINS

















   SUBROUTINE solar_diag (rho, dz8w, ph, phb, cldfrac3d, coszen, swdnb, swdnt, &
             param_first_scalar, p_qc, p_qi, p_qs, qv, qc, qi, qs, &
             qc_tot, qi_tot, has_reqc, has_reqi, has_reqs, f_qv, f_qc, f_qi, f_qs, &
             re_cloud, re_ice, re_snow, clrnidx, sza, cldfrac2d, wvp2d, lwp2d, iwp2d, swp2d, &
             wp2d_sum, lwp2d_tot, iwp2d_tot, wp2d_tot_sum, re_cloud_path, re_ice_path, re_snow_path, &
             re_cloud_path_tot, re_ice_path_tot, tau_qc, tau_qi, tau_qs, tau_qc_tot, tau_qi_tot, &
             cbase, ctop, cbase_tot, ctop_tot, ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, &
             kms, kme, ips, ipe, jps, jpe, kps, kpe, i_start, i_end, j_start, j_end, kts, kte, num_tiles)



     USE module_model_constants, ONLY: G

     IMPLICIT NONE

     REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN) :: coszen, swdnb, swdnt
     REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: ph, phb, cldfrac3d, qv, qc, qi, qs, qc_tot, qi_tot, &
          re_cloud, re_ice, re_snow, rho, dz8w
     INTEGER, INTENT(IN) :: param_first_scalar, p_qc, p_qi, p_qs, has_reqc, has_reqi, has_reqs
     LOGICAL, INTENT(IN) :: f_qv, f_qc, f_qi, f_qs
     REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT) :: clrnidx, sza, cldfrac2d, wvp2d, lwp2d, iwp2d, swp2d, wp2d_sum, &
          lwp2d_tot, iwp2d_tot, wp2d_tot_sum, re_cloud_path, re_ice_path, re_snow_path, re_cloud_path_tot, re_ice_path_tot, &
          tau_qc, tau_qi, tau_qs, tau_qc_tot, tau_qi_tot, cbase, ctop, cbase_tot, ctop_tot
     INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, &
             ips, ipe, jps, jpe, kps, kpe, kts, kte, num_tiles
     INTEGER, DIMENSION(num_tiles), INTENT(IN) :: i_start, i_end, j_start, j_end
 
     INTEGER :: i, j, k, its, ite, jts, jte, ij
     REAL    :: pmw, swp, iwp, lwp, q_aux, wc
     REAL, PARAMETER :: THREE_OVER_TWO = 3.0 / 2.0
     REAL, PARAMETER :: PI = 4.0 * ATAN(1.0)
     REAL, PARAMETER :: DEGRAD = 180.0 / PI
     REAL, PARAMETER :: Q_MIN = 0.0
     REAL, PARAMETER :: WC_MIN = 1E-5
     REAL, PARAMETER :: RE_MIN = 0.0
     REAL, PARAMETER :: MISSING = -999.0
     REAL, DIMENSION(kms:kme) :: rhodz
     INTEGER, PARAMETER :: TAU_ICE_METHOD = 1
     INTEGER, PARAMETER :: I_TO_PLOT = 299, J_TO_PLOT = 84

!      !$OMP PARALLEL DO   &
!      !$OMP PRIVATE ( ij )
       DO ij = 1, num_tiles
         its = i_start(ij)
         ite = i_end(ij)
         jts = j_start(ij)
         jte = j_end(ij)
         DO j = jts, jte
           DO i = its, ite

             
             sza(i, j) = DEGRAD * MAX( ACOS( coszen(i, j) ), 0.0 )

             
             if ( (sza(i, j) < 90.0) .and. (swdnt(i, j) > 0.0) ) then
               clrnidx(i, j) = swdnb(i, j) / swdnt(i, j)
             else
               clrnidx(i, j) = MISSING
             end if

             
             cldfrac2d(i, j) = MAXVAL( cldfrac3d(i, kts:kte-1, j) )

             
             rhodz(:) = rho(i, :, j) * dz8w(i, :, j) / (1. + qv(i, :, j))

             
             if (f_qv) then
                 
               pmw = integrate_1var (rhodz, qv(i, :, j), kms, kme, kts, kte)
               wvp2d(i, j) = pmw
             end if

             
             if (f_qc .and. p_qc > param_first_scalar) then
                 
                 
               q_aux = integrate_1var (rhodz, qc(i, :, j), kms, kme, kts, kte)
               lwp = q_aux
               lwp2d(i, j) = SIGN( MAX( lwp, 0.0 ), 1.0 )

               if (has_reqc == 1) then
                   
                 if (q_aux > Q_MIN) then
                   re_cloud_path(i, j) = integrate_2var (rhodz, qc(i, :, j), &
                       re_cloud(i, :, j), kms, kme, kts, kte)
                   re_cloud_path(i, j) = re_cloud_path(i, j) / q_aux
                 else
                   re_cloud_path(i, j) = 0.0
                 end if

                   
                 if (re_cloud_path(i, j) > RE_MIN) then
                   tau_qc(i, j) = THREE_OVER_TWO * lwp / re_cloud_path(i, j) / 1000.0
                 else
                   tau_qc(i, j) = 0.0
                 end if
               end if

                 
                 
               q_aux = integrate_1var (rhodz, qc_tot(i, :, j), kms, kme, kts, kte)
               lwp = q_aux
               lwp2d_tot(i, j) = SIGN( MAX( lwp, 0.0 ), 1.0 )

               if (has_reqc == 1) then
                   
                 if (q_aux > Q_MIN) then
                   re_cloud_path_tot(i, j) = integrate_2var (rhodz, qc_tot(i, :, j), &
                       re_cloud(i, :, j), kms, kme, kts, kte)
                   re_cloud_path_tot(i, j) = re_cloud_path_tot(i, j) / q_aux
                 else
                   re_cloud_path_tot(i, j) = 0.0
                 end if

                   
                 if (re_cloud_path_tot(i, j) > RE_MIN) then
                   tau_qc_tot(i, j) = THREE_OVER_TWO * lwp / re_cloud_path_tot(i, j) / 1000.0
                 else
                   tau_qc_tot(i, j) = 0.0
                 end if
               end if
             else
               lwp2d(i, j) = MISSING
               re_cloud_path(i, j) = MISSING
               tau_qc(i, j) = MISSING
               lwp2d_tot(i, j) = MISSING
               re_cloud_path_tot(i, j) = MISSING
               tau_qc_tot(i, j) = MISSING
             end if

             
             if (f_qi .and. p_qi > param_first_scalar) then
                 
                 
               q_aux = integrate_1var (rhodz, qi(i, :, j), kms, kme, kts, kte)
               iwp = q_aux
               iwp2d(i, j) = SIGN( MAX( iwp, 0.0 ), 1.0 )

               if (has_reqi == 1) then
                   
                 if (q_aux > Q_MIN) then
                   re_ice_path(i, j) = integrate_2var (rhodz, qi(i, :, j), &
                       re_ice(i, :, j), kms, kme, kts, kte)
                   re_ice_path(i, j) = re_ice_path(i, j) / q_aux
                 else
                   re_ice_path(i, j) = 0.0
                 end if

                   
                 if (re_ice_path(i, j) > RE_MIN) then
                   if (TAU_ICE_METHOD == 1) then
                       
                     tau_qi(i, j) = iwp * 1000.0 * (0.02 + 4.2 / (2.0 * re_ice_path(i, j) * 1.0e+6))
                   else
                     tau_qi(i, j) = iwp * 1000.0 * (-0.006656 + 3.686 / (2.0 * re_ice_path(i, j) * 1.0e+6))
                   end if
                 else
                   tau_qi(i, j) = 0.0
                 end if
               end if

                 
                 
               q_aux = integrate_1var (rhodz, qi_tot(i, :, j), kms, kme, kts, kte)
               iwp = q_aux
               iwp2d_tot(i, j) = SIGN( MAX( iwp, 0.0 ), 1.0 )

               if (has_reqi == 1) then
                   
                 if (q_aux > Q_MIN) then
                   re_ice_path_tot(i, j) = integrate_2var (rhodz, qi_tot(i, :, j), &
                       re_ice(i, :, j), kms, kme, kts, kte)
                   re_ice_path_tot(i, j) = re_ice_path_tot(i, j) / q_aux
                 else
                   re_ice_path_tot(i, j) = 0.0
                 end if

                   
                 if (re_ice_path_tot(i, j) > RE_MIN) then
                   if (TAU_ICE_METHOD == 1) then
                       
                     tau_qi_tot(i, j) = iwp * 1000.0 * (0.02 + 4.2 / (2.0 * re_ice_path_tot(i, j) * 1.0e+6))
                   else
                     tau_qi_tot(i, j) = iwp * 1000.0 * (-0.006656 + 3.686 / (2.0 * re_ice_path_tot(i, j) * 1.0e+6))
                   end if
                 else
                   tau_qi_tot(i, j) = 0.0
                 end if
               end if
             else
               iwp2d(i, j) = MISSING
               re_ice_path(i, j) = MISSING
               tau_qi(i, j) = MISSING
               iwp2d_tot(i, j) = MISSING
               re_ice_path_tot(i, j) = MISSING
               tau_qi_tot(i, j) = MISSING
             end if

             
             if (f_qs .and. p_qs > param_first_scalar) then
                 
                 
               q_aux = integrate_1var (rhodz, qs(i, :, j), kms, kme, kts, kte)
               swp = q_aux
               swp2d(i, j) = SIGN( MAX( swp, 0.0 ), 1.0 )

               if (has_reqs == 1) then
                 if (q_aux > Q_MIN) then
                   re_snow_path(i, j) = integrate_2var (rhodz, qs(i, :, j), &
                       re_snow(i, :, j), kms, kme, kts, kte)
                   re_snow_path(i, j) = re_snow_path(i, j) / q_aux
                 else
                   re_snow_path(i, j) = 0.0
                 end if

                   
                 if (re_snow_path(i, j) > RE_MIN) then
                   if (TAU_ICE_METHOD == 1) then
                       
                     tau_qs(i, j) = swp * 1000.0 * (0.02 + 4.2 / (2.0 * re_snow_path(i, j) * 1.0e+6))
                   else
                     tau_qs(i, j) = swp * 1000.0 * (-0.006656 + 3.686 / (2.0 * re_snow_path(i, j) * 1.0e+6))
                   end if
                 else
                   tau_qs(i, j) = 0.0
                 end if
               end if
             else  
               swp2d(i, j) = MISSING
               re_snow_path(i, j) = MISSING
               tau_qs(i, j) = MISSING
             end if

             if ( (f_qc .or. f_qi .or. f_qs) .and. & 
                  (p_qc > param_first_scalar .or. &
                   p_qi > param_first_scalar .or. &
                   p_qs > param_first_scalar) ) then
                 
                 
               wp2d_sum(i, j) = MAX( lwp2d(i, j), 0.0 ) + MAX( iwp2d(i, j), 0.0 ) + MAX( swp2d(i, j), 0.0 )

                 
                 
               if (wp2d_sum(i, j) > Q_MIN) then
                 k = kts
                 wc = 0.0
                 if (f_qc .and. p_qc > param_first_scalar) wc = wc + qc(i, k, j)
                 if (f_qi .and. p_qi > param_first_scalar) wc = wc + qi(i, k, j)
                 if (f_qs .and. p_qs > param_first_scalar) wc = wc + qs(i, k, j)
                 do while ( (wc < WC_MIN) .and. (k < kte) )
                   k = k + 1
                   wc = 0.0
                   if (f_qc .and. p_qc > param_first_scalar) wc = wc + qc(i, k, j)
                   if (f_qi .and. p_qi > param_first_scalar) wc = wc + qi(i, k, j)
                   if (f_qs .and. p_qs > param_first_scalar) wc = wc + qs(i, k, j)
                 end do

                 if (k == kte) then
                   cbase(i, j) = MISSING
                 else
                   cbase(i, j) = ( (ph(i, k, j) + phb(i, k, j)) + (ph(i, k + 1, j) + phb(i, k + 1, j)) ) / (2.0 * G)
                 end if

                   
                 k = kte
                 wc = 0.0
                 if (f_qc .and. p_qc > param_first_scalar) wc = wc + qc(i, k, j)
                 if (f_qi .and. p_qi > param_first_scalar) wc = wc + qi(i, k, j)
                 if (f_qs .and. p_qs > param_first_scalar) wc = wc + qs(i, k, j)
                 do while ( (wc < WC_MIN) .and. (k > kts) )
                   k = k - 1
                   wc = 0.0
                   if (f_qc .and. p_qc > param_first_scalar) wc = wc + qc(i, k, j)
                   if (f_qi .and. p_qi > param_first_scalar) wc = wc + qi(i, k, j)
                   if (f_qs .and. p_qs > param_first_scalar) wc = wc + qs(i, k, j)
                 end do

                 if (k == kts) then
                   ctop(i, j) = MISSING
                 else
                   ctop(i, j) = ( (ph(i, k, j) + phb(i, k, j)) + (ph(i, k + 1, j) + phb(i, k + 1, j)) ) / (2.0 * G)
                 end if
               else
                 cbase(i, j) = MISSING
                 ctop(i, j) = MISSING
               end if

                 
                 
                 
               wp2d_tot_sum(i, j) = MAX( lwp2d_tot(i, j), 0.0 ) + MAX( iwp2d_tot(i, j), 0.0 ) + MAX( swp2d(i, j), 0.0 )

                 
               if (wp2d_tot_sum(i, j) > Q_MIN) then
                 k = kts
                 wc = 0.0
                 if (f_qc .and. p_qc > param_first_scalar) wc = wc + qc_tot(i, k, j)
                 if (f_qi .and. p_qi > param_first_scalar) wc = wc + qi_tot(i, k, j)
                 if (f_qs .and. p_qs > param_first_scalar) wc = wc + qs(i, k, j)
                 do while ( (wc < WC_MIN) .and. (k < kte) )
                   k = k + 1
                   wc = 0.0
                   if (f_qc .and. p_qc > param_first_scalar) wc = wc + qc_tot(i, k, j)
                   if (f_qi .and. p_qi > param_first_scalar) wc = wc + qi_tot(i, k, j)
                   if (f_qs .and. p_qs > param_first_scalar) wc = wc + qs(i, k, j)
                 end do

                 if (k == kte) then
                   cbase_tot(i, j) = MISSING
                 else
                   cbase_tot(i, j) = ( (ph(i, k, j) + phb(i, k, j)) + (ph(i, k + 1, j) + phb(i, k + 1, j)) ) / (2.0 * G)
                 end if

                   
                 k = kte
                 wc = 0.0
                 if (f_qc .and. p_qc > param_first_scalar) wc = wc + qc_tot(i, k, j)
                 if (f_qi .and. p_qi > param_first_scalar) wc = wc + qi_tot(i, k, j)
                 if (f_qs .and. p_qs > param_first_scalar) wc = wc + qs(i, k, j)
                 do while ( (wc < WC_MIN) .and. (k > kts) )
                   k = k - 1
                   wc = 0.0
                   if (f_qc .and. p_qc > param_first_scalar) wc = wc + qc_tot(i, k, j)
                   if (f_qi .and. p_qi > param_first_scalar) wc = wc + qi_tot(i, k, j)
                   if (f_qs .and. p_qs > param_first_scalar) wc = wc + qs(i, k, j)
                 end do

                 if (k == kts) then
                   ctop_tot(i, j) = MISSING
                 else
                   ctop_tot(i, j) = ( (ph(i, k, j) + phb(i, k, j)) + (ph(i, k + 1, j) + phb(i, k + 1, j)) ) / (2.0 * G)
                 end if
               else
                 cbase_tot(i, j) = MISSING
                 ctop_tot(i, j) = MISSING
               end if
             else
               wp2d_sum(i, j) = MISSING
               cbase(i, j) = MISSING
               ctop(i, j) = MISSING
               wp2d_tot_sum(i, j) = MISSING
               cbase_tot(i, j) = MISSING
               ctop_tot(i, j) = MISSING
             end if

           ENDDO
         ENDDO
       ENDDO
!      !$OMP END PARALLEL DO

   END SUBROUTINE solar_diag


   FUNCTION Integrate_1var (rhodz, var1_1d, kms, kme, kts, kte) &
        RESULT (return_value)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: kts, kte, kms, kme
      REAL, DIMENSION(kms:kme), INTENT(IN) :: var1_1d, rhodz

        
      REAL :: return_value
      INTEGER :: k

      return_value = 0.0
      do k = kts, kte - 1
        return_value = return_value + var1_1d(k) * rhodz(k)
      end do

    END FUNCTION Integrate_1var


    FUNCTION Integrate_2var (rhodz, var1_1d, var2_1d, kms, kme, kts, kte) &
        RESULT (return_value)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: kts, kte, kms, kme
      REAL, DIMENSION(kms:kme), INTENT(IN) :: var1_1d, var2_1d, rhodz

        
      REAL :: return_value
      INTEGER :: k

      return_value = 0.0
      do k = kts, kte - 1
        return_value = return_value + var1_1d(k) * var2_1d(k) * rhodz(k)
      end do

    END FUNCTION Integrate_2var

END MODULE module_diag_solar
