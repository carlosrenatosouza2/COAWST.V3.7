
























MODULE module_trad_fields
CONTAINS

   SUBROUTINE trad_fields ( u,v,w,t,qv,zp,zb,pp,pb,p,pw,            &
                    msfux,msfuy,msfvx,msfvy,msftx,msfty,            &
                    f,e,sina,cosa,                                  &
                    qc,rho,dz8w, ht,                                &
                    use_theta_m,                                    &
                    psfc,rainc,rainnc,snownc,graupelnc,hailnc,      &
                    sealevelp,                                      &
                    temperature,pressure,geoheight,                 &
                    umet,vmet,speed,dir,                            &
                    rain, liqrain, tpw,potential_t, rh,             &
                    ids,ide, jds,jde, kds,kde,                      &
                    ims,ime, jms,jme, kms,kme,                      &
                    ips,ipe, jps,jpe, kps,kpe,                      &
                    its,ite, jts,jte, kts,kte                       )
   
      USE diag_functions
      USE module_model_constants
   
      IMPLICIT NONE
   
   
      
   
      INTEGER, INTENT(IN   )                                          :: ids,ide, jds,jde, kds,kde, &
                                                                         ims,ime, jms,jme, kms,kme, &
                                                                         ips,ipe, jps,jpe, kps,kpe, &
                                                                         its,ite, jts,jte, kts,kte
      REAL   , INTENT(IN   ) , DIMENSION(ims:ime , jms:jme)           :: msfux,msfuy,msfvx,msfvy,msftx,msfty, &
                                                                         f,e,sina,cosa,ht
      INTEGER, INTENT(IN   )                                          :: use_theta_m
      REAL   , INTENT(IN   ) , DIMENSION(ims:ime , kms:kme , jms:jme) :: u,v,w,t,qv,zp,zb,pp,pb,p,pw

      REAL   , INTENT(IN   ) , DIMENSION(ims:ime , kms:kme , jms:jme) :: qc, rho, dz8w
      REAL   , INTENT(IN   ) , DIMENSION(ims:ime , jms:jme)           :: psfc, rainc, rainnc, hailnc,graupelnc, snownc
   
      
   
      REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , kms:kme , jms:jme) :: temperature , &
                                                                          pressure    , &
                                                                          geoheight   , &
                                                                          umet        , &
                                                                          vmet        , &
                                                                          speed       , &
                                                                          potential_t , &
                                                                          rh          , &
                                                                          dir
      REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)           :: sealevelp, rain, liqrain,tpw
   
      
   
      REAL :: ptot

      REAL, PARAMETER :: eps = 0.622, t_kelvin = svpt0 , s1 = 243.5, s2 = svp2 , s3 = svp1*10., s4 = 611.0, s5 = 5418.12
      REAL, PARAMETER :: zshul=75., tvshul=290.66
   
      INTEGER :: i, j, k
      REAL    :: es, qs
      REAL    :: gammas
   
      

      j_loop_h : DO j = jts , MIN(jte,jde-1)
         k_loop_h : DO k = kts , MIN(kte,kde-1)
            i_loop_h : DO i = its , MIN(ite,ide-1)
   
               

               temperature(i,k,j) = ( ( t(i,k,j) + t0 ) * ( (pb(i,k,j)+pp(i,k,j)) / p1000mb ) ** rcp )
 
               

               pressure(i,k,j) = p(i,k,j)
 
               

               geoheight(i,k,j) = ( zb(i,k,j)+zp(i,k,j)+zb(i,k+1,j)+zp(i,k+1,j) ) / (2.0 * g )
   
               

               umet(i,k,j) = 0.5 * ( (u(i,k,j)+u(i+1,k,j))*cosa(i,j) - (v(i,k,j)+v(i,k,j+1))*sina(i,j) )
               vmet(i,k,j) = 0.5 * ( (u(i,k,j)+u(i+1,k,j))*sina(i,j) + (v(i,k,j)+v(i,k,j+1))*cosa(i,j) )
   
               

               speed(i,k,j) = SQRT ( umet(i,k,j)**2 + vmet(i,k,j)**2 )
   
               

               IF      ( ( umet(i,k,j) .EQ. 0. ) .AND. ( vmet(i,k,j) .EQ. 0. ) ) THEN
                  dir(i,k,j) = 0.
               ELSE IF ( ( umet(i,k,j) .EQ. 0. ) .AND. ( vmet(i,k,j) .GT. 0. ) ) THEN
                  dir(i,k,j) = 180.
               ELSE IF ( ( umet(i,k,j) .EQ. 0. ) .AND. ( vmet(i,k,j) .LT. 0. ) ) THEN
                  dir(i,k,j) = 0.
               ELSE
                  dir(i,k,j) = 270. - atan2(vmet(i,k,j),umet(i,k,j)) * 180./3.14159265358979
                  IF ( dir(i,k,j) .GE. 360. ) THEN
                     dir(i,k,j) = dir(i,k,j) - 360.
                  END IF
                  IF ( dir(i,k,j) .LE.   0. ) THEN
                     dir(i,k,j) = dir(i,k,j) + 360.
                  END IF
               END IF

               

               potential_t(i,k,j) =   t(i,k,j) + t0  


               

                  ptot      = pb(i,k,j)+pp(i,k,j)
                  rh(i,k,j) = calc_rh(ptot, temperature(i,k,j), qv(i,k,j)) 

            END DO i_loop_h
         END DO k_loop_h
      END DO j_loop_h
   
      

      j_loop_f : DO j = jts , MIN(jte,jde-1)
         k_loop_f : DO k = kts , kte
            i_loop_f : DO i = its , MIN(ite,ide-1)

 
 
            END DO i_loop_f
         END DO k_loop_f
      END DO j_loop_f
   
      

      j_loop_2 : DO j = jts , MIN(jte,jde-1)
         i_loop_2 : DO i = its , MIN(ite,ide-1)

               

               sealevelp(i,j) =   MSLP ( ht(i,j), pressure(i,kms,j), geoheight(i,kms,j) , &
                                         qv(i,kms,j), temperature(i,kms,j) )

               

               rain(i,j) =   rainc(i,j) + rainnc(i,j)

               

               liqrain(i,j) =   rainc(i,j) + rainnc(i,j) - snownc(i,j) - graupelnc(i,j) - hailnc(i,j)

               

               tpw(i,j) = PWAT(kme-kms+1, qv(i,kms:kme,j), qc(i,kms:kme,j), dz8w(i,kms:kme,j), rho(i,kms:kme,j))  

         END DO i_loop_2
      END DO j_loop_2

   END SUBROUTINE trad_fields

END MODULE module_trad_fields
