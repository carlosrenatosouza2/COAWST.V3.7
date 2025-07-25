program calcula_tempos

   implicit none
   
   integer, parameter         :: NCOUP=3
   integer                    :: rank_atm, rank_ocn, i
   real*8                     :: i_atm_init, f_atm_init, i_atm_run, f_atm_run, i_atm_fina, f_atm_fina
   real*8                     :: i_ocn_init, f_ocn_init, i_ocn_run, f_ocn_run, i_ocn_fina, f_ocn_fina
   real*8, dimension(NCOUP)   :: i_isend_atm, f_isend_atm, i_waits_atm, f_waits_atm
   real*8                     :: ref
   real                       :: rrank_atm, rrank_ocn
   character(len=20)          :: nome1, nome2, nome3, nome4, nome5, nome6
   
   
   ! Inputs:
   ! Master ATM:
   open(10, file='fort.1320')
   ! send/waits ATM:
   open(11, file='fort.5000')
   ! Master OCN:
   open(20, file='fort.1000')
   
   
   !Outputs:
   open(30, file='graf3.gnu.input1')
   open(31, file='graf3.gnu.input2')

   
   ! ATM Init
   read(10, *) nome1, nome2, nome3, rank_atm, nome4, nome5, nome6, i_atm_init, f_atm_init
   ! ATM Run
   read(10, *) nome1, nome2, nome3, rank_atm, nome4, nome5, nome6, i_atm_run, f_atm_run
   ! ATM Finalize
   read(10, *) nome1, nome2, nome3, rank_atm, nome4, nome5, nome6, i_atm_fina, f_atm_fina
   
   ! ATM Send/Waits:
   do i = 1, NCOUP
      read(11, *) nome1, nome2, nome3, nome4, nome5, nome6, nome1, i_isend_atm(1), f_isend_atm(i)
      read(11, *) nome1, nome2, nome3, nome4, nome5, nome6, nome1, i_waits_atm(1), f_waits_atm(i)
      
   enddo

   ! OCN Init
   read(20, *) nome1, nome2, nome3, rank_ocn, nome4, nome5, nome6, i_ocn_init, f_ocn_init
   ! OCN Run
   read(20, *) nome1, nome2, nome3, rank_ocn, nome4, nome5, nome6, i_ocn_run, f_ocn_run
   ! OCN Finalize
   read(20, *) nome1, nome2, nome3, rank_ocn, nome4, nome5, nome6, i_ocn_fina, f_ocn_fina


   ref=i_atm_init
   i_atm_init = i_atm_init - ref
   f_atm_init = f_atm_init - ref
   i_atm_run = i_atm_run - ref
   f_atm_run = f_atm_run - ref
   i_atm_fina = i_atm_fina - ref
   f_atm_fina = f_atm_fina - ref
   i_ocn_init = i_ocn_init - ref
   f_ocn_init = f_ocn_init - ref
   i_ocn_run = i_ocn_run - ref
   f_ocn_run = f_ocn_run - ref
   i_ocn_fina = i_ocn_fina - ref
   f_ocn_fina = f_ocn_fina - ref
   do i = 1, NCOUP
      i_isend_atm(i) = i_isend_atm(i) - ref
      f_isend_atm(i) = f_isend_atm(i) - ref
      i_waits_atm(i) = i_waits_atm(i) - ref
      f_waits_atm(i) = f_waits_atm(i) - ref
   enddo

   
  
   write(*, *) 'ATM Init : ', i_atm_init, f_atm_init
   write(*, *) 'ATM Run  : ', i_atm_run, f_atm_run
   write(*, *) 'ATM Fina : ', i_atm_fina, f_atm_fina
   write(*, *) 'ATM ISend: ', i_isend_atm(1), f_isend_atm(1)
   write(*, *) 'ATM Waits: ', i_waits_atm(1), f_waits_atm(1)
   print*, ''
   write(*, *) 'OCN Init: ', i_ocn_init, f_ocn_init
   write(*, *) 'OCN Run : ', i_ocn_run, f_ocn_run
   write(*, *) 'OCN Fina: ', i_ocn_fina, f_ocn_fina
   

   rrank_atm = float(rank_atm)
   rrank_ocn = float(rank_ocn)

!  Imprimindo arquivo de dados para gnuplot:
   write(30, "('col1    ATM_initI ATM_run ATM_fin')")
   write(30, "(f5.1, 1x, f6.2, 5x, f6.2, 3x, f6.2)") rrank_atm, i_atm_init, i_atm_run, i_atm_fina
   write(30, "(f5.1, 1x, f6.2, 5x, f6.2, 3x, f6.2)") rrank_atm, f_atm_init, f_atm_run, f_atm_fina

   write(31, "('col1    OCN_initI OCN_run OCN_fin')")
   write(31, "(f5.1, 1x, f6.2, 5x, f6.2, 3x, f6.2)") rrank_ocn, i_ocn_init, i_ocn_run, i_ocn_fina
   write(31, "(f5.1, 1x, f6.2, 5x, f6.2, 3x, f6.2)") rrank_ocn, f_ocn_init, f_ocn_run, f_ocn_fina



end program
