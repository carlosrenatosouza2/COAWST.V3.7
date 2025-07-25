program porcent

   implicit none
   
   
   integer                       :: rank, i, j, ranki, unid=1000
   real*8                        :: natmcores = 0.0, tempo1 = 0.0, tempo2 = 0.0 
   real*8, dimension(3000)       :: tempi_solveem = 0.0, deltat_timestep = 0.0
   character(len=9)              :: func
   character(len=4)              :: cunid
   character                     :: pos
   
   

   !Output:
   open(10, file='outputATM.txt')
   open(12, file='outputATM_media.txt')

   
   ! ATM: 320 a 639
   ! ATM: 320 a 1039
   ! ATM: 320 a 1599
   natmcores=1280.0
   i=0
   do rank = 320, 1599
   !do rank = 320, 322
   

      write(cunid, '(i4)') unid+rank
      print*, 'ATM ', cunid

         
      
      !lendo os tempos solve_em: fort.1320~1639(ATM): ----------------------
      open(unid+rank, file='datain/sosolveem/fort.'//cunid)
      read(unid+rank, *, end=14) tempo1, ranki, func, pos 
      j=1
      do
         read(unid+rank, *, end=14) tempo2, ranki, func, pos 
         deltat_timestep(j) = deltat_timestep(j) + tempo2 - tempo1
         !print*, rank, j, tempo1, tempo2,  deltat_timestep(j)
         tempo1 = tempo2
         
         j=j+1
      enddo
14    continue    
      close(unid+rank) 

      i=i+1
   enddo
   
   deltat_timestep = deltat_timestep / i
   
   
   do i=1, j
      write(12, "(i5, 1x, f7.2)") i,  deltat_timestep(i)
   enddo
      
end program
