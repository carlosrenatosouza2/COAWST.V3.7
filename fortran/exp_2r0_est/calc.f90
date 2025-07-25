program calcula_tempos

   implicit none
   
   integer, parameter               :: nl=6
   integer                          :: i        
   integer, dimension(nl)           :: rankws, rankwr 
   integer, dimension(2)            :: rankrunatm
   real*8                           :: ref, deltaws = 0.0, deltawr = 0.0, deltarunatm = 0.0
   real*8, dimension(nl)            :: tempows, tempowr
   real*8, dimension(2)             :: temporunatm
   character(len=9), dimension(nl)  :: funcws, funcwr
   character(len=9), dimension(2)   :: funcrunatm
   character, dimension(nl)         :: posws, poswr
   character, dimension(2)          :: posrunatm
   
   
   !leitura:
   open(10, file='fort.3320') ! waits
   open(11, file='fort.5320') ! waitr
   open(12, file='fort.1320') ! init run fina
   
   do i = 1, nl
      read(10, *) tempows(i), rankws(i), funcws(i), posws(i)
      read(11, *) tempowr(i), rankwr(i), funcwr(i), poswr(i)
   enddo
   do i = 1, 2
      read(12, *) temporunatm(i), rankrunatm(i), funcrunatm(i), posrunatm(i)
   enddo
   
   deltaws = 0.0
   do i = 1, nl-1, 2
      deltaws = deltaws + (tempows(i+1)-tempows(i))
      deltawr = deltawr + (tempowr(i+1)-tempowr(i))
   enddo
   deltarunatm = temporunatm(2) - temporunatm(1)
   
   print*, 
   
   write(*, "('Rank ', i3, ' :: waits = ', f6.2, ' %,  waitr = ', f6.2, ' %, wait total = ', f6.2, ' %.')") &
   rankrunatm(1), deltaws/deltarunatm*100, deltawr/deltarunatm*100, (deltaws+deltawr)/deltarunatm*100
   

end program
