program calcula_tempos

   implicit none
   
   
   integer, parameter               :: nl=26
   integer                          :: i, rest, j
   integer, dimension(nl)           :: rank 
   real*8                           :: ref
   real*8, dimension(nl)            :: tempo
   character(len=9), dimension(nl)  :: func
   character, dimension(nl)         :: pos
   character(len=10*nl)             :: linha
   character(len=10)                :: color


   open(10, file='tudoatm.txt')

   !Outputs:
   open(30, file='graf3.gnu.input1')
   open(31, file='graf3.gnu')
   

   
   do i = 1, nl
      read(10, *) tempo(i), rank(i), func(i), pos(i)
!      write(*, *) tempo(i), rank(i), func(i), pos(i)
   enddo


   ref = tempo(1)
   
   tempo = tempo - ref

   do i = 1, nl
      write(*, *) i, tempo(i), rank(i), func(i), pos(i)
   enddo
   print*,''

   write(linha, "('rank', 1x,a9, 1x, 13(a9, 1x))") func(1),(func(i), i=2, nl, 2)
   write(30, *) linha
   
   write(30, "(i3, 1x, f11.4)", advance='no') rank(1), tempo(1)
   !write(*, "(i9)", advance='no') 1
   do i = 2, nl-2,2
      write(30, "(f11.4)", advance='no') tempo(i)
      !write(*, "(i9)", advance='no') i
   enddo
   !write(*, "(i9)") nl-1
   write(30, "(f11.4)") tempo(nl-1)

   write(30, "(i3, 1x, f11.4)", advance='no') rank(1), tempo(2)
   !write(*, "(i9)", advance='no') 2
   do i = 3, nl-1,2
      write(30, "(f11.4)", advance='no') tempo(i)
      !write(*, "(i9)", advance='no') i
   enddo
   !write(*, "(i9)") nl
   write(30, "(f11.4)") tempo(nl)
   
   
   print*, ''
   
   write(31, *) "reset"
   write(31, *) "set title 'Tempos Wall Time COAWST v3.7'"
   write(31, *) "set terminal png"
   write(31, *) "set output 'graf3.gnu.png'"
   write(31, *) "set xlabel 'Wall clock (s)'"
   write(31, *) "set ylabel 'Cores'"
   write(31, *) "set yrange [-10:1000]"
   write(31, *) "set xrange [0:300]"
   write(31, *) "set key left top"
   write(31, *) "plot 'graf3.gnu.input1' using  2:1 with line linecolor rgb 'black' title 'WRF Run', \"
   
   j=3
   do i=3, nl, 2
      if (func(i) == "ATM_ISend") then
         color="blue"
      endif
      if (func(i) == "ATM_Waits") then
         color="red"
      endif
      if (func(i) == "ATM_Waitr") then
         color="red"
      endif
      if (func(i) == "ATM_IRecv") then
         color="green"
      endif
      
   
      write(31, '(a, i2, a)') "      'graf3.gnu.input1' using ", j, ":1 with line linecolor rgb '" &
      //color//"' title '"//func(i)//"', \"
      j=j+1
   enddo
   write(31, '(a, i2, a)') "      'graf3.gnu.input1' using ", j, ":1 with line linecolor rgb 'black' title 'WRF Run'"
   
   
   
   print*, ''
end program
