      MODULE get_hash_mod
!
!svn $Id: get_hash.F 1066 2021-05-14 21:47:45Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This module includes several routines to compute the "checksum" of  !
!  a floating-point array using one of the following methods:          !
!                                                                      !
!    bitsum        Simple bit-by-bit order-invariant sum algorithm     !
!                                                                      !
!  The available methods compute the "checksum" from characters and    !
!  integers. For floating-point data, its values are interpreted as    !
!  unsigned bytes. Here, we have the problem that Fortran does not     !
!  support unsigned integers.  Therefore, the intrinsic function       !
!  TRANSFER is used to convert for 32-bit reals to 32-bit integers.    !
!                                                                      !
!  The "checksum" value can be used during debugging to compare        !
!  input data solutions from different versions of ROMS when           !
!  implementing new algorithms. It is only available for reading       !
!  and writting data in input/output NetCDF files.                     !
!                                                                      !
!  The function "bitsum" is the default method in ROMS since it        !
!  allows tiled I/O data when the PIO library is used. Notice that     !
!  reduction communications are not required with the standard NetCDF  !
!  library since all the data is processed by the master.              !
!                                                                      !
!=======================================================================
!
      USE mod_kinds
!
      USE mod_scalars,    ONLY : HashMethod, exit_flag
      USE mod_iounits,    ONLY : stdout
      USE strings_mod,    ONLY : uppercase
!
      implicit none
!
      PUBLIC  :: get_hash
      PRIVATE :: bitsum
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE get_hash (A, Asize, hash, Lreduce)
!***********************************************************************
!
!
!  Imported variable declarations.
!
      logical, intent(in), optional :: Lreduce
!
      integer, intent(in) :: Asize
      integer(i8b), intent(out) :: hash
!
      real(r8), intent(in) :: A(:)
!
!  Local variable declarations.
!
      logical, save :: first = .TRUE.
!
!-----------------------------------------------------------------------
!  Compute checksum for the requested floating point vector.
!-----------------------------------------------------------------------
!
      hash=0_i8b
!
      SELECT CASE (uppercase(TRIM(HashMethod)))
        CASE ('BITSUM')
          CALL bitsum (A, Asize, hash, Lreduce)
        CASE DEFAULT
          WRITE (stdout,10) TRIM(HashMethod)
          exit_flag=5
      END SELECT
!
  10  FORMAT (/,' GET_HASH - Illegal checksum method: ',a)
!
      RETURN
      END SUBROUTINE get_hash
!
!***********************************************************************
      SUBROUTINE bitsum (A, Asize, hash, Lreduce)
!***********************************************************************
!                                                                      !
!  Computes the checksum of a 1D floating-point by casting each value  !
!  to an integer to faciliate the invariant order of the sum in tiled  !
!  parallel applications. A real number can be represented with a set  !
!  64-bit integers (Hallberg and Adcroft, 2014).                       !
!                                                                      !
!  Reference:                                                          !
!                                                                      !
!    Hallberg, R. and A. Adcroft, 2014: An order-invariant real-to-    !
!      integer conversion sum, Parallel Computing, 40, 140-143,        !
!      doi:10.1016/j.parco.2014.04.007.                                !
!                                                                      !
!***********************************************************************
!
!  Imported variable declarations.
!
      logical, intent(in), optional :: Lreduce
!
      integer, intent(in) :: Asize
      integer(i8b) :: hash
!
      real(r8), intent(in) :: A(:)
!
!  Local variable declarations.
!
      integer, parameter :: Ak = KIND(A)
      integer :: i, j
!
      integer(i8b) :: Ac, Asum
!
!-----------------------------------------------------------------------
!  Compute checksum by counting bit-by-bit and summing.
!-----------------------------------------------------------------------
!
!  Here, the "POPCNT" function counts the number of set bits in a
!  machine instruction.  For example, for two 8-bit words operated
!  with XOR, we get
!
!    00100110
!    01100000
!   ----------
!    01000110
!
!    POPCNT(01000110) = 3       'counts the number of bits set to 1'
!
!  The POPCNT is available in all modern Fortran compilers and CPU
!  architectures.
!
      Asum=0_i8b
      DO i=1,Asize
        Ac=POPCNT(TRANSFER(ABS(A(i)), 1_Ak))
        Asum=Asum+Ac
      END DO
      hash=Asum
!
      RETURN
      END SUBROUTINE bitsum
!
      END MODULE get_hash_mod
