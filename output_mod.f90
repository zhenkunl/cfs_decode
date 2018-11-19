!***********************************************************************
!  DESCRIPTION:
!       Module to write decoded field(s) to binary file
!
!  REVISION  HISTORY:
!       Prototype 01/2015 by Li Zhenkun, SCC
!       Revised 10/2018 by Li Zhenkun, SCC
!
!***********************************************************************

  module output_mod

    implicit none

    public      :: write_out

    interface write_out
      module procedure write_out_single
      module procedure write_out_multiple
    end interface

    contains

!----------------------------------------------------------------------
!   Public subroutine to write out single varible
!----------------------------------------------------------------------
    subroutine write_out_single(output_dir, fct_year, fct_mon, fct_day, nday, nlat, nlon, var_name, var, ierr)

      implicit none

!----------------------------------------------------------------------
!   dummy arguments
!----------------------------------------------------------------------
      character(len=*), intent(in)       ::  output_dir
      integer, intent(in)                ::  fct_year
      integer, intent(in)                ::  fct_mon
      integer, intent(in)                ::  fct_day
      integer, intent(in)                ::  nday
      integer, intent(in)                ::  nlat
      integer, intent(in)                ::  nlon
      character(len=*), intent(in)       ::  var_name
      real(4), intent(in)                ::  var(nlon, nlat, nday)
      integer, intent(out)               ::  ierr

!----------------------------------------------------------------------
!   local variables
!----------------------------------------------------------------------
      character(len=*), parameter        ::  subname = '(write_out_single)'
      character(4)                       ::  fct_year_s
      character(2)                       ::  fct_mon_s
      character(2)                       ::  fct_day_s
      character(8)                       ::  fct_date
      integer                            ::  iunit
      character(256)                     ::  filename
      integer                            ::  status
      integer                            ::  i, irec

      write( fct_year_s, '(i4)' ) fct_year
      fct_mon_s = char(fct_mon/10+48)//char(mod(fct_mon,10)+48)
      fct_day_s = char(fct_day/10+48)//char(mod(fct_day,10)+48)
      fct_date = fct_year_s//fct_mon_s//fct_day_s

      ierr = 0
      iunit = 101
      filename = trim(output_dir)//'/'//trim(var_name)//'-'//fct_date//'-cfs-forecast.dat'
      open( unit = iunit, file = trim(filename), form = 'unformatted', access = 'direct', recl = nlon*nlat, iostat = status )
      if ( status /= 0 ) then
         write( 6, '(3a)' ) subname, 'ERROR: can NOT rightly open file ', trim(filename)
         ierr = 1
         return
      end if

      irec = 1
      do i = 1, nday
        write( iunit, rec = irec ) var(:, nlat:1:-1, i)
        irec = irec + 1
      end do

      close( iunit )

    end subroutine write_out_single

!----------------------------------------------------------------------
!   Public subroutine to write out multiple varibles
!----------------------------------------------------------------------
    subroutine write_out_multiple(output_dir, fct_year, fct_mon, fct_day, nday, nlat, nlon, var_name, var, var2, ierr)

      implicit none

!----------------------------------------------------------------------
!   dummy arguments
!----------------------------------------------------------------------
      character(len=*), intent(in)       ::  output_dir
      integer, intent(in)                ::  fct_year
      integer, intent(in)                ::  fct_mon
      integer, intent(in)                ::  fct_day
      integer, intent(in)                ::  nday
      integer, intent(in)                ::  nlat
      integer, intent(in)                ::  nlon
      character(len=*), intent(in)       ::  var_name
      real(4), intent(in)                ::  var(nlon, nlat, nday)
      real(4), intent(in)                ::  var2(nlon, nlat, nday)
      integer, intent(out)               ::  ierr

!----------------------------------------------------------------------
!   local variables
!----------------------------------------------------------------------
      character(len=*), parameter        ::  subname = '(write_out_multiple)'
      character(4)                       ::  fct_year_s
      character(2)                       ::  fct_mon_s
      character(2)                       ::  fct_day_s
      character(8)                       ::  fct_date
      integer                            ::  iunit
      character(256)                     ::  filename
      integer                            ::  status
      integer                            ::  i, irec

      write( fct_year_s, '(i4)' ) fct_year
      fct_mon_s = char(fct_mon/10+48)//char(mod(fct_mon,10)+48)
      fct_day_s = char(fct_day/10+48)//char(mod(fct_day,10)+48)
      fct_date = fct_year_s//fct_mon_s//fct_day_s

      ierr = 0
      iunit = 102
      filename = trim(output_dir)//'/'//trim(var_name)//'-'//fct_date//'-cfs-forecast.dat'

      open( unit = iunit, file = trim(filename), form = 'unformatted', access = 'direct', recl = nlon*nlat, iostat = status )
      if ( status /= 0 ) then
         write( 6, '(3a)' ) subname, 'ERROR: can NOT rightly open file ', trim(filename)
         ierr = 1
         return
      end if

      irec = 1
      do i = 1, nday
        write( iunit, rec = irec ) var(:, nlat:1:-1, i)
        irec = irec + 1
        write( iunit, rec = irec ) var2(:, nlat:1:-1, i)
        irec = irec + 1
      end do

      close( iunit )

    end subroutine write_out_multiple

  end module output_mod
