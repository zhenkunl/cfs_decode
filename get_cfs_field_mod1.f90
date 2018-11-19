!***********************************************************************
!  DESCRIPTION:
!       Module to get data from CFSv2 forecast products
!
!  REVISION  HISTORY:
!       Prototype 01/2015 by Li Zhenkun, SCC
!       Revised 10/2018 by Li Zhenkun, SCC
!
!***********************************************************************
  module get_cfs_field_mod

    use eccodes

    implicit none

    public       ::  GetCfsField

    interface GetCfsField
      module procedure GetCfsField_single
      module procedure GetCfsField_multiple
    end interface

    contains

!----------------------------------------------------------------------
!   Public subroutine to obtain single varible from CFSv2 product.
!   ierr = 0 when reading cfs file successfully, otherwise ierr = 1
!----------------------------------------------------------------------
    subroutine GetCfsField_single(input_dir, fct_year, fct_mon, fct_day, nday, nlat, nlon, var_name, var, ierr)

      implicit none

!----------------------------------------------------------------------
!   dummy arguments
!----------------------------------------------------------------------
      character(len=*), intent(in)       ::  input_dir
      integer, intent(in)                ::  fct_year
      integer, intent(in)                ::  fct_mon
      integer, intent(in)                ::  fct_day
      integer, intent(in)                ::  nday
      integer, intent(in)                ::  nlat
      integer, intent(in)                ::  nlon
      character(len=*), intent(in)       ::  var_name
      real(4), intent(out)               ::  var(nlon, nlat, nday)
      integer, intent(out)               ::  ierr

!----------------------------------------------------------------------
!   local variables
!----------------------------------------------------------------------
      character(len=*), parameter        ::  subname = '(GetCfsField_single)'
      character(4)                       ::  fct_year_s
      character(2)                       ::  fct_mon_s
      character(2)                       ::  fct_day_s
      character(8)                       ::  fct_date
      character(2)                       ::  run(4) = (/'01' , '02' , '03' , '04'/)
      character(2)                       ::  runtime(4) = (/'00' , '06' , '12' , '18'/)
      character(256)                     ::  pathaddname
      logical                            ::  lexist
      integer                            ::  infile
      integer                            ::  status
      integer                            ::  numOfMessages
      integer, allocatable               ::  gribid(:)
      real(4), allocatable               ::  values(:)
      real(4), allocatable               ::  var_6h(:, :, :)
      real(4)                            ::  var_daily(16, nlon, nlat, nday)
      integer                            ::  i, j, k, l

      ierr = 0
      write( fct_year_s, '(i4)' ) fct_year
      fct_mon_s = char(fct_mon/10 + 48)//char(mod(fct_mon, 10) + 48)
      fct_day_s = char(fct_day/10 + 48)//char(mod(fct_day, 10) + 48)
      fct_date = fct_year_s//fct_mon_s//fct_day_s

      do i = 1, 4     !run cycle
        do j = 1, 4   !runtime cycle
          pathaddname = trim(input_dir)//'/'//fct_date//'/'//trim(var_name)//"."//run(i)//"."//fct_date//runtime(j)//".daily.grb2"
          ! pathaddname = trim(input_dir)//'/cfs.'//fct_date//'/'//runtime(j)//'/'//trim(var_name)//"."//run(i)//"."//fct_date//runtime(j)//".daily.grb2"
          ! pathaddname is in the form of "prate.01.2013060218.daily.grb2"

          inquire( file = trim(pathaddname), exist = lexist )
          if ( .not. lexist ) then
            write( 6, '(4a)' ) subname, 'WARNING: ', trim(pathaddname) , ' is NOT available'
            ierr = 1
            return
          else
            write( 6, '(3a)' ) subname, 'Read the GRIB file ', trim(pathaddname)
          end if

          call codes_open_file(infile, pathaddname, 'r', status)
          if ( status /= CODES_SUCCESS ) then
            write( 6, '(3a)' ) subname, 'WARNING: can NOT rightly open GRIB file ', trim(pathaddname)
            ierr = 1
            return
          end if

          call codes_count_in_file(infile, numOfMessages, status)
          if ( status /= CODES_SUCCESS ) then
            write( 6, '(3a)' ) subname, 'WARNING: can NOT count number of messages in ', trim(pathaddname)
            ierr = 1
            return
          end if

          allocate( gribid(numOfMessages) )
          allocate( values(nlon*nlat) )
          allocate( var_6h(nlon, nlat, min(numOfMessages, 178)) )

          ! load the messages from the file.
          do k = 1, min(numOfMessages, 178)
            call codes_grib_new_from_file(infile, gribid(k), status)
            if ( status /= CODES_SUCCESS ) then
              write( 6, '(2a, i3, 2a)' ) subname, 'WARNING: can NOT read the ', k, 'th message of GRIB file ', trim(pathaddname)
              ierr = 1
              return
            end if

            ! get data values
            call codes_get(gribid(k), 'values', values, status)
            if ( status /= CODES_SUCCESS ) then
              write( 6, '(2a, i3, 2a)' ) subname, 'WARNING: can NOT get the ', k, 'th message of GRIB file ', trim(pathaddname)
              ierr = 1
              return
            end if

            var_6h(:, :, k) = reshape(values, (/nlon, nlat/))

            call codes_release(gribid(k), status)
            if ( status /= CODES_SUCCESS ) then
              write( 6, '(2a, i3, 2a)' ) subname, 'WARNING: can NOT release the ', k, 'th message of GRIB file ', trim(pathaddname)
              ierr = 1
              return
            end if
          end do

          do l = 1, nday
            if ( j == 1 ) then
              var_daily(4*i-4+j, :, :, l) = sum(var_6h(:, :, 4*l+3:4*l+6), dim=3)
            else if ( j == 2 ) then
              var_daily(4*i-4+j, :, :, l) = sum(var_6h(:, :, 4*l+2:4*l+5), dim=3)
            else if ( j == 3 ) then
              var_daily(4*i-4+j, :, :, l) = sum(var_6h(:, :, 4*l+1:4*l+4), dim=3)
            else
              var_daily(4*i-4+j, :, :, l) = sum(var_6h(:, :, 4*l:4*l+3), dim=3)
            end if
          end do

          deallocate( gribid )
          deallocate( values )
          deallocate( var_6h )

          call codes_close_file(infile, status)
          if ( status /= CODES_SUCCESS ) then
            write( 6, '(3a)' ) subname, 'WARNING: can NOT close GRIB file ', trim(pathaddname)
            ierr = 1
            return
          end if

        end do
      end do

      if ( var_name == 'prate' ) then
        var_daily = var_daily * 6 * 3600
      else
        var_daily = var_daily / 4.0
      end if

      var = sum(var_daily, dim=1) / 16.0

    end subroutine GetCfsField_single

!----------------------------------------------------------------------
!   Public subroutine to obtain multiple varibles from CFSv2 product.
!   ierr = 0 when reading cfs file successfully, otherwise ierr = 1
!----------------------------------------------------------------------
    subroutine GetCfsField_multiple(input_dir, fct_year, fct_mon, fct_day, nday, nlat, nlon, var_name, var, var2, ierr)

      implicit none

!----------------------------------------------------------------------
!   dummy arguments
!----------------------------------------------------------------------
      character(len=*), intent(in)       ::  input_dir
      integer, intent(in)                ::  fct_year
      integer, intent(in)                ::  fct_mon
      integer, intent(in)                ::  fct_day
      integer, intent(in)                ::  nday
      integer, intent(in)                ::  nlat
      integer, intent(in)                ::  nlon
      character(len=*), intent(in)       ::  var_name
      real(4), intent(out)               ::  var(nlon, nlat, nday)
      real(4), intent(out)               ::  var2(nlon, nlat, nday)
      integer, intent(out)               ::  ierr

!----------------------------------------------------------------------
!   local variables
!----------------------------------------------------------------------
      character(len=*), parameter        ::  subname = '(GetCfsField_multiple)'
      character(4)                       ::  fct_year_s
      character(2)                       ::  fct_mon_s
      character(2)                       ::  fct_day_s
      character(8)                       ::  fct_date
      character(2)                       ::  run(4) = (/'01' , '02' , '03' , '04'/)
      character(2)                       ::  runtime(4) = (/'00' , '06' , '12' , '18'/)
      character(256)                     ::  pathaddname
      logical                            ::  lexist
      integer                            ::  infile
      integer                            ::  status
      integer                            ::  numOfMessages
      integer, allocatable               ::  gribid(:)
      real(4), allocatable               ::  values(:)
      character(256)                     ::  shortName
      real(4), allocatable               ::  var_6h(:, :, :), var2_6h(:, :, :)
      real(4)                            ::  var_daily(16, nlon, nlat, nday), var2_daily(16, nlon, nlat, nday)
      integer                            ::  i, j, k, l
      integer                            ::  unum, vnum

      ierr = 0
      write( fct_year_s, '(i4)' ) fct_year
      fct_mon_s = char(fct_mon/10 + 48)//char(mod(fct_mon, 10) + 48)
      fct_day_s = char(fct_day/10 + 48)//char(mod(fct_day, 10) + 48)
      fct_date = fct_year_s//fct_mon_s//fct_day_s

      do i = 1, 4    !run cycle
        do j = 1, 4   !runtime cycle
          pathaddname = trim(input_dir)//'/'//fct_date//'/'//trim(var_name)//"."//run(i)//"."//fct_date//runtime(j)//".daily.grb2"
          ! pathaddname = trim(input_dir)//'/cfs.'//fct_date//'/'//runtime(j)//'/'//trim(var_name)//"."//run(i)//"."//fct_date//runtime(j)//".daily.grb2"
          ! pathaddname is in the form of "prate.01.2013060218.daily.grb2"

          inquire( file = trim(pathaddname), exist = lexist )
          if ( .not. lexist ) then
            write( 6, '(4a)' ) subname, 'WARNING:', trim(pathaddname) , ' is NOT available'
            ierr = 1
            return
          else
            write( 6, '(3a)' ) subname, 'Read the GRIB file ', trim(pathaddname)
          end if

          call codes_open_file(infile, pathaddname, 'r', status)
          if ( status /= CODES_SUCCESS ) then
            write( 6, '(3a)' ) subname, 'WARNING: can NOT rightly open GRIB file ', trim(pathaddname)
            ierr = 1
            return
          end if

          call codes_count_in_file(infile, numOfMessages, status)
          if ( status /= CODES_SUCCESS ) then
            write( 6, '(3a)' ) subname, 'WARNING: can NOT count number of messages in ', trim(pathaddname)
            ierr = 1
            return
          end if

          allocate( gribid(numOfMessages) )
          allocate( values(nlon*nlat) )
          allocate( var_6h(nlon, nlat, min(numOfMessages, 178)) )
          allocate( var2_6h(nlon, nlat, min(numOfMessages, 178)) )

          unum = 1
          vnum = 1

          ! load the messages from the file.
          do k = 1, numOfMessages
            call codes_grib_new_from_file(infile, gribid(k), status)
            if ( status /= CODES_SUCCESS ) then
              write( 6, '(2a, i3, 2a)' ) subname, 'WARNING: can NOT read the ', k, 'th message of GRIB file ', trim(pathaddname)
              ierr = 1
              return
            end if

            call codes_get(gribid(k), 'shortName', shortName, status)
            if ( status /= CODES_SUCCESS ) then
              write( 6, '(2a, i3, 2a)' ) subname, 'WARNING: can NOT get shortName in the ', k, 'th message of GRIB file ', trim(pathaddname)
              ierr = 1
              return
            end if

            ! get data values
            call codes_get(gribid(k), 'values', values)
            if ( status /= CODES_SUCCESS ) then
              write( 6, '(2a, i3, 2a)' ) subname, 'WARNING: can NOT get the ', k, 'th message of GRIB file ', trim(pathaddname)
              ierr = 1
              return
            end if

            if ( trim(shortName) == 'u' .or. trim(shortName) == '10u' ) then
              if ( unum <= 178 ) then
                var_6h(:, :, unum) = reshape(values, (/nlon, nlat/))
                unum = unum + 1
              else
                cycle
              end if
            else if ( trim(shortName) == 'v' .or. trim(shortName) == '10v' ) then
              if ( vnum <= 178 ) then
                var2_6h(:, :, vnum) = reshape(values, (/nlon, nlat/))
                vnum = vnum + 1
              else
                cycle
              end if
            else
              write( 6, '(2a, i3, 2a)' ) subname, 'WARNING: can NOT assign values in ', k, 'th message of GRIB file ', trim(pathaddname)
              ierr = 1
              return
            end if

            call codes_release(gribid(k), status)
            if ( status /= CODES_SUCCESS ) then
              write( 6, '(2a, i3, 2a)' ) subname, 'WARNING: can NOT release the ', k, 'th message of GRIB file ', trim(pathaddname)
              ierr = 1
              return
            end if

            if ( unum > 178 .and. vnum > 178 ) exit
          end do

          do l = 1, nday
            if ( j == 1 ) then
              var_daily(4*i-4+j, :, :, l) = sum(var_6h(:, :, 4*l+3:4*l+6), dim=3)
              var2_daily(4*i-4+j, :, :, l) = sum(var2_6h(:, :, 4*l+3:4*l+6), dim=3)
            else if ( j == 2 ) then
              var_daily(4*i-4+j, :, :, l) = sum(var_6h(:, :, 4*l+2:4*l+5), dim=3)
              var2_daily(4*i-4+j, :, :, l) = sum(var2_6h(:, :, 4*l+2:4*l+5), dim=3)
            else if ( j == 3 ) then
              var_daily(4*i-4+j, :, :, l) = sum(var_6h(:, :, 4*l+1:4*l+4), dim=3)
              var2_daily(4*i-4+j, :, :, l) = sum(var2_6h(:, :, 4*l+1:4*l+4), dim=3)
            else
              var_daily(4*i-4+j, :, :, l) = sum(var_6h(:, :, 4*l:4*l+3), dim=3)
              var2_daily(4*i-4+j, :, :, l) = sum(var2_6h(:, :, 4*l:4*l+3), dim=3)
            end if
          end do

          deallocate( gribid )
          deallocate( values )
          deallocate( var_6h )
          deallocate( var2_6h )

          call codes_close_file(infile, status)
          if ( status /= CODES_SUCCESS ) then
            write( 6, '(3a)' ) subname, 'WARNING: can NOT close GRIB file ', trim(pathaddname)
            ierr = 1
            return
          end if

        end do
      end do

      var_daily = var_daily / 4.0
      var2_daily = var2_daily / 4.0

      var = sum(var_daily, dim=1) / 16.0
      var2 = sum(var2_daily, dim=1) / 16.0

    end subroutine GetCfsField_multiple

  end module get_cfs_field_mod
