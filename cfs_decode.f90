!***********************************************************************
!  DESCRIPTION:
!       Main Program to decode NCEP CFSv2 operational products using ECMWF
!       GRIB_API library
!
!  PRECONDITIONS REQUIRED:
!       NCEP CFSv2 raw data should be processed on Unix-like platform
!       or in Windows/Cygwin environment, and GRIB_API library need to be
!       installed first
!
!  REVISION  HISTORY:
!       Prototype 01/2015 by Li Zhenkun, SCC
!       Revised 11/2018 by Li Zhenkun, SCC
!
!***********************************************************************
  program cfs_decode

    use params_mod
    use date_link_mod
    use get_cfs_field_mod
    use output_mod

    implicit none

    integer                            ::  nlon, nlat
    integer, parameter                 ::  nday = 43
    character(256)                     ::  progname, namelistfile
    integer                            ::  start_year, start_mon, start_day
    integer                            ::  end_year, end_mon, end_day
    integer                            ::  day_num
    type(date_link), pointer           ::  date_link_pointer
    integer                            ::  ierr
    integer                            ::  days_month(12) = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)
    real(4), allocatable               ::  var(:, :, :), var2(:, :, :)
    integer                            ::  iday

!----------------------------------------------------------------------
!   Get the names of this program and namelist control file
!----------------------------------------------------------------------
    call getarg(0, progname)
    call getarg(1, namelistfile)
    call init_params(namelistfile, ierr)
    if ( ierr /= 0 ) then
      write( 6, '(a)' ) 'Parameter initialization not completed'
      write( 6, '(a)' ) 'Usage : '
      write( 6, '(3a)' ) '        ', trim(progname), ' namelist'
      write( 6, '(a)' ) ' '
      write( 6, '(a)' ) 'Check argument and namelist syntax'
      stop
    end if

    start_year = start_date/10000
    start_mon  = (start_date - start_year*10000)/100
    start_day  = mod(start_date, 100)
    end_year   = end_date/10000
    end_mon    = (end_date - end_year*10000)/100
    end_day    = mod(end_date, 100)

    if ( var_name == 'prate' .or. var_name == 'tmp2m' .or. var_name == 'wnd10m' ) then
      nlon = 384
      nlat = 190
    else
      nlon = 360
      nlat = 181
    end if
    allocate ( var(nlon, nlat, nday) )

    if ( var_name == 'wnd850' .or. var_name == 'wnd10m' ) then
      allocate ( var2(nlon, nlat, nday) )
    end if

    call init_date_link(start_year, start_mon, start_day, end_year, end_mon, end_day, day_num, ierr)
    if ( ierr /= 0 ) then
      write( 6, '(a, i4, 1x, i2, 1x, i2)' ) 'ERROR: can NOT initialize date link'
    end if
    date_link_pointer => date_link_head

    do iday = 1, day_num

      write( 6, '(a, i4, 1x, i2, 1x, i2)' ) 'Begin to get forecast data of ', date_link_pointer%date%year, date_link_pointer%date%mon, date_link_pointer%date%day
      if ( var_name == 'wnd850' .or. var_name == 'wnd10m' ) then
        call GetCfsField(input_dir, date_link_pointer%date%year, date_link_pointer%date%mon, date_link_pointer%date%day, nday, nlat, nlon, var_name, var, var2, ierr)
      else
        call GetCfsField(input_dir, date_link_pointer%date%year, date_link_pointer%date%mon, date_link_pointer%date%day, nday, nlat, nlon, var_name, var, ierr)
      end if

      if ( ierr /= 0 ) then
        write( 6, '(a, i4, 1x, i2, 1x, i2, a)' ) 'WARNING: forecast data of ', date_link_pointer%date%year, date_link_pointer%date%mon, date_link_pointer%date%day, ' is incomplete'
        date_link_pointer => date_link_pointer%next
        cycle
      end if

      write( 6, '(a, i4, 1x, i2, 1x, i2)' ) 'Begin to write forecast data of ', date_link_pointer%date%year, date_link_pointer%date%mon, date_link_pointer%date%day

      if ( var_name == 'wnd850' .or. var_name == 'wnd10m' ) then
        call write_out(output_dir, date_link_pointer%date%year, date_link_pointer%date%mon, date_link_pointer%date%day, nday, nlat, nlon, var_name, var, var2, ierr)
      else
        call write_out(output_dir, date_link_pointer%date%year, date_link_pointer%date%mon, date_link_pointer%date%day, nday, nlat, nlon, var_name, var, ierr)
      end if

      if ( ierr /= 0 ) then
        write( 6, '(a, i4, 1x, i2, 1x, i2, a)' ) 'ERROR: can NOT write forecast data of ', date_link_pointer%date%year, date_link_pointer%date%mon, date_link_pointer%date%day, ' to binary'
        date_link_pointer => date_link_pointer%next
        cycle
      end if

      date_link_pointer => date_link_pointer%next

    end do

    call release_date_link(day_num)

    deallocate ( var )
    if ( var_name == 'wnd850' .or. var_name == 'wnd10m' ) then
      deallocate ( var2 )
    end if

    write( 6, '(a)' ) 'Successfully complete the mission'

  end program cfs_decode
