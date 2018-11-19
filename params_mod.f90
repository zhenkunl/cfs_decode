!***********************************************************************
!  DESCRIPTION:
!       Module to load some necessary parameters to decode NCEP CFSv2
!       operational products
!
!  REVISION  HISTORY:
!       Prototype 10/2014 by Li Zhenkun, SCC
!       Revised 10/2018 by Li Zhenkun, SCC
!
!***********************************************************************
  module params_mod

    implicit none

!----------------------------------------------------------------------
!    Parameter definitions for namelist file
!
!    start_date   =  the start date of CFSv2 product
!    end_date     =  the end date of CFSv2 product
!    var_name     =  the varible name like "prate", "tmp2m"
!    input_dir    =  the directory of CFSv2 product
!    output_dir   =  the directory of output binary data
!
!----------------------------------------------------------------------

    integer                            ::  start_date, end_date
    character(256)                     ::  var_name
    character(256)                     ::  input_dir, output_dir

    public          :: init_params   !initialize the parameters listed above

    contains

!----------------------------------------------------------------------
!   Public subroutine to initialize control parameters
!----------------------------------------------------------------------
    subroutine init_params(filename, ierr)

      implicit none

!----------------------------------------------------------------------
!   dummy arguments
!----------------------------------------------------------------------
      character(len=*), intent(in)       ::  filename   ! namelist file name
      integer, intent(out)               ::  ierr       ! error identifier

!------------------------------------------------------------------
!   local variables
!------------------------------------------------------------------
      character(len=*), parameter        ::  subname = '(init_params)'
      logical                            ::  lexist
      integer                            ::  iunit = 255 ! file unit

      namelist /params/ start_date, end_date, var_name, input_dir, output_dir

!------------------------------------------------------------------
!   Open the namelist file and obtain parameter's value
!------------------------------------------------------------------
      open( unit = iunit, file = filename, status = 'old', action = 'read', err = 100 )

      read( iunit, params, err = 200 )

      ierr = 0
      return

100   write( 6, '(3a)' ) subname, 'can NOT read namelist file ', trim(filename)
      ierr = 1
      return

200   write( 6, '(3a)' ) subname, 'can NOT read namelist stanza: params  ',  trim(filename)
      ierr = 1
      close( iunit )

    end subroutine init_params

  end module params_mod
