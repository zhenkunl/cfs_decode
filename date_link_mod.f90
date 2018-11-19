!***********************************************************************
!  DESCRIPTION:
!       Module to process date_link type linked list
!
!  REVISION  HISTORY:
!       Prototype 11/2018 by Li Zhenkun, SCC
!
!***********************************************************************
  module date_link_mod

    implicit none

    type date_type
      integer                            ::  year
      integer                            ::  mon
      integer                            ::  day
    end type date_type

    type date_link
      type(date_type)                    ::  date
      type(date_link), pointer           ::  next
    end type date_link

    type(date_link), pointer             ::  date_link_head

    public      ::  init_date_link
    public      ::  release_date_link
    private     ::  nfeb

    contains

!----------------------------------------------------------------------
!   Public subroutine to obtain every single day saved by a date_link
!   type linked list and the number of days between two date windows
!----------------------------------------------------------------------
    subroutine init_date_link(start_year, start_mon, start_day, end_year, end_mon, end_day, day_num, ierr)

      implicit none

!----------------------------------------------------------------------
!   dummy arguments
!----------------------------------------------------------------------
      integer, intent(in)                ::  start_year, start_mon, start_day
      integer, intent(in)                ::  end_year, end_mon, end_day
      integer, intent(out)               ::  day_num
      integer, intent(out)               ::  ierr

!----------------------------------------------------------------------
!   local variables
!----------------------------------------------------------------------
      character(len=*), parameter        ::  subname = '(init_date_link)'
      type(date_link), pointer           ::  date_link_pointer
      integer                            ::  days_mon(12) = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)
      integer                            ::  iyear, imon, iday

      allocate( date_link_head, stat = ierr )
      if ( ierr /= 0 ) then
        write( 6, '(2a)' ) subname, 'ERROR: can NOT allocate memory space to pointer date_link_head'
        return
      end if

      nullify( date_link_head%next )

      date_link_pointer => date_link_head
      day_num = 0

      if ( start_year == end_year ) then

        if ( start_mon == end_mon ) then

          if ( start_day <= end_day ) then
            do iday = start_day, end_day
              date_link_pointer%date%year = start_year
              date_link_pointer%date%mon  = start_mon
              date_link_pointer%date%day  = iday
              day_num = day_num + 1
              allocate( date_link_pointer%next, stat = ierr )
              if ( ierr /= 0 ) then
                write( 6, '(2a)' ) subname, 'ERROR: can NOT allocate memory space to pointer date_link_pointer%next'
                return
              end if
              date_link_pointer => date_link_pointer%next
              nullify( date_link_pointer%next )
            end do
          else
            write( 6, '(2a)' ) subname, 'ERROR: When years and months are the same, end day must NOT be less than start day'
            write( 6, '(2a)' ) subname, 'Check namelist variables start_day and end_day'
            return
          end if

        else if (start_mon < end_mon) then

          do imon = start_mon, end_mon
             if ( imon == 2 ) days_mon(imon) = nfeb(start_year)
             if ( imon == start_mon ) then
                do iday = start_day, days_mon(imon)
                   date_link_pointer%date%year = start_year
                   date_link_pointer%date%mon  = imon
                   date_link_pointer%date%day  = iday
                   day_num = day_num + 1
                   allocate( date_link_pointer%next, stat = ierr )
                   if ( ierr /= 0 ) then
                      write( 6, '(2a)' ) subname, 'ERROR: can NOT allocate memory space to pointer date_link_pointer%next'
                      return
                   end if
                   date_link_pointer => date_link_pointer%next
                   nullify( date_link_pointer%next )
                end do
             else if ( imon == end_mon) then
                do iday = 1, end_day
                   date_link_pointer%date%year = start_year
                   date_link_pointer%date%mon  = imon
                   date_link_pointer%date%day  = iday
                   day_num = day_num + 1
                   allocate( date_link_pointer%next, stat = ierr )
                   if ( ierr /= 0 ) then
                      write( 6, '(2a)' ) subname, 'ERROR: can NOT allocate memory space to pointer date_link_pointer%next'
                      return
                   end if
                   date_link_pointer => date_link_pointer%next
                   nullify( date_link_pointer%next )
                end do
             else
                do iday = 1, days_mon(imon)
                   date_link_pointer%date%year = start_year
                   date_link_pointer%date%mon  = imon
                   date_link_pointer%date%day  = iday
                   day_num = day_num + 1
                   allocate( date_link_pointer%next, stat = ierr )
                   if ( ierr /= 0 ) then
                      write( 6, '(2a)' ) subname, 'ERROR: can NOT allocate memory space to pointer date_link_pointer%next'
                      return
                   end if
                   date_link_pointer => date_link_pointer%next
                   nullify( date_link_pointer%next )
                end do
             end if
          end do

        else

          write( 6, '(2a)' ) subname, 'ERROR: When years are the same, end month must NOT be less than start month'
          write( 6, '(2a)' ) subname, 'Check namelist variables start_mon and end_mon'
          return

        end if

      else if (start_year < end_year) then

        do iyear = start_year, end_year

          if (iyear == start_year) then

            do imon = start_mon, 12
               if ( imon == 2 ) days_mon(imon) = nfeb(iyear)
               if ( imon == start_mon ) then
                  do iday = start_day, days_mon(imon)
                     date_link_pointer%date%year = iyear
                     date_link_pointer%date%mon  = imon
                     date_link_pointer%date%day  = iday
                     day_num = day_num + 1
                     allocate( date_link_pointer%next, stat = ierr )
                     if ( ierr /= 0 ) then
                        write( 6, '(2a)' ) subname, 'ERROR: can NOT allocate memory space to pointer date_link_pointer%next'
                        return
                     end if
                     date_link_pointer => date_link_pointer%next
                     nullify( date_link_pointer%next )
                  end do
               else
                  do iday = 1, days_mon(imon)
                     date_link_pointer%date%year = iyear
                     date_link_pointer%date%mon  = imon
                     date_link_pointer%date%day  = iday
                     day_num = day_num + 1
                     allocate( date_link_pointer%next, stat = ierr )
                     if ( ierr /= 0 ) then
                        write( 6, '(2a)' ) subname, 'ERROR: can NOT allocate memory space to pointer date_link_pointer%next'
                        return
                     end if
                     date_link_pointer => date_link_pointer%next
                     nullify( date_link_pointer%next )
                  end do
               end if
            end do

          else if (iyear == end_year) then

            do imon = 1, end_mon
               if ( imon == 2 ) days_mon(imon) = nfeb(iyear)
               if ( imon == end_mon ) then
                  do iday = 1, end_day
                     date_link_pointer%date%year = iyear
                     date_link_pointer%date%mon  = imon
                     date_link_pointer%date%day  = iday
                     day_num = day_num + 1
                     allocate( date_link_pointer%next, stat = ierr )
                     if ( ierr /= 0 ) then
                        write( 6, '(2a)' ) subname, 'ERROR: can NOT allocate memory space to pointer date_link_pointer%next'
                        return
                     end if
                     date_link_pointer => date_link_pointer%next
                     nullify( date_link_pointer%next )
                  end do
               else
                  do iday = 1, days_mon(imon)
                     date_link_pointer%date%year = iyear
                     date_link_pointer%date%mon  = imon
                     date_link_pointer%date%day  = iday
                     day_num = day_num + 1
                     allocate( date_link_pointer%next, stat = ierr )
                     if ( ierr /= 0 ) then
                        write( 6, '(2a)' ) subname, 'ERROR: can NOT allocate memory space to pointer date_link_pointer%next'
                        return
                     end if
                     date_link_pointer => date_link_pointer%next
                     nullify( date_link_pointer%next )
                  end do
               end if
            end do

          else

            do imon = 1, 12
               if ( imon == 2 ) days_mon(imon) = nfeb(iyear)
               do iday = 1, days_mon(imon)
                  date_link_pointer%date%year = iyear
                  date_link_pointer%date%mon  = imon
                  date_link_pointer%date%day  = iday
                  day_num = day_num + 1
                  allocate( date_link_pointer%next, stat = ierr )
                  if ( ierr /= 0 ) then
                     write( 6, '(2a)' ) subname, 'ERROR: can NOT allocate memory space to pointer date_link_pointer%next'
                     return
                  end if
                  date_link_pointer => date_link_pointer%next
                  nullify( date_link_pointer%next )
               end do
            end do

          end if

        end do

      else

        write ( 6, '(2a)' ) subname, 'ERROR: End year must NOT be less than start year!'
        write ( 6, '(2a)' ) subname, 'Check namelist variables start_year and end_year'
        stop

      end if

    end subroutine init_date_link

!----------------------------------------------------------------------
!   Public subroutine to release date_link type linked list allocate by
!   public subroutine init_date_link()
!----------------------------------------------------------------------
    subroutine release_date_link(num)

      implicit none

!----------------------------------------------------------------------
!   dummy arguments
!----------------------------------------------------------------------
      integer, intent(in)                ::  num

!----------------------------------------------------------------------
!   local variables
!----------------------------------------------------------------------
      type(date_link), pointer           ::  date_link_pointer
      integer                            ::  i

      do i = 1, num
         date_link_pointer => date_link_head%next
         deallocate( date_link_head )
         date_link_head => date_link_pointer
      end do
      deallocate( date_link_head )

    end subroutine release_date_link

!----------------------------------------------------------------------
!   Private function to calculate the number of day in February for a
!   given year
!----------------------------------------------------------------------
    integer function nfeb(iyear)

      implicit none

!------------------------------------------------------------------
!   dummy arguments
!------------------------------------------------------------------
      integer           ::  iyear

      if ( (mod(iyear,4) == 0 .and. mod(iyear,100) /= 0) .or. mod(iyear,400) == 0 ) then
        nfeb = 29
      else
        nfeb = 28
      end if

      return

    end function nfeb

  end module date_link_mod
