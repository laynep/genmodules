!This is a module to implement workarounds for some features which may become
!necessary when writing programs in Fortran 90/95 that will be compiled without
!support for Fortran 2003/2008.

module features
  implicit none

  !Private by default.
  private
  public :: newunit

  contains

  ! This is a simple function to search for an available unit.
  ! LUN_MIN and LUN_MAX define the range of possible LUNs to check.
  ! The UNIT value is returned by the function, and also by the optional
  ! argument. This allows the function to be used directly in an OPEN
  ! statement, and optionally save the result in a local variable.
  ! If no units are available, -1 is returned.

  !This is taken verbatim from http://fortranwiki.org/fortran/show/newunit	
  integer function newunit(u)
    implicit none

    integer, intent(out), optional :: u
    integer, parameter :: LUN_MIN=10, LUN_MAX=10000
    logical :: op
    integer :: lun

    newunit=-1
    do lun=LUN_MIN,LUN_MAX
      inquire(unit=lun,opened=op)
      if (.not. op) then
        newunit=lun
        exit
      end if
    end do
    if (present(u)) u=newunit

  end function newunit

  !This shows how to place a non-advancing status counter in a loop, assuming
  !that the loop starts at i=1 and goes until i=ending.  Can optionally declare
  !an interval (integer) so that the output is printed only at approximately
  !these percentages, e.g. interval=10 gives 0%, 10%, 20%,...,100%.  Defaults to
  !an interval of 5.

  !Taken from web.utah.edu/thorne/computing/Handy_Fortran_Tricks.pdf.
  subroutine loop_progress_print(i,ending,start,interval)
    implicit none

    integer, intent(in) :: i, ending
    integer, optional, intent(in) :: interval, start
    integer :: step, begin

    if (present(start)) then
      begin=start
    else
      begin=1
    end if

    if (present(interval)) then
      step=ceiling(real(ending-begin-1)/real(interval))
      if (i==begin .and. (interval>100 .or. interval<1)) then
        print*,"Progress display interval out-of-bounds.  Supressing..."
        return
      else if (interval>100 .or. interval<1) then
        return
      else if (mod(i,step)==0) then
        call write_the_percent()
      end if
    else
      step=ceiling(real(ending-begin-1)/20.0)
      if (mod(i,step)==0) call write_the_percent()
    end if

    contains

      subroutine write_the_percent
        write(*,fmt="(A1,A,t21,F6.2,A)",advance="no") achar(13), &
             & " Percent Complete: ", (real(i-begin)/real(ending))*100.0, "%"
      end subroutine write_the_percent

  end subroutine loop_progress_print

end module features
