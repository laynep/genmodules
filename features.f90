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
  ! local
    integer, parameter :: LUN_MIN=10, LUN_MAX=1000
    logical :: op
    integer :: lun
  ! begin
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

end module features
