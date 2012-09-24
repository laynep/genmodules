!Module that defines single, double, and quad precision and a few common
!numerical parameters.

module types
  implicit none

  !32-,, 64-, and 128-bit real types.
  integer, parameter :: single = selected_real_kind(6, 37)
  integer, parameter :: double = selected_real_kind(15, 307)
  integer, parameter :: quad  = selected_real_kind(33, 4931)

  !Choose which real type to use.
  integer, parameter :: dp=double

  !Common parameters.
  real(dp), parameter :: pi=3.14159265358979323846264338327950288_dp
  real(dp), parameter :: e=2.71828182845904523536028747135266249_dp

  !Set default to private.
  private
  public :: dp, pi, e

end module types
