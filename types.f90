!Module that defines single, double, and quad precision and a few common
!numerical parameters.

module types
  implicit none

  !32-,, 64-, and 128-bit real types.
  integer, parameter :: sp = selected_real_kind(6, 37)
  integer, parameter :: dp = selected_real_kind(15, 307)
  integer, parameter :: qp = selected_real_kind(33, 4931)

  !Common parameters.
  real(dp), parameter :: pi=3.14159265358979323846264338327950288_dp
  real(dp), parameter :: e=2.71828182845904523536028747135266249_dp

  private
  public :: sp, dp, qp, pi, e

end module types

