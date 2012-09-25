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
  real(dp), parameter :: c=2.99792458_dp

  !Cosmology parameters --- from CAMB.
  real(dp), parameter :: h_P = 6.62606896e-34_dp
  real(dp), parameter :: G=6.67428e-11_dp
  real(dp), parameter :: sigma_thomson = 6.6524616e-29_dp
  real(dp), parameter :: sigma_boltz = 5.6704e-8_dp  
  real(dp), parameter :: k_B = 1.3806504e-23_dp 
  real(dp), parameter :: m_p = 1.672621637e-27_dp  ! 1.672623e-27_dp
  real(dp), parameter :: m_H = 1.673575e-27_dp !av. H atom
  real(dp), parameter :: m_e = 9.10938215e-31_dp
  real(dp), parameter :: mass_ratio_He_H = 3.9715_dp
  !Default to private.  Can change when need to use them.
  real(dp), parameter :: Gyr=3.1556926e16_dp
  real(dp), parameter :: Mpc = 3.085678e22_dp !seem to be different definitions of this?
  real(dp), parameter :: MPC_in_sec = Mpc/c ! Mpc/c = 1.029272d14 in SI units      
  real(dp), parameter :: barssc0= k_B / m_p / c**2
  real(dp), parameter :: kappa=8._dp*pi*G
  real(dp), parameter :: a_rad = 8._dp*pi**5*k_B**4/15/c**3/h_p**3  
  real(dp), parameter :: f_21cm = 1420.40575e6_dp, l_21cm= c/f_21cm, T_21cm = h_P*f_21cm/k_B
  real(dp), parameter :: A10 = 2.869e-15, B10 = l_21cm**3/2/h_P/c*A10
  real(dp), parameter :: line21_const = 3*l_21cm**2*C*h_P/32/pi/k_B*A10 * Mpc_in_sec * 1000
  real(dp), parameter :: COBE_CMBTemp = 2.726 !(Fixsen 2009)


  !Set default to private.
  private
  public :: dp, pi, e, c, h_P, G, sigma_thomson, sigma_boltz, k_B, m_p, m_H,&
  & m_e, mass_ratio_He_H

end module types
