!Generally useful numerical routines.
module numerical_routines
  use types, only : dp
  implicit none

  contains

  !*******************************************
  !Function that returns the value, y in (y_1,y_2), of a cubic Hermite interpolation -- cspline -- at the point x in (x_1,x_2) when the tangent values at end points (v_1,v_2) are known.  To build a full interpolation, combine the csplines of all desired points together piece-wise.

  pure real(dp) function cspline(x,x_1,x_2,y_1,y_2,v_1,v_2)
  implicit none

  	real(dp), intent(in) :: x,x_1,x_2
  	real(dp), intent(in) :: v_1,v_2, y_1, y_2
  	real(dp) :: h_00, h_01, h_10, h_11	!hermite basis functs.
  	real(dp) :: t
  	integer :: i
	
  	t=(x-x_1)/(x_2-x_1)
  	h_00 = 2*(t**3) -3*(t**2)+1
  	h_10 = t**3 -2*(t**2) + t
  	h_01 = -2*(t**3) + 3*(t**2)
  	h_11 = t**3 - t**2

  	cspline = h_00*y_1 + h_10*(x_2-x_1)*v_1 + h_01*y_2 + h_11*v_2


  end function cspline

  !Implements Simpson's rule integration of a function f over a range [a,b].
  !The function needs to be continuous over this range.
  !NOTE: error is prop to 4th deriv of f.
  real(dp) function simpson(f,a,b)
    implicit none

    interface
      pure real(dp) function f(x)
        use types, only : dp
        implicit none
        real(dp), intent(in) :: x
      end function
    end interface
    real(dp), intent(in) :: a, b
    real(dp) :: c

    c=(a+b)/2_dp
    simpson = ( (b-a)/6_dp)*( f(a) + 4*f(c) + f(b))

  end function simpson

end module numerical_routines
