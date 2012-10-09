!Module that contains routines for random number generation.  Some of this has been taken (where indicated) from RosettaCode.org and from other sites on the net.

module rng
implicit none

interface init_random_seed
	module procedure init_random_seed_parallel
	module procedure init_random_seed_serial
end interface

interface shuffle
	module procedure shuffle_dp_dimn
	module procedure shuffle_int_dimn
	module procedure shuffle_dp_1
	module procedure shuffle_int_1
end interface

!Default to public.


contains

!Generate an array of n numbers sampled from a Gaussian normal distribution with mean and standard deviation given.
!Adapted from RosettaCode.org.
function normal(n,mean,std)
  use types, only : dp
  implicit none
 
  integer, intent(in) :: n
  integer :: i
  real(dp) :: normal(n), temp
  real(dp), intent(in) :: mean, std
  real(dp), parameter :: pi = 4.0*ATAN(1.0)

  !Get uniform distribution.
  call random_number(normal)
 
  !Now convert to normal distribution
  do i = 1, n-1, 2
    temp = std * SQRT(-2.0*LOG(normal(i))) * COS(2*pi*normal(i+1)) + mean
    normal(i+1) = std * SQRT(-2.0*LOG(normal(i))) * SIN(2*pi*normal(i+1)) + mean
    normal(i) = temp
  end do
 
end function normal

!Generate a random seed based on the clock time.
!Adapted from GNU site.
subroutine init_random_seed_serial()
implicit none
	integer :: i, n, clock
	integer, dimension(:), allocatable :: seed
          
	call random_seed(size = n)
	allocate(seed(n))
          
	call system_clock(count=clock)
          
	seed = clock + 37* (/ (i - 1, i = 1, n) /)
	call random_seed(PUT = seed)
          
	deallocate(seed)
end subroutine init_random_seed_serial


!Generate a random seed based on the clock time --- For use in parallel.
subroutine init_random_seed_parallel(rank)
implicit none
	integer :: i, n, clock
	integer, dimension(:), allocatable :: seed
	integer, intent(in) :: rank

	call random_seed(size = n)
	allocate(seed(n))

	call system_clock(count=clock)

	seed = clock + 37*rank* (/ (i - 1, i = 1, n) /)
	call random_seed(PUT = seed)

	deallocate(seed)
end subroutine init_random_seed_parallel


!Subroutine to shuffle an array by Knuth Shuffle.
!Inspired by RosettaCode.org.
subroutine shuffle_int_1(a)
implicit none
	integer, intent(inout) :: a(:)
	integer :: i, randpos, temp
	real :: r
 
	!Count backwards
	do i = size(a), 2, -1
		!Get a random number.
		call random_number(r)
		randpos = int(r * i) + 1
		!Exchange the rows.
		temp = a(randpos)
		a(randpos) = a(i)
		a(i) = temp
	end do
 
end subroutine shuffle_int_1

subroutine shuffle_dp_dimn(a)
  use types, only : dp
  implicit none

  real(dp), dimension(:,:), intent(inout) :: a
	real(dp), dimension(size(a,2)) :: temp
	integer :: i, hold
	real(dp) :: rand
	
	!Count backwards.
	do i=size(a,1), 1, -1
		!Generate a random int from 1-i.
		call random_number(rand)
		hold=int(rand*i)+1
		!Swap the ith row with the rand row.
		temp(:)=a(hold,:)
		a(hold,:)=a(i,:)
		a(i,:) = temp(:)
	end do

end subroutine shuffle_dp_dimn

subroutine shuffle_dp_1(a)
  use types, only : dp
  implicit none

	real(dp), dimension(:), intent(inout) :: a
	real(dp) :: temp
	integer :: i, hold
	real(dp) :: rand
	
	!Count backwards.
	do i=size(a,1), 1, -1
		!Generate a random int from 1-i.
		call random_number(rand)
		hold=int(rand*i)+1
		!Swap the ith row with the rand row.
		temp=a(hold)
		a(hold)=a(i)
		a(i) = temp
	end do

end subroutine shuffle_dp_1

subroutine shuffle_int_dimn(a)
  use types, only : dp
  implicit none

	integer, dimension(:,:), intent(inout) :: a
	integer, dimension(size(a,2)) :: temp
	integer :: i, hold
	real(dp) :: rand
	
	!Count backwards.
	do i=size(a,1), 1, -1
		!Generate a random int from 1-i.
		call random_number(rand)
		hold=int(rand*i)+1
		!Swap the ith row with the rand row.
		temp(:)=a(hold,:)
		a(hold,:)=a(i,:)
		a(i,:) = temp(:)
	end do

end subroutine shuffle_int_dimn

!Subroutine that takes two sets, shuffles them together n times, then divides them along the columns, returning same sized arrays that are a mix of both sets.
subroutine shuffle_cut(setA, setB, n)
  use types, only : dp
  implicit none

	real(dp), dimension(:,:), intent(inout) :: setA, setB
	integer, optional, intent(in) :: n
	real(dp), dimension((size(setA,1)+size(setB,1)),size(setA,2)) :: work
	real(dp) ::  rand
	integer :: i, iend

	!Load the work function.
	do i=1,size(work,1)
		if (i .le. size(setA,1)) then
			work(i,:)=setA(i,:)
		else
			work(i,:)=setB(i-size(setA,1),:)
		end if
	end do

	!Shuffle the set.
	if (present(n)) then
		iend=n
	else
		iend=2
	end if
	do i=1,iend
		call shuffle(work)
	end do

	!Unload work.
	do i=1,size(work,1)
		if (i .le. size(setA,1)) then
			setA(i,:)=work(i,:)
		else
			setB(i-size(setA,1),:)=work(i,:)
		end if
	end do

end subroutine shuffle_cut


!Function to make a cluster of "n" points centered at "center" with given std "sigma."
function make_blob(n, center, sigma)
  use types, only : dp
  implicit none

	real(dp), dimension(:), intent(in) :: center
	real(dp), optional, intent(in) :: sigma
	real(dp):: std
	integer, intent(in) :: n
	real(dp), dimension(:,:), allocatable :: make_blob
	integer :: i,j

	!Optional argument for standard deviation.
	if (present(sigma)) then
		std = sigma
	else
		std = 1_dp
	end if

	call init_random_seed()

	allocate(make_blob(n,size(center)))

	do j=1,size(center)
		make_blob(:,j) = normal(n,center(j),std)
	end do

end function make_blob

!Subroutine that will return a random sample from a target distribution.  Uses
!the Metropolis algorithm with a random Gaussian walk proposal function.
!subroutine sample(func,table,n, std)
!  use types, only : dp
!  implicit none
!
!  interface
!    real(dp) function func(x)
!      use types, only : dp
!      implicit none
!      real(dp), intent(in) :: x
!    end function
!  end interface
!  integer, intent(in) :: n
!  real(dp), dimension(n), intent(out) :: table
!  real(dp), optional, intent(in) :: std
!  real(dp) :: sigma, y1, y2, a, rand, mean
!  integer :: i
!
!  if (.not. present(std)) then
!    sigma=1.0_dp
!  else
!    sigma=std
!  end if
!
!  !Get a random point.
!  mean = 0_dp
!  y1 = normal(1,mean, sigma)
!
!  do i=1,n
!    !Propose a new point.
!    y2 = normal(1,y1,sigma)
!
!    !Accept with probability a.
!    if ( (func(y2)/func(y1)) < 1_dp) then
!      a = (func(y2)/func(y1))
!    else
!      a = 1_dp
!    end if
!
!    call random_number(rand)
!    if (rand<a) then
!      !Accept
!      table(i)=y2
!      !Set old pt to new pt.
!      y1=y2
!    else
!      !Decline
!      table(i)=y1
!    end if
!    !Set old pt to new pt.
!    y1=y2
!  end do
!
!end subroutine sample


end module rng




































