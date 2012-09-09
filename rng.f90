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

contains

!Generate an array of n numbers sampled from a Gaussian normal distribution with mean and standard deviation given.
!Adapted from RosettaCode.org.
function normal(n,mean,std)
implicit none
 
  integer, intent(in) :: n
  integer :: i
  double precision :: normal(n), temp
  double precision, intent(in) :: mean, std
  double precision, parameter :: pi = 4.0*ATAN(1.0)

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
implicit none

	double precision, dimension(:,:), intent(inout) :: a
	double precision, dimension(size(a,2)) :: temp
	integer :: i, hold
	double precision :: rand
	
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
implicit none

	double precision, dimension(:), intent(inout) :: a
	double precision :: temp
	integer :: i, hold
	double precision :: rand
	
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
implicit none

	integer, dimension(:,:), intent(inout) :: a
	integer, dimension(size(a,2)) :: temp
	integer :: i, hold
	double precision :: rand
	
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




!Function to make a cluster of "n" points centered at "center" with given std "sigma."
function make_blob(n, center, sigma)
implicit none

	double precision, dimension(:), intent(in) :: center
	double precision, optional, intent(in) :: sigma
	double precision :: std
	integer, intent(in) :: n
	double precision, dimension(:,:), allocatable :: make_blob
	integer :: i,j

	!Optional argument for standard deviation.
	if (present(sigma)) then
		std = sigma
	else
		std = 1d0
	end if

	call init_random_seed()

	allocate(make_blob(n,size(center)))

	do j=1,size(center)
		make_blob(:,j) = normal(n,center(j),std)
	end do

end function make_blob

end module rng