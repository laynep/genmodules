!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Written by Layne Price, University of Auckland, May 2012
! Much of this is taken from Numerical Recipes, and where indicated elsewhere.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!UPDATE OF SORTERS FOR DOUBLE PRECISION.

!This is a module which contains a number of subroutines and functions used to sort an nXm array.  These subroutines are detailed here:

!Sorters utilizing heapsort (Faster)
!1.	vector_heapsort(ra)	SUBROUTINE: heapsorts a real vector ra.
!2.	heapsort(table)		SUBROUTINE: heapsorts a real table based on the value of 					the first column only.
!3.	vect_eq_tablerow(vect,i,table)	SUBROUTINE: makes a vector equal to the ith row in 					a table (same rank).
!4.	row_equal(i,j,table)	SUBROUTINE: changes the ith row equal to jth row in table.
!5.	int_heapsort(table)	SUBROUTINE: heapsorts an integer table on first column.
!6.	int_vect_eq_tablerow(vect,i,table) SUBROUTINE: same as 3 for integers.
!7.	int_row_equal(i,j,table) SUBROUTINE: same as 4 for integers.
!8.	heapsort_order2(table)	SUBROUTINE: heapsorts int table based on first two cols only
!9.*****heapsorttotal(table)	SUBROUTINE: heapsorts integer table ALL cols.
!10.	equalcond(numb,i,j,table) FUNCTION: .TRUE. if i & j row of table equal in first 				numb of columns NOTE: .FALSE. if j exceeds nrows in table.

!Sorters utilizng injection sort (Slow for large numbers).
!1.	real_array_sort(table)	SUBROUTINE: injection sorts real array by 1st colmn
!2.	real_row_switch(i_1,i_2,table) SUBROUTINE: switches i_1&i_2 cols in table
!3.	int_array_sort(table)	SUBROUTINE: 1. for integers.
!4.	int_row_switch(i_1,i_2,table) SUBROUTINe: 2. for integers

!Location finders.
!1.	locate(table,x,j)       SUBROUTINE searchs table finds value where X is between 				XX(J,1) and XX(J+1,1).
!2.	hunt(table,x,j)		SUBROUTINE finds point in ordered table by searching 					with initial guess.



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



module sorters
  use types, only : dp
  implicit none

  !Interface to subroutines.
  interface heapsort
  	module procedure heapsort_d
  	module procedure heapsorttotal
  end interface heapsort

  interface locate
  	module procedure locate_dp
  	module procedure locate_int
    module procedure locate_int_int
    module procedure locate_dp_vect
  	module procedure locate_int_vect
    module procedure locate_int_int_vect

  end interface locate

contains

!**********************************************************
!Heapsorts a table based on the first column only.

!Adapted from Numerical Recipes pg 231.

pure subroutine heapsort_d(table)
  implicit none

	real(dp), dimension(:,:), intent(inout) :: table
	integer :: n, l, ir, i, j, i_1, i_2
	real(dp), dimension(size(table,2)) :: rra	!row temporary placeholder.

	rra=0_dp
	n=size(table,1)
	l = (n/2)+1	!note the integer division.
	ir = n
do1:	do		!indefinite do.  exited by return statement in if.
		if(l > 1) then
			l = l-1
			call vect_eq_tablerow_d(rra,l,table)	
		else
			call vect_eq_tablerow_d(rra,ir,table)	
			call row_equal_d(ir,1,table)
			ir = ir -1
			if(ir==1)then
				do i_1=1,size(table,2)
					table(1,i_1) = rra(i_1)
				end do
				return
			end if
		end if
		i = l
		j = l+l
do2:		do while(j <= ir)
			if(j < ir) then
				if(table(j,1) < table(j+1,1)) then
					j = j+1
				end if
			end if
			if(rra(1) < table(j,1)) then
				call row_equal_d(i,j,table)
				i = j
				j =j+j
			else
				j = ir + 1
			end if
		end do do2
		do i_2=1,size(table,2)
			table(i,i_2) = rra(i_2)
		end do
	end do do1
		

end subroutine heapsort_d


!this subroutine makes a vector (of rank equal to the number of columns in table) equal to the ith row in a table.
pure subroutine vect_eq_tablerow_d(vect,i,table)
implicit none

	real(dp), dimension(:), intent(inout) :: vect
	real(dp), dimension(:,:), intent(in) :: table
	integer, intent(in) :: i
	integer :: k

	vect(:) = table(i,:)

end subroutine vect_eq_tablerow_d



!this subroutine changes the ith row of a table to equal the jth row.
pure subroutine row_equal_d(i,j,table)
implicit none

	real(dp), dimension(:,:), intent(inout) :: table
	integer, intent(in) :: i,j

	table(i,:) = table(j,:)

end subroutine row_equal_d



!***********************************************************************************
!for integers

!heapsorts an integer table based on the first column only.

!adapted from numerical recipes pg 231.

pure subroutine int_heapsort(table)
implicit none

	integer, dimension(:,:), intent(inout) :: table
	integer :: n, l, ir, i, j, i_1, i_2
	integer, dimension(size(table,2)) :: rra	!row temporary placeholder.
	
	n=size(table,1)
	l = (n/2)+1	!note the integer division.
	ir = n

do1:	do		!indefinite do.  exited by return statement in if.

		if(l > 1) then
			l = l-1
			call int_vect_eq_tablerow(rra,l,table)	
		else
			call int_vect_eq_tablerow(rra,ir,table)	
			call int_row_equal(ir,1,table)
			ir = ir -1
			if(ir==1)then
				do i_1=1,size(table,2)
					table(1,i_1) = rra(i_1)
				end do
				return
			end if
		end if

		i = l
		j = l+l

do2:		do while(j <= ir)
			if(j < ir) then
				if(table(j,1) < table(j+1,1)) j = j+1
			end if
			if(rra(1) < table(j,1)) then
				call int_row_equal(i,j,table)
				i = j
				j =j+j
			else
				j = ir + 1
			end if
		end do do2

		do i_2=1,size(table,2)
			table(i,i_2) = rra(i_2)
		end do

	end do do1
		

end subroutine int_heapsort


!this subroutine makes a vector (of rank equal to the number of columns in table) equal to the ith row in a table.

pure subroutine int_vect_eq_tablerow(vect,i,table)
implicit none

	integer, dimension(:), intent(inout) :: vect
	integer, dimension(:,:), intent(inout) :: table
	integer, intent(in) :: i

  vect(:) = table(i,:)

end subroutine int_vect_eq_tablerow

!this subroutine changes the ith row of a table to equal the jth row.

pure subroutine int_row_equal(i,j,table)
implicit none

	integer, dimension(:,:), intent(inout) :: table
	integer, intent(in) :: i,j

	table(i,:) = table(j,:)


end subroutine int_row_equal


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

pure subroutine heapsort_order2(table)
implicit none

	integer, dimension(:,:), intent(inout) :: table
	integer, dimension(:,:), allocatable :: block
	integer :: counter, maxim, minim, nrows, ncols, nblock, low, countsum
	integer :: i_1, i_2, i_3, i_4, i_5, i_6

	nrows = size(table,1)
	ncols = size(table,2)
	nblock = ncols - 1

	if(ncols==1) then
		call int_heapsort(table)
		return
	end if

	if(nrows==1) return
	
	maxim = table(nrows,1)
	minim = table(1,1)

	low=1
	counter = 0
	countsum = 0

do1:	do i_1=minim, maxim

		counter = 0

	do2:	do i_2=low, nrows
			if(table(i_2,1)==i_1)then
				counter = counter + 1
			else if(table(i_2,1) > i_1) then
				low = i_2
				exit do2
			end if
		end do do2

		if(counter>1) then

			allocate(block(counter,nblock))

		do3:	do i_3=1,counter
			do4:	do i_4=1,nblock
					block(i_3,i_4) = table(countsum+i_3,i_4+1)
				end do do4
			end do do3

			call int_heapsort(block)

		do5:	do i_5=1,counter
			do6:	do i_6=1,nblock
					table(countsum+i_5,i_6+1) = block(i_5,i_6)
				end do do6
			end do do5

			deallocate(block)
		end if

		countsum = countsum + counter

	end do do1

end subroutine heapsort_order2

!*****************************************************

pure subroutine heapsorttotal(table)
implicit none

	integer, dimension(:,:), intent(inout) :: table
	integer, dimension(:,:), allocatable :: block
	integer :: nrows, ncols, counter, low, countsum, nblock
	integer :: i_1,i_2,i_3,i_4,i_5,i_6
	logical :: same

	if(nrows==1)return

	nrows = size(table,1)
	ncols = size(table,2)

	!assume it's sorted to order 1 already by int_heapsort.  uncomment if not.
	call int_heapsort(table)
	if(ncols==1) return

	!sort to order 2.	
	call heapsort_order2(table)
	if(ncols<=2) return

	do i_1=2,ncols-1
		countsum = 0
		counter = 0
	do2:	do i_2=1,nrows
			same = equalcond(i_1,i_2,(i_2+1),table)
			if(same .eqv. .true.) then
				counter = counter + 1
			elseif(same .eqv. .false.) then
				counter = counter + 1
				nblock = ncols - i_1		
				if(counter==1) then
					countsum = countsum + counter
					counter = 0
					cycle do2
				end if							
				allocate(block(counter,nblock))
				do i_3=1,counter
					do i_4=1,nblock
						block(i_3,i_4)=table(countsum+i_3,i_4+i_1)
					end do
				end do	

				call int_heapsort(block)
				
				do i_5=1,counter
					do i_6=1,nblock
						table(countsum+i_5,i_6+i_1)=block(i_5,i_6)
					end do
				end do
				
				deallocate(block)
				
				countsum = countsum + counter
				counter = 0
			end if
		end do do2
	end do

end subroutine heapsorttotal

!tells if the ith and jth row of a table are of equal value in the first numb of columns.

!note: if j>size of table, then returns false by default.

pure logical function equalcond(numb,i,j,table)
implicit none

	integer, intent(in) :: numb,i,j
	integer, dimension(:,:), intent(in) :: table
	integer :: k

	if(j>size(table,1)) then
		equalcond = .false.
		return
	end if

dok:	do k=1,numb
		if(table(i,k)==table(j,k))then
			equalcond = .true.
		else
			equalcond = .false.
			exit dok
		end if
	end do dok
		
end function equalcond

!*********************************************************
!subroutine which uses the hunt algorithm to find a point in an ordered table by first supplying the search with an initial guess.  if the guess is good, then this speeds up the bisection algorithm above by a factor of log_2(n).

!this is taken from page 91 of numerical recipes.

!given an array, table, of length n and width m, which has been ordered by the values in its first column either ascending or descending and given a reference value x, the subroutine will return the value jlo st x is between table(jlo,:) and table(jlo+1,:)

!jlo on input is the original guess for the position in the table.

pure subroutine hunt(table, x, jlo)
implicit none

	real(dp), dimension(:,:), intent(in) :: table
	real(dp), intent(in) :: x
	integer, intent(inout) :: jlo
	integer :: n, jhi, inc, jm
	logical :: ascnd

	n=size(table,1)

	ascnd = table(n,1)>table(1,1)
	if(jlo .le. 0 .or. jlo .gt. n) then
		jlo = 0
		jhi = n+1
		go to 3
	end if

	inc = 1

	if(x .ge. table(jlo,1) .eqv. ascnd) then
1		jhi = jlo + inc
		if(jhi > n) then
			jhi = n+1
		else if(x.ge.table(jhi,1) .eqv. ascnd) then
			jlo=jhi
			inc = inc + inc
			go to 1
		end if
	else
		jhi = jlo
2		jlo = jhi - inc
		if(jlo<1)then
			jlo =0
		else if (x<table(jlo,1) .eqv. ascnd) then
			jhi =jlo
			inc = inc+inc
			go to 2
		end if
	end if

3	if(jhi-jlo.eq.1)return
	jm=(jhi+jlo)/2
	if(x>table(jm,1) .eqv. ascnd) then
		jlo = jm
	else
		jhi=jm
	end if
	go to 3
	

end subroutine hunt

!*******************************************************
!subroutine which will search using bisection a table xx(n,m)  which has been previously ordered by its first column.  given a value x it will return the value j such that x is between xx(j,1) and xx(j+1,1).  j=0 or j=size(xx,1) if x is out of range.

!this function is taken almost verbatim from numerical recipes pg 90.

pure subroutine locate_dp(table,x,j)
implicit none

	real(dp), dimension(:,:), intent(in) :: table
	real(dp), intent(in) :: x
	integer :: jl, ju, n, jm
	integer :: i
	integer, intent(out) :: j

	n = size(table,1)

	jl = 0
	ju = n+1
	do while (ju-jl>1)
		jm=(ju+jl)/2
		if((table(n,1)> table(1,1)) .eqv. (x > table(jm,1))) then
			jl = jm
		else
			ju=jm
		end if
	end do
	j = jl

end subroutine locate_dp

pure subroutine locate_dp_vect(table,x,j)
implicit none

	real(dp), dimension(:), intent(in) :: table
	real(dp), intent(in) :: x
	integer :: jl, ju, n, jm
	integer :: i
	integer, intent(out) :: j

	n = size(table)

	jl = 0
	ju = n+1
	do while (ju-jl>1)
		jm=(ju+jl)/2
		if((table(n)> table(1)) .eqv. (x > table(jm))) then
			jl = jm
		else
			ju=jm
		end if
	end do
	j = jl


end subroutine locate_dp_vect


pure subroutine locate_int(table,x,j)
implicit none

	integer, dimension(:,:), intent(in) :: table
	real(dp), intent(in) :: x
	integer :: jl, ju, n, jm
	integer :: i
	integer, intent(out) :: j

	n = size(table,1)

	jl = 0
	ju = n+1
	do while (ju-jl>1)
		jm=(ju+jl)/2
		if((table(n,1) .ge. table(1,1)) .eqv. (x > table(jm,1))) then
			jl = jm
		else
			ju=jm
		end if
	end do
	j = jl


end subroutine locate_int

pure subroutine locate_int_vect(table,x,j)
implicit none

	integer, dimension(:), intent(in) :: table
	real(dp), intent(in) :: x
	integer :: jl, ju, n, jm
	integer :: i
	integer, intent(out) :: j

	n = size(table)

	jl = 0
	ju = n+1
	do while (ju-jl>1)
		jm=(ju+jl)/2
		if((table(n) .ge. table(1)) .eqv. (x > table(jm))) then
			jl = jm
		else
			ju=jm
		end if
	end do
	j = jl


end subroutine locate_int_vect


pure subroutine locate_int_int(table,x,j)
implicit none

	integer, dimension(:,:), intent(in) :: table
	integer, intent(in) :: x
	integer :: jl, ju, n, jm
	integer :: i
	integer, intent(out) :: j

	n = size(table,1)

	jl = 0
	ju = n+1
	do while (ju-jl>1)
		jm=(ju+jl)/2
		if((table(n,1) .ge. table(1,1)) .eqv. (x > table(jm,1))) then
			jl = jm
		else
			ju=jm
		end if
	end do
	j = jl


end subroutine locate_int_int

pure subroutine locate_int_int_vect(table,x,j)
implicit none

	integer, dimension(:), intent(in) :: table
	integer, intent(in) :: x
	integer :: jl, ju, n, jm
	integer :: i
	integer, intent(out) :: j

	n = size(table)

	jl = 0
	ju = n+1
	do while (ju-jl>1)
		jm=(ju+jl)/2
		if((table(n) .ge. table(1)) .eqv. (x > table(jm))) then
			jl = jm
		else
			ju=jm
		end if
	end do
	j = jl


end subroutine locate_int_int_vect


end module sorters

