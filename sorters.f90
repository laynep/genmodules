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
	module procedure int_heapsort
end interface heapsort

interface locate
	module procedure locate_dp
	module procedure locate_int
end interface locate




contains

!**********************************************************
!Heapsorts a table based on the first column only.

!Adapted from Numerical Recipes pg 231.

subroutine heapsort_d(table)
IMPLICIT NONE

	real(dp), DIMENSION(:,:), INTENT(INOUT) :: table
	INTEGER :: n, l, ir, i, j, i_1, i_2
	real(dp), DIMENSION(SIZE(table,2)) :: rra	!Row temporary placeholder.

	rra=0_dp
	n=SIZE(table,1)
	l = (n/2)+1	!Note the integer division.
	ir = n
do1:	DO		!Indefinite do.  Exited by return statement in IF.
		IF(l > 1) THEN
			l = l-1
			CALL vect_eq_tablerow_d(rra,l,table)	
		ELSE
			CALL vect_eq_tablerow_d(rra,ir,table)	
			CALL row_equal_d(ir,1,table)
			ir = ir -1
			IF(ir==1)THEN
				DO i_1=1,SIZE(table,2)
					table(1,i_1) = rra(i_1)
				END DO
				RETURN
			END IF
		END IF
		i = l
		j = l+l
do2:		DO WHILE(j <= ir)
			IF(j < ir) THEN
				IF(table(j,1) < table(j+1,1)) THEN
					j = j+1
				END IF
			END IF
			IF(rra(1) < table(j,1)) THEN
				CALL row_equal_d(i,j,table)
				i = j
				j =j+j
			ELSE
				j = ir + 1
			END IF
		END DO do2
		DO i_2=1,SIZE(table,2)
			table(i,i_2) = rra(i_2)
		END DO
	END DO do1
		

END SUBROUTINE heapsort_d


!This subroutine makes a vector (of rank equal to the number of columns in table) equal to the ith row in a table.
SUBROUTINE vect_eq_tablerow_d(vect,i,table)
IMPLICIT NONE

	real(dp), DIMENSION(:), INTENT(INOUT) :: vect
	real(dp), DIMENSION(:,:), INTENT(IN) :: table
	INTEGER, INTENT(IN) :: i
	INTEGER :: k

	DO k=1,SIZE(vect)
		vect(k) = table(i,k)
	END DO

END SUBROUTINE vect_eq_tablerow_d



!This subroutine changes the ith row of a table to equal the jth row.
SUBROUTINE row_equal_d(i,j,table)
IMPLICIT NONE

	real(dp), DIMENSION(:,:), INTENT(INOUT) :: table
	INTEGER, INTENT(IN) :: i,j
	INTEGER :: k


	DO k=1,SIZE(table,2)
		table(i,k) = table(j,k)
	END DO


END SUBROUTINE row_equal_d



!***********************************************************************************
!For INTEGERS

!Heapsorts an integer table based on the first column only.

!Adapted from Numerical Recipes pg 231.

SUBROUTINE int_heapsort(table)
IMPLICIT NONE

	INTEGER, DIMENSION(:,:), INTENT(INOUT) :: table
	INTEGER :: n, l, ir, i, j, i_1, i_2
	INTEGER, DIMENSION(SIZE(table,2)) :: rra	!Row temporary placeholder.
	
	n=SIZE(table,1)
	l = (n/2)+1	!Note the integer division.
	ir = n

do1:	DO		!Indefinite do.  Exited by return statement in IF.

		IF(l > 1) THEN
			l = l-1
			CALL int_vect_eq_tablerow(rra,l,table)	
		ELSE
			CALL int_vect_eq_tablerow(rra,ir,table)	
			CALL int_row_equal(ir,1,table)
			ir = ir -1
			IF(ir==1)THEN
				DO i_1=1,SIZE(table,2)
					table(1,i_1) = rra(i_1)
				END DO
				RETURN
			END IF
		END IF

		i = l
		j = l+l

do2:		DO WHILE(j <= ir)
			IF(j < ir) THEN
				IF(table(j,1) < table(j+1,1)) j = j+1
			END IF
			IF(rra(1) < table(j,1)) THEN
				CALL int_row_equal(i,j,table)
				i = j
				j =j+j
			ELSE
				j = ir + 1
			END IF
		END DO do2

		DO i_2=1,SIZE(table,2)
			table(i,i_2) = rra(i_2)
		END DO

	END DO do1
		

END SUBROUTINE int_heapsort


!This subroutine makes a vector (of rank equal to the number of columns in table) equal to the ith row in a table.

SUBROUTINE int_vect_eq_tablerow(vect,i,table)
IMPLICIT NONE

	INTEGER, DIMENSION(:), INTENT(INOUT) :: vect
	INTEGER, DIMENSION(:,:), INTENT(INOUT) :: table
	INTEGER, INTENT(IN) :: i
	INTEGER :: k

	DO k=1,SIZE(vect)
		vect(k) = table(i,k)
	END DO

END SUBROUTINE int_vect_eq_tablerow

!This subroutine changes the ith row of a table to equal the jth row.

SUBROUTINE int_row_equal(i,j,table)
IMPLICIT NONE

	INTEGER, DIMENSION(:,:), INTENT(INOUT) :: table
	INTEGER, INTENT(IN) :: i,j
	INTEGER :: k


	DO k=1,SIZE(table,2)
		table(i,k) = table(j,k)
	END DO


END SUBROUTINE int_row_equal


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE heapsort_order2(table)
IMPLICIT NONE

	INTEGER, DIMENSION(:,:), INTENT(INOUT) :: table
	INTEGER, DIMENSION(:,:), ALLOCATABLE :: block
	INTEGER :: counter, maxim, minim, nrows, ncols, nblock, low, countsum
	INTEGER :: i_1, i_2, i_3, i_4, i_5, i_6

	nrows = SIZE(table,1)
	ncols = SIZE(table,2)
	nblock = ncols - 1

	IF(ncols==1) THEN
		CALL int_heapsort(table)
		RETURN
	END IF

	IF(nrows==1) RETURN
	
	maxim = table(nrows,1)
	minim = table(1,1)

	low=1
	counter = 0
	countsum = 0

do1:	DO i_1=minim, maxim

		counter = 0

	do2:	DO i_2=low, nrows
			IF(table(i_2,1)==i_1)THEN
				counter = counter + 1
			ELSE IF(table(i_2,1) > i_1) THEN
				low = i_2
				EXIT do2
			END IF
		END DO do2

		IF(counter>1) THEN

			ALLOCATE(block(counter,nblock))

		do3:	DO i_3=1,counter
			do4:	DO i_4=1,nblock
					block(i_3,i_4) = table(countsum+i_3,i_4+1)
				END DO do4
			END DO do3

			CALL int_heapsort(block)

		do5:	DO i_5=1,counter
			do6:	DO i_6=1,nblock
					table(countsum+i_5,i_6+1) = block(i_5,i_6)
				END DO do6
			END DO do5

			DEALLOCATE(block)
		END IF

		countsum = countsum + counter

	END DO do1
		
		


END SUBROUTINE heapsort_order2

!*****************************************************

SUBROUTINE heapsorttotal(table)
IMPLICIT NONE

	INTEGER, DIMENSION(:,:), INTENT(INOUT) :: table
	INTEGER, DIMENSION(:,:), ALLOCATABLE :: block
	INTEGER :: nrows, ncols, counter, low, countsum, nblock
	INTEGER :: i_1,i_2,i_3,i_4,i_5,i_6
	LOGICAL :: same

	IF(nrows==1)RETURN

	nrows = SIZE(table,1)
	ncols = SIZE(table,2)

	!Assume it's sorted to order 1 already by int_heapsort.  Uncomment if not.
	CALL int_heapsort(table)
	IF(ncols==1) RETURN

	!Sort to order 2.	
	CALL heapsort_order2(table)
	IF(ncols<=2) RETURN

	DO i_1=2,ncols-1
		countsum = 0
		counter = 0
	do2:	DO i_2=1,nrows
			same = equalcond(i_1,i_2,(i_2+1),table)
			IF(same .EQV. .TRUE.) THEN
				counter = counter + 1
			ELSEIF(same .EQV. .FALSE.) THEN
				counter = counter + 1
				nblock = ncols - i_1		
				IF(counter==1) THEN
					countsum = countsum + counter
					counter = 0
					CYCLE do2
				END IF							
				ALLOCATE(block(counter,nblock))
				DO i_3=1,counter
					DO i_4=1,nblock
						block(i_3,i_4)=table(countsum+i_3,i_4+i_1)
					END DO
				END DO	

				CALL int_heapsort(block)
				
				DO i_5=1,counter
					DO i_6=1,nblock
						table(countsum+i_5,i_6+i_1)=block(i_5,i_6)
					END DO
				END DO
				
				DEALLOCATE(block)
				
				countsum = countsum + counter
				counter = 0
			END IF
		END DO do2
	END DO

END SUBROUTINE heapsorttotal

!Tells if the ith and jth row of a table are of equal value in the first numb of columns.

!NOTE: if j>size of table, then returns FALSE by default.

LOGICAL FUNCTION equalcond(numb,i,j,table)
IMPLICIT NONE

	INTEGER, INTENT(IN) :: numb,i,j
	INTEGER, DIMENSION(:,:), INTENT(IN) :: table
	INTEGER :: k

	IF(j>SIZE(table,1)) THEN
		equalcond = .FALSE.
		RETURN
	END IF

dok:	DO k=1,numb
		IF(table(i,k)==table(j,k))THEN
			equalcond = .TRUE.
		ELSE
			equalcond = .FALSE.
			EXIT dok
		END IF
	END DO dok
		
END FUNCTION equalcond

!*********************************************************
!SUBROUTINE which uses the Hunt algorithm to find a point in an ordered table by first supplying the search with an initial guess.  If the guess is good, then this speeds up the bisection algorithm above by a factor of log_2(N).

!This is taken from page 91 of Numerical Recipes.

!Given an array, table, of length N and width M, which has been ordered by the values in its first column either ascending or descending and given a reference value X, the subroutine will return the value jlo st X is between TABLE(JLO,:) and TABLE(JLO+1,:)

!JLO on input is the original guess for the position in the table.

SUBROUTINE hunt(table, x, jlo)
IMPLICIT NONE

	real(dp), DIMENSION(:,:), INTENT(IN) :: table
	real(dp), INTENT(IN) :: x
	INTEGER, INTENT(INOUT) :: jlo
	INTEGER :: n, jhi, inc, jm
	LOGICAL :: ascnd

	n=SIZE(table,1)

	ascnd = table(n,1)>table(1,1)
	IF(jlo .le. 0 .OR. jlo .gt. n) THEN
		jlo = 0
		jhi = n+1
		GO TO 3
	END IF

	inc = 1

	IF(x .ge. table(jlo,1) .EQV. ascnd) THEN
1		jhi = jlo + inc
		IF(jhi > n) THEN
			jhi = n+1
		ELSE IF(x.ge.table(jhi,1) .EQV. ascnd) THEN
			jlo=jhi
			inc = inc + inc
			GO TO 1
		END IF
	ELSE
		jhi = jlo
2		jlo = jhi - inc
		IF(jlo<1)THEN
			jlo =0
		ELSE IF (x<table(jlo,1) .EQV. ascnd) THEN
			jhi =jlo
			inc = inc+inc
			GO TO 2
		END IF
	END IF

3	IF(jhi-jlo.EQ.1)RETURN
	jm=(jhi+jlo)/2
	IF(x>table(jm,1) .EQV. ascnd) THEN
		jlo = jm
	ELSE
		jhi=jm
	END IF
	GO TO 3
	

END SUBROUTINE hunt

!*******************************************************
!SUBROUTINE which will search using bisection a table XX(n,m)  which has been previously ordered by its first column.  Given a value X it will return the value J such that X is between XX(J,1) and XX(J+1,1).  J=0 or J=SIZE(XX,1) if X is out of range.

!This function is taken almost verbatim from Numerical Recipes pg 90.

SUBROUTINE locate_dp(table,x,j)
IMPLICIT NONE

	real(dp), DIMENSION(:,:), INTENT(IN) :: table
	real(dp), INTENT(IN) :: x
	INTEGER :: jl, ju, n, jm
	INTEGER :: i
	INTEGER, INTENT(OUT) :: j

	n = SIZE(table,1)

	jl = 0
	ju = n+1
	DO WHILE (ju-jl>1)
		jm=(ju+jl)/2
		IF((table(n,1)> table(1,1)) .EQV. (x > table(jm,1))) THEN
			jl = jm
		ELSE
			ju=jm
		END IF
	END DO
	j = jl


END SUBROUTINE locate_dp

SUBROUTINE locate_int(table,x,j)
IMPLICIT NONE

	INTEGER, DIMENSION(:,:), INTENT(IN) :: table
	real(dp), INTENT(IN) :: x
	INTEGER :: jl, ju, n, jm
	INTEGER :: i
	INTEGER, INTENT(OUT) :: j

	n = SIZE(table,1)

	jl = 0
	ju = n+1
	DO WHILE (ju-jl>1)
		jm=(ju+jl)/2
		IF((table(n,1)> table(1,1)) .EQV. (x > table(jm,1))) THEN
			jl = jm
		ELSE
			ju=jm
		END IF
	END DO
	j = jl


END SUBROUTINE locate_int


END MODULE sorters

