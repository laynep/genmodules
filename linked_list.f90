!*********************************************
!Layne Price, University of Auckland, 5/9/2012
!*********************************************

!This is a module that has everything necessary to create a linked list.  Each node in the list is represented by some data --- in this case an allocatable array --- with one pointer pointing down the list.  

!The list is started by a node-pointer, called the "head," and ended by a node-pointer, called the "tail."  "Head" points at the first element in the list, itself a node-pointer that points to a real node.  The real nodes have a pointer going down the list.  These point to the next node-pointer in the chain.  This is shown below:

!         node1 ===========V       node2 ============V    node-end
!          ^                       ^                       ^
! HEAD => NODE1-POINT /   NODE2-POINT / ... /     NODE-END-POINT <= TAIL

!It is necessary that the naiive elements of the chain be node-pointers --- as opposed to nodes themselves --- so that we can ALLOCATE(node-pointer).  Doing this creates an unnamed area in memory that the node-pointer implicitly points to.  For example, let p=>unnamed and q=2 be an integer.  Setting p=q changes the unnamed spot in memory to unnamed=2.  We can refer to this spot in memory by referencing the node-pointer.  In this way, we are able to dynamically create spots in the memory which we can then add to the list.            


module linked_list
implicit none

	!This is a node in the link.
	type :: llnode
		!This point to the next nodes. Initializes to null.
		type(llnode), pointer :: next 
		!This is the data.
		double precision, dimension(:), allocatable :: a
	end type llnode 


contains

	!Initialize the list.  Remember that "head" and "tail" are bookend pointers for 
	!the list, which point to the first and last nodes, respectively.
	subroutine ll_init(head,tail)
	implicit none

		type(llnode), pointer, intent(inout) :: head, tail

		!Makes an empty list, i.e. the node is null().
		head => null()
		tail => null()

	end subroutine ll_init

	!Subroutine which returns the nth node in list, where head is counted as the zeroth
	!node.  Can optionally return the previous and next node, too.  If n=1, then 
	!previous node is head and if n=n_end, then next node is null() for both next and
	!prev.  If list is empty, then previous is head, next is tail, and sel has null() 		!pointers.  If n is out-of-bounds, then returns tail for all pointers.
	!NOTE: this is much slower than the random access given by a simple array --- 
	!O(n) vs O(1)
	subroutine ll_nav(n,head,tail,sel,plus,minus)
	implicit none

		integer, intent(in) :: n
		type(llnode), pointer, intent(inout) :: head,tail
		type(llnode), pointer, intent(out) :: sel
		type(llnode), pointer, optional, intent(out) :: plus, minus
		type(llnode), pointer :: move
		integer :: i

		!Make space in memory for a dummary type(llnode).
		allocate(sel)
		if(present(plus)) allocate(plus)
		if(present(minus)) allocate(minus)

		!Check to see if n is too small.
		if (n==0) then
			if (present(plus)) plus=>head
			return
		else if (n<0) then
			return
		end if

		!Check if list is empty.
		if (.not. associated(head)) then !List is empty.
			sel%next=>null()
			if (present(minus)) minus=>head
			if (present(plus)) plus=>tail
		else
			!See if n=1.
			if (n==1) then
				sel=>head
				if(present(plus)) plus=>head%next
				return
			end if

			!Navigate down list.
			move => head
			do i=1, n
				if (i==1) then
					move => head
				else
					!Point to the next node in list.
					move => move%next
				end if
				!Check if we're at the end of list.
				if (.not. associated(move)) then !we're at the end.
					!Out of bounds.
					if(i==n) then
						if (present(minus)) minus=>tail
					end if
					return
				end if		
			end do
			!If haven't reached end, then load nodes.
			if (present(plus) ) then
				if (associated(move%next)) then
					!Setting obj=pointer sets obj=pointer's target.
					plus=>move%next
				end if
			end if
			sel=move	
		end if

	end subroutine ll_nav


	!Add a node to the end of the list.  Takes as input a pointer which points to the
	!node which we want to add.  
	subroutine ll_append(new, head, tail)
	implicit none

		type(llnode), pointer, intent(inout) :: head, tail
		type(llnode), pointer, intent(inout) :: new

		!Check if the list is empty.
		if (associated(head)) then !Not empty.
			tail%next => new	!Whichever node is tail, connect to new.
						!This should point to an unnamed llnode.
						!i.e. new=>unnamed node defined via allocate.
			new%next => null()	!
			tail => new	
					

		else
			head => new		!Attach new node to end of head, making it
						!the first node.
			tail => new		!Attach the tail, indicated new is the end.
			tail%next => null()	!No successor to node that new points to.
		end if

	end subroutine ll_append

	!Add a node to the end of the nth node in a list, making the new node the (n+1)st node
	subroutine ll_insert(n,new,head,tail)
	implicit none

		type(llnode), pointer, intent(inout) :: head, tail
		type(llnode), pointer, intent(inout) :: new
		type(llnode), pointer :: sel, plus
		integer, intent(in) :: n

		if(n==0) then
			call ll_nav(n,head,tail,sel,plus)
			head=>new
			new%next=>plus
			return
		end if
	
		!Navigate to nth node in list.
		call ll_nav(n,head,tail,sel,plus)

		!Check if n is more than numb nodes in list.
		if (.not. associated(sel%next)) then !n or less nodes in list.
			!Stick new on the end of list.
			call ll_append(new,head,tail)
			return
		else
			sel%next=>null()
			new%next=>plus
			sel%next=>new
			
		end if

	end subroutine


	!Print a list.
	subroutine ll_print(head)
	implicit none

		type(llnode), pointer, intent(in) :: head
		type(llnode), pointer :: move
		integer :: i

		!Check if list is empty.
		if (.not. associated(head)) then
			print*, "The list is empty."
		else
			move=>head
			do
				print*,(move%a(i),i=1,size(move%a))
				if (.not. associated(move%next)) then
					exit
				else
					move=>move%next
				end if
			end do
		end if

	end subroutine ll_print

	!Write a list to file, "fname".  Optional input are formt, and unit.
	subroutine ll_write(head,fname,frmt,unumb)
	implicit none

		type(llnode), pointer, intent(in) :: head
		character(len=*), intent(in) :: fname
		character(len=*), optional, intent(in) :: frmt
		integer, optional, intent(in) :: unumb
		type(llnode), pointer :: move
		integer :: i, numb

		!Open the file which we will write to.
		if (present(unumb)) then
			numb = unumb
		else
			numb = 31415927
		end if
		if (present(frmt)) then
			open(unit=numb,file=fname,status="old",form=frmt)
		else
			open(unit=numb,file=fname,status="old",form="unformatted")
		end if

		!Check if list is empty.
		if (.not. associated(head)) then
			print*, "The list is empty."
		else
			move=>head
			do
				write(unit=numb),(move%a(i),i=1,size(move%a))
				move=>move%next
				if (.not. associated(move)) exit
			end do
		end if
	
		!Close the file.
		close(unit=numb)

	end subroutine ll_write


	!Make a new node from an array.
	subroutine ll_make(node,array)
	implicit none

		type(llnode), pointer, intent(inout) :: node
		double precision, dimension(:), intent(in) :: array

		!Creates an unnamed node of the specified size.  Since this is unnamed, it
		!can only be referred to by a pointer and it does this implicitly whenever
		!the pointer is called.  This is the main point in creating
		!memory space dynamically for pointers!
		allocate(node)

		!Sets the array component equal to array.  Automatically allocates a.
		allocate(node%a(size(array)))
		node%a=array
		!Sets pointer components to point to null.
		node%next=>null()

	end subroutine ll_make


	!Change a list to an array that it is conformable with.  Assumes that all the 
	!vectors in the list are of the same dimension.
	subroutine ll_to_array(head, table)
	implicit none

		type(llnode), pointer, intent(inout) :: head
		double precision, dimension(:,:), allocatable, intent(out) :: table
		type(llnode), pointer :: move
		integer :: counter, i

		!Check if empty.
		if (.not. associated(head)) then
			return
		else
			!Count number of elements in array st can allocate array.
			!O(n)
			counter=1
			move=>head
			do
				if (.not. associated(move%next)) then
					exit
				else
					counter = counter + 1
					move=>move%next
				end if
			end do
			!Allocate table
			allocate(table(counter,size(head%a)))

			!Load the table with the list.
			move=>head
			do i=1, counter
				table(i,:)=move%a
				if (associated(move%next)) move=>move%next
			end do
		end if

	end subroutine ll_to_array


	!Delete the first element in a linked list.  Returns a pointer to the first node.
	subroutine ll_del_first(head, tail, first)
	implicit none

		type(llnode), pointer, intent(inout) :: head, tail
		type(llnode), pointer :: test
		type(llnode), pointer, intent(out) :: first

		!Check to see if list is empty.
		if (associated(head)) then !list not empty.
			!Check if more than 1 node.
			if (associated(head%next)) then !more than one node.
				first => head
				!Makes sure unnamed array deallocated in mem.
				head => head%next
			else
				first => head
				head => null()
				tail => null()
			end if
			
		else
			!List empty.
			first => null()
		end if

	end subroutine ll_del_first


	!Delete all elements in a list.  Leaves the list initialized.
	subroutine ll_del_all(head,tail)
	implicit none

		type(llnode), pointer, intent(inout) :: head, tail
		type(llnode), pointer :: next
		
		do
			!Check if list empty.
			if (.not. associated(head)) then
				exit
			else
				call ll_del_first(head,tail,next)
				nullify(next)
			end if			
		end do

	end subroutine


end module linked_list


























