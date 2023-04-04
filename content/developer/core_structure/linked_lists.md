---
title: Linked Lists

draft: false
toc: true
type: docs

menu:
  developer:
    parent: Core Structure
    weight: 50
---

In EPOCH, different processors are responsible for different cells, and each 
MPI rank only tracks the particles which exist with these cells. Hence, as
macro-particles move around the simulation, they must be removed from one
processor and added to another. If macro-particles were stored in arrays, these 
would continuously have to be resized as macro-particles moved around. Instead, 
EPOCH stores macro-particles in _linked lists_.

Linked lists are a standard computer programming technique which is still
slightly unusual in Fortran, and may well be unfamiliar to many Fortran
programmers. They effectively allow you to have an array of arbitrary length,
although this comes with various trade-offs about memory locality and speed of
accessing elements. The general concept is that of a chain where each link in
the chain only knows about the previous link in the chain and the next link in
the chain. Although there are schemes for doing this in languages which don't
have pointers, the normal method of implementing linked lists is to use
pointers to point to previous and next elements in the list, and this is how
they are implemented in EPOCH. Since both linked lists and Fortran pointers
are slightly esoteric concepts, while being key to the operation of EPOCH a
brief overview of them is presented here. 

The simplest possible form of a linked list element would be a TYPE which
looks like:
 ```perl
TYPE linked_list
  TYPE(linked_list), POINTER :: next
  TYPE(linked_list), POINTER :: prev
END TYPE linked_list
 ```

You also have to have a pointer to the start of the list, and to speed up
adding new elements to the list, you normally also keep a pointer to the
last element of the list. Therefore, you would also have variables which look
like:
 ```perl
TYPE(linked_list) :: head, tail
 ```

Since Fortran pointers are not initialised in any particular state, you have
to remember to set the head and tail pointers to explicitly point nowhere
(normally called a null pointer by analogy with the older C style
pointers). This is done using the nullify command.
 ```perl
NULLIFY(head)
NULLIFY(tail)
 ```

The same thing is important when creating a new linked list element, so you
would normally have a creation function for linked list elements.
 ```perl
SUBROUTINE create_element(element)

  TYPE(linked_list), POINTER :: element

  ALLOCATE(element)
  NULLIFY(element%next)
  NULLIFY(element%prev)

END SUBROUTINE create_element
 ```
Note that the allocate function can be used on pointers in the same way that
it can be used with variables which have the allocatable attribute. However,
there is one important difference between a pointer and an allocatable
variable. If you attempt to allocate an already allocated variable which has
the allocatable attribute then the code will fail, whereas allocating an
already allocated pointer is perfectly valid, and will allocate the new
variable and point the pointer to it. This does not deallocate the memory that
the pointer previously pointed to, and Fortran does not have a "garbage
collector" which deallocates memory no longer accessible. So if you
allocate a pointer which already points to a variable, it is very important
that you have another pointer somewhere which points to the same memory. Once
you no longer have a pointer to an area of memory, that area of memory is
completely inaccessible and cannot even be deallocated. This is termed a
memory leak and for programs which run for many cycles and have a memory leak
on each cycle, the entire memory can very quickly be used up.

So, to add a new element to the list you would have a subroutine which looks
like:
 ```perl
SUBROUTINE add_element(element)

  TYPE(linked_list), POINTER :: element

  IF (.NOT. ASSOCIATED(head)) THEN
    ! Adding first element to list, so just set
    ! both head and tail to the element
    head=>element
    tail=>element
    RETURN
  ENDIF

  tail%next=>element
  element%prev=>tail
  tail=>element

END SUBROUTINE add_element
 ```
This subroutine adds the new Fortran operator of `=>` which means "points
to". Unlike C or similar languages, Fortran pointers try to be partially
transparent to the end user, so the following code would fail:
 ```perl
PROGRAM test

  REAL, TARGET :: a = 10.0
  REAL, POINTER :: b

  b = a

END PROGRAM test
 ```

This happens because Fortran will try to copy the value of "a" into "b".
However, "b" is a pointer which hasn't been initialised, so the code will
crash when it tries to copy the data in (in theory, the code may not crash if
the uninitialised "b" pointer happens to point somewhere in memory which is
a valid target, but this is very unlikely). Note also that "a" has the
attribute "TARGET". The target attribute means that it is possible to point
a pointer to this variable. You can only point a pointer to a variable which
is either a pointer itself or has the target attribute. This is to try and
keep Fortran pointers "safer" than C style pointers. The correct code would
use `b=>a`, at which point "b" is set to point to "a" and
can then be used everywhere in place of "a".

So, to set up a linked list of n elements, you would use the following code:
 ```perl
TYPE(linked_list), POINTER :: new
NULLIFY(new)

DO i = 1,n
  CALL create_element(new)
  CALL add_element(new)
ENDDO
 ```

To then run through the elements of your newly created linked list, you would
use code like:
 ```perl
TYPE(linked_list), POINTER :: current

current=>head
DO WHILE(ASSOCIATED(current))
  ! Do stuff
  current=>current%next
ENDDO
 ```

This code snippet introduces one new function "ASSOCIATED", which tells you
whether a pointer is a null pointer or not (this is why it is so important to
nullify new pointers, because ASSOCIATED on its own doesn't check whether a
pointer is valid, just whether or not it is a null pointer). You can also use
ASSOCIATED to check whether a pointer points to a particular object or not, in
which case the syntax is `RESULT = ASSOCIATED(b, TARGET=a)`, which
returns true if "b" points to "a", or false if it doesn't, even if "b"
is a valid pointer pointing to something else. It also introduces the way in
which you must use linked lists in EPOCH. The execution flow is as follows
 
-  Point current to the current element to the start of the linked list
  (head).
-  Iterate while current points to a valid element.
-  Perform whatever actions you want on current.
-  Point current to the next element in the chain.
  
This leads to the slightly counter intuitive behaviour where even though the
loop only acts on the variable named "current", all of the elements in the
list are operated on. Although there are many tricks which can be performed
with linked lists, the only other aspect which needs to be explained is how
to delete elements. A subroutine to remove a single element from a linked list
would look like:
 ```perl
SUBROUTINE remove_element(element)

  TYPE(linked_list), POINTER :: element

  IF (ASSOCIATED(element%prev)) THEN
    ! Previous element exists
    element%prev%next=>element%next
  ELSE
    ! Previous element does not exist therefore element is the head. When
    ! element is removed the head is the element after the one being removed
    head=>element%next
  ENDIF

  IF (ASSOCIATED(element%next)) THEN
    ! next element exists
    element%next%prev=>element%prev
  ELSE
    ! next element does not exists therefore element is the tail. When element
    ! is removed the head is the element before the one being removed
    tail=>element%prev
  ENDIF

END SUBROUTINE remove_element
 ```

Once again, this code looks slightly counter-intuitive, but if you go through
step by step, it's fairly simple. In the following discussion the element
being removed is called "C", the element before "C" (if it exists) is
called "P" and the element after "C" (if it exists) is called "N"
 
-  Check whether `C`'s prev element exists, this means that
  `P` exists.
-  If `P` exists then the element to be removed isn't at the
  start of the chain. When `C` is removed, we need
  `P`\%next to point to `C`\%next. This leads to the odd
  looking element\%prev\%next=> element\%next syntax.
-  If `P` does not exist then `C` is at the the start
  of the chain. In order to not leave the chain orphaned when `C`
  is removed, we need head to point to `C`\%next.
-  Exactly the same logic applies for updating the element after
  `C`.
-  Check whether `C`'s next element exists, this means that
  `N` exists.
-  If `N` exists then the element to be removed isn't at the end
  of the chain. When `C` is removed, we need `N`\%prev
  to point to `C`\%prev.
-  If `P` does not exist then `C` is at the the start
  of the chain. In order to not leave the chain orphaned when `C`
  is removed, we need head to point to `C`\%next.
  

Therefore, code to remove some elements from a linked list would look like:
 ```perl
TYPE(linked_list), POINTER :: current, next

current=>head
DO WHILE(ASSOCIATED(current))
  next=>current%next
  IF (dealloc) THEN
    CALL remove_element(current)
    DEALLOCATE(current)
  ENDIF
  current=>next
ENDDO
 ```
Note that "current" must be deallocated explicitly even after it has been
removed from the linked list to prevent a memory leak. Note also that the
pointer to the "next" element is saved before "current" is deallocated.
This is not necessary but means that there is only one IF statement rather
than the two otherwise required.