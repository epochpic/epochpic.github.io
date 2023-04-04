---
title: Partlist

draft: false
toc: true
type: docs

menu:
  developer:
    parent: Core Structure
    weight: 51
---

Collections of particles in EPOCH are represented by the `particle_list`
object. These objects abstract much of the operation of the linked lists,
including adding and removing particles and sending particles to other
processors. This page details the functions present in 
`src/housekeeping/partlist.F90`, which details how particle lists are formed. 

## create_empty_partlist

```perl
SUBROUTINE create_empty_partlist(partlist)
  
TYPE(particle_list), INTENT(INOUT) :: partlist
```
`create_empty_partlist` is a routine which takes a particle_list
object and sets it up so that it points to no particles at all. It should be
used on newly allocated particle_list objects and when a particle_list has
served its purpose. It DOES NOT destroy the particles linked in the list at
the point that it is called. If the user wishes to delete all the particles in a
particle_list then the routine `destroy_partlist` should be used
instead.
   
## create_unsafe_partlist

```perl
SUBROUTINE create_unsafe_partlist(partlist, a_particle, &
    n_elements)
  
TYPE(particle_list), INTENT(INOUT) :: partlist
TYPE(particle), POINTER :: a_particle
INTEGER(KIND=8), INTENT(IN) :: n_elements
```
`create_unsafe_partlist` is a routine which allows the creation of
a particle_list which represents a subset of another particle list. This subset
is defined as starting at the particle pointed to by `a_particle`
and extending for `n_elements` elements. The new particle_list is
then flagged as "unsafe" because if it is destroyed for any reason then it
will affect other particle lists. Many particle_list functions can only work
on safe particle lists.
   
  
## create_unsafe_partlist_by_tail

```perl
SUBROUTINE create_unsafe_partlist_by_tail(partlist, head, &
    tail)
  
TYPE(particle_list), INTENT(INOUT) :: partlist
TYPE(particle), POINTER :: head, tail
```
`create_unsafe_partlist_by_tail` is almost identical to
`create_unsafe_partlist`, but instead of specifying the first
particle and a number of elements, the user specifies the first and last
elements in the subset of the particle list. If the particle objects specified
for head and tail are not in the same partlist or tail actually comes before
head then the routine will fail in an undefined manner.
   

## create_allocated_partlist

```perl
SUBROUTINE create_allocated_partlist(partlist, n_elements)
  
TYPE(particle_list), INTENT(INOUT) :: partlist
INTEGER(KIND=8), INTENT(IN) :: n_elements
```
   
`create_allocated_partlist` is a helper routine to setup a new
particle_list and create `n_elements` new particle objects already
in place in the list.
   
  
You should always use this routine when creating large numbers of new particle
objects since there is no guarantee that the internal structure of the
particle_list objects will not change in the future. This routine will
be modified to reflect any changes in the underlying code.

## create_filled_partlist

```perl
SUBROUTINE create_filled_partlist(partlist, data_in, &
    n_elements)
  
TYPE(particle_list), INTENT(INOUT) :: partlist
REAL(num), DIMENSION(:), INTENT(IN) :: data_in
INTEGER(KIND=8), INTENT(IN) :: n_elements
```
  
`create_filled_partlist` is a helper routine to setup a new
particle_list and create `n_elements` new particle objects already
in place in the list. These new particle objects are then assigned properties
from the array `data_in` where the particle properties are contained
in packed form. The particle data is unpacked from the array using the
`unpack_particle` routine.
   
You should always use this routine, if possible, when copying particles out of
packed format since there is no guarantee that the internal structure of the
particle_list objects will not change in the future. This routine will
be modified to reflect any changes in the underlying code.

## test_partlist

```perl
FUNCTION test_partlist(partlist)
  
TYPE(particle_list), INTENT(INOUT) :: partlist
```
  
`test_partlist` is a routine which tests for various possible types
of error within a particle_list object. It has a number of possible return
codes for different errors, using negative values for errors so severe that
the main tests cannot be run, or with a bitmask for errors in the main tests.
The return codes are:
 
-  0 - No error, particle_list has passed all tests.
-  -1 - Either the head or tail of the particle_list object is NULL. This is
  a serious error and usually means that there is a serious error inside the
  particle_list routines.
  
The other error codes are returned as a bitmask and mean the following
 
-  1 - A particle_list marked as safe has a head element which is linked to
  a preceding particle object.
-  2 - A particle_list marked as safe has a tail element which is linked to
  a proceding particle object.
-  4 - The count property of a particle_list does not correspond to the
  actual number of objects linked between the head and tail objects. This error
  code on its own usually means that the count property has been modified
  improperly.
  
Note that this routine is only intended for debugging and is very slow. It
should never be used by the code in normal operation and all routines should be
written in such a way that it is impossible for a particle_list object to
become corrupted.

## destroy_partlist

```perl
SUBROUTINE destroy_partlist(partlist)
  
TYPE(particle_list), INTENT(INOUT) :: partlist
```

`destroy_partlist` is a helper routine to delete all the particles
attached to a particle_list and free up the memory that they use. It also
guarantees to leave the particle_list object itself in a blank state where new
particles can be added to it. It DOES NOT delete the particle_list object
itself, since it does not know whether or not the particle_list is dynamically
allocated. If using dynamically allocated particle_list objects then it is up
to the user to deallocate them AFTER the attached particles are destroyed using
`destroy_partlist`.
   
If a particle_list is deleted without deleting the attached particle objects,
either using this routine or explicitly by the user, then the particles will
become orphaned and sit around using memory until the code ends. If this
happens regularly then the code will quickly crash, usually with a SIG_SEGV
error.

## copt_partlist

```perl
SUBROUTINE copy_partlist(partlist1, partlist2)
  
TYPE(particle_list), INTENT(INOUT) :: partlist1, partlist2
```
  
`copy_partlist` is a routine which sets `partlist2` to
point to the same linked list of particles as `partlist1`. It does
not copy the particles, just sets the head and tail pointers of
`partlist1` to point to the same particle objects as
`partlist1`.

## append_partlist

```perl
SUBROUTINE append_partlist(head, tail)
  
TYPE(particle_list), INTENT(INOUT) :: head, tail
```

`append_partlist` is a routine which takes the particles
attached to the particle_list object `tail` and adds them to the
end of the linked list for particle_list `head`. The particle_list
`tail` is then set to be an empty particle_list.
   
  
This routine can only append one safe particle_list to another safe
particle_list.


## add_particle_to_partlist

```perl
SUBROUTINE add_particle_to_partlist(partlist, new_particle)
  
TYPE(particle_list), INTENT(INOUT) :: partlist
TYPE(particle), POINTER :: new_particle
```

`add_particle_to_partlist` adds a new
particle (`new_particle`) to the end of the linked list of
particles in the particle_list object `partlist`. It deals with
cases of empty particle_list objects automatically.
   
If you want to add a new particle to the end of a particle list you should
always use this routine.

## remove_particle_from_partlist

```perl
SUBROUTINE remove_particle_from_partlist(partlist, &
    a_particle)
  
TYPE(particle_list), INTENT(INOUT) :: partlist
TYPE(particle), POINTER :: a_particle
```
  
`remove_particle_from_partlist` removes the particle object
specified by `a_particle` from the particle_list object given by
`partlist`. Be very careful that `a_particle` is indeed
in the linked list pointed to by `partlist`, otherwise it is possible
for the particle_list object which really does contain `a_particle`
to be left with an invalid pointer as its head or tail element if
`a_particle` is either the head or tail element.
   
  
Although this routine does work with unsafe particle_list objects, you should
be very careful using it in this case as it can break the head or tail element
of the primary particle_list which the unsafe particle_list is a subset of.
As a general rule, you should only use this routine to remove particles from a
simple particle_list which is a singly referenced primary, safe particle_list.

## setup_partlists

```perl
SUBROUTINE setup_partlists()
  

```
  
`setup_partlists` is a routine which is called once when EPOCH
first starts. It sets the variable `nvars` which is the number of
REAL(num) values required to contain all the information about a
single particle object needed when a particle is transferred to another
processor. How the information is packed and unpacked from the particle object
into an array of REAL(num) values is controlled in the functions
`pack_particle` and `unpack_particle`.
   
  
If the particle type gains additional properties as the result of preprocessor
directives then there should be a line which increments `nvars` by
the correct number when that preprocessor directive is active. For example:
 ```perl
#ifdef PER_PARTICLE_CHARGE_MASS
  nvar = nvar+2
#endif
 ```

## pack_particle and unpack_particle

```perl
SUBROUTINE   pack_particle(array, a_particle)
SUBROUTINE unpack_particle(array, a_particle)
  
REAL(num), DIMENSION(:), INTENT(INOUT) :: array
TYPE(particle), POINTER :: a_particle
```
  
`pack_particle` and `unpack_particle` are subroutines
which are used to copy all the information about a particle
necessary for the particle to be transferred to another processor into a
temporary array before sending to another processor. If a new particle property
has been added to the particle then these routines must be modified to allow
the copying of the new data into the array. The parameter `array` is
a REAL(num) array of length `nvars` and is the array into which the
data either must be packed or from which it must be
unpacked. `a_particle` is the particle object which must either have
its data copied into the array or be
populated with data from the array. No restriction is placed on how the
data should be packed into the data array, but obviously
`pack_particle` and `unpack_particle` must be inverse
operations so that particles packed by one processor can be unpacked correctly
by another processor.
   
Since it is very unlikely that EPOCH will be run on anything other than a
homogeneous cluster, it is acceptable to use the Fortran `TRANSFER`
function to pack incompatible data types into the `array` array. Just
make sure that `nvars` is defined in `setup_partlists`
to be long enough to contain all the information. More documentation on the
`TRANSFER` function (which is rarely used and dangerous!) can be
found [here](http://www.macresearch.org/%advanced_fortran_90_callbacks_with_the_transfer_function).


## display_particle

```perl
SUBROUTINE display_particle(a_particle)
  
TYPE(particle), POINTER :: a_particle
```

Displays the key information about a particle given by the parameter
`a_particle`. Used by `compare_particles`.
   
## compare_particles

```perl
FUNCTION compare_particles(particle1, particle2)
  
TYPE(particle), POINTER :: particle1, particle2
LOGICAL :: compare_particles
```   
  
Compares all the properties of two particle objects and displays the
information if they don't match. Used internally by
`test_packed_particles`. If the particle object is extended then
this routine should also be modified to test for equivalence of the new
properties.
   
## test_packed_particles

```perl
FUNCTION test_packed_particles(partlist, array, &
    npart_in_data)
  
TYPE(particle_list), INTENT(IN) :: partlist
REAL(num), DIMENSION(:), INTENT(IN) :: array
INTEGER(KIND=8), INTENT(IN) :: npart_in_data
LOGICAL :: test_packed_particles
```
  
`test_packed_particles` is a routine which checks that a packed
array of particles can be successfully unpacked back into particle objects. The
parameters are:
 
-  `partlist` - The particle_list corresponding to the original
  unpacked particles.
-  `array` - The REAL(num) array containing the packed data.
-  `npart_in_data` - The number of particles which were packed
  into `array`.
  
The routine tests that the number of particles in the particle_list match the
number believed to be in the data array, that the length of the data array is
correct and then unpacks each particle in turn from the data array and uses the
`compare_particles` function to compare the particles with the
original versions in the particle_list. If any particles fail the comparison
then an error is output to stdout and the function returns a value of
`.FALSE.`. The error message includes the processor
rank on which the problem occurs but the routine does not specifically include
any MPI commands, so it is possible to call the routine on a subset of
processors.
   

## partlist_send

```perl
SUBROUTINE partlist_send(partlist, dest)
  
TYPE(particle_list), INTENT(INOUT) :: partlist
INTEGER, INTENT(IN) :: dest
```   

`partlist_send` is a routine for sending all the particles in the
particle_list object `partlist` to another processor. The
destination processor is identified by its rank which is given by the
`dest` parameter. The routine does not destroy the particle_list
object which is given to it.
  
`partlist_send` uses MPI blocking sends, so unless a matching
`partlist_recv` has been posted on `dest` then the
routine will deadlock. It would be fairly simple to write a non-blocking
version of `partlist_send`, but at present no need for such a
routine has been found.

## partlist_recv

```perl
SUBROUTINE partlist_recv(partlist, src)
  
TYPE(particle_list), INTENT(INOUT) :: partlist
INTEGER, INTENT(IN) :: src
```
 
`partlist_recv` is a routine for receiving particles sent by a call
to `partlist_send` and loading them into the particle_list object
`partlist`. The source processor is identified by its rank which is
given by the `src` parameter. The routine destroys the particle_list
which it is given and indeed will leave orphaned particles if it is not given
an empty particle_list to receive the data.
   
-  `partlist_recv` uses MPI blocking receives, so unless a
  matching `partlist_send` has been posted on `src` then
  the routine will deadlock. It would be fairly simple to write a non-blocking
  version of `partlist_recv`, but at present no need for such a
  routine has been found.

-  Although it is not possible to directly use `partlist_recv`
  to add new particles onto an existing particle_list, it is only two lines to
  do this. First call `partlist_recv` with a temporary
  particle_list to receive the data and then use `append_partlist`
  to attach the particles to the end of the already populated list.

## partlist_send_recv

```perl
SUBROUTINE partlist_send_recv(partlist_send, &
    partlist_recv, dest, src)
  
TYPE(particle_list), INTENT(INOUT) :: partlist_send
TYPE(particle_list), INTENT(INOUT) :: partlist_recv
INTEGER, INTENT(IN) :: dest, src
```

`partlist_sendrecv` is a routine equivalent to
`MPI_SENDRECV` in that it allows overlapping sends and receives to
be written in a single line rather than the end user having to split processors
into red/black ordered pairs for communication. It sends the particle data in
`partlist_send` to the processor with rank `dest` and
receives particle data sent by processor `src` and stores it in the
particle_list `partlist_recv`. The routine is destructive to both
sending and receiving particle_lists, and can lead to orphaned particles if a
filled particle_list is passed as `partlist_recv`. this is the
routine which is used in the particle boundary conditions.
 
-  `partlist_sendrecv` uses MPI blocking sendrecv commands, so
  should be used in matching pairs or the routine will deadlock.

-  Although it is not possible to directly use
  `partlist_sendrecv` to add new particles onto an existing
  particle_list, it is only two lines to do this. First call
  `partlist_sendrecv` with a temporary particle_list to receive the
  data and then use `append_partlist` to attach the particles to
  the end of the already populated list.
  

