module Entity
  use VariablePrecision
  use PhysicalProperties
  implicit none
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! Author: Greg Jones
  !! Date: 17-10-19
  !! Description: This module contains a class which holds all information about
  !!              a general entity.
  !! Dependencies: VariablePrecision
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  type :: entityC
    integer :: id ! this is the unique id of the entity (ascending number format)
    integer :: number_associated_properties
    type(physPropertyC), pointer :: properties(:)
    integer, allocatable :: property_ids(:)
  contains
    procedure, public, pass :: associate_properties
    procedure, public, pass :: get_id
  end type

contains

  subroutine associate_properties( this, properties )
    class(entityC), intent(inout) :: this
    type(propertySetC) , intent(in), target  :: properties

    integer :: index ! obtain index of properties for linking

    ! loop through each of the specified properties within the entity section of gmesh
    ! obtain the array index of the property with matching id and set up pointer
    ! association
    do i = 1, this%number_associated_properties
      ! determine whether the property is a physical property of just indicating that the entity is a boundary
      index == properties%property_index(property_id(i))
      if ( index /= -1 ) then
        this%properties(i) => properties%properties(index)
      else
        stop 'Error initialising point entity, cannot find property id in database'
      endif
    enddo
  end subroutine

  integer function get_id(this)
    class(entityC), intent(in) :: this
    get_id = this%id
  end function




end module
