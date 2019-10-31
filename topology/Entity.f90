module Entity
  use VarPrecision
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
    type(propertyC), allocatable :: properties(:)
    integer, allocatable :: property_ids(:)
  contains
    procedure, public, pass :: associate_properties
    procedure, public, pass :: get_id
    procedure, public, pass :: get_property_name_id
    procedure, public, pass :: get_number_associated_properties
    procedure, public, pass :: is_boundary => entity_is_boundary
  end type

contains

  subroutine associate_properties( this, properties )
    class(entityC), intent(inout) :: this
    type(propertySetC) , intent(in), target  :: properties

    integer :: index ! obtain index of properties for linking
    integer :: i
    integer :: status

    allocate( this%properties(this%number_associated_properties), stat=status)
    if ( status /= 0 ) stop 'Error allocating entity properties, go buy more RAM!'
    ! loop through each of the specified properties within the entity section of gmesh
    ! obtain the array index of the property with matching id and set up pointer
    ! association
    do i = 1, this%number_associated_properties
      ! determine whether the property is a physical property of just indicating that the entity is a boundary
      index = properties%property_index(this%property_ids(i))
      if ( index /= -1 ) then
        this%properties(i) = properties%property(index)
      else
        stop 'Error initialising point entity, cannot find property id in database'
      endif
    enddo
  end subroutine

  integer function get_id(this)
    class(entityC), intent(in) :: this
    get_id = this%id
  end function

  character(len=50) function get_property_name_id( this, index )
    class(entityC), intent(in) :: this
    integer       , intent(in) :: index
    get_property_name_id = trim(this%properties(index)%get_property_name())
  end function

  integer function get_number_associated_properties( this )
    class(entityC), intent(in) :: this
    get_number_associated_properties = this%number_associated_properties
  end function

  logical function entity_is_boundary( this, index )
    class(entityC), intent(in) :: this
    integer       , intent(in) :: index
    entity_is_boundary = this%properties(index)%is_boundary()
  end function



end module
