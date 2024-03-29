module EntityPoint
  use Entity
  implicit none
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! Author: Greg Jones
  !! Date: 17-10-19
  !! Description: This module contains a class which holds all information about
  !!              a point entity in the topological space.
  !! Dependencies: VariablePrecision, EntityModule
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  type, extends(entityC) :: entityPointC
    real(wp) :: coordinates(3)
  contains
    procedure, public, pass :: initialise => initialise_point_entity
    procedure, public, pass :: get_coordinates
    procedure, public, pass :: get_coordinate => point_get_coordinate
  end type

contains

  subroutine initialise_point_entity( this, id, coordinates, number_associated_properties, property_ids )
    class(entityPointC)    , intent(out) :: this
    real(wp)               , intent(in)  :: coordinates(3)
    integer                , intent(in)  :: id
    integer                , intent(in)  :: number_associated_properties
    integer                , intent(in)  :: property_ids(:)

    integer :: status

    this%id = id
    this%coordinates = coordinates
    this%number_associated_properties = number_associated_properties

    allocate( this%property_ids(number_associated_properties), stat=status )
    if ( status /= 0 ) stop 'Error allocating entity list, go buy more RAM!'
    this%property_ids = property_ids

  end subroutine

  function get_coordinates( this ) result( coords )
    class(entityPointC), intent(in) :: this
    real(wp)                        :: coords(3)
    coords = this%coordinates
  end function

  real(wp) function point_get_coordinate( this, dim )
    !! returns a single coordinate in the dimension that is specified
    class(entityPointC), intent(in) :: this
    integer           , intent(in) :: dim

    if ( dim > 3 .or. dim < 1 ) stop 'Error incorrect dimenson specification in line get_coordinates, expect 1/2/3'
    point_get_coordinate = this%coordinates(dim)
  end function


end module
