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
    procedure, public, pass :: associate => associate_point_properties
  end type

contains

  subroutine initialise_point_entity( this, coordinates, number_associated_properties, property_ids )
    class(entityPointC)    , intent(out) :: this
    real(wp)               , intent(in)  :: coordinates(3)
    integer                , intent(in)  :: number_associated_properties
    integer                , intent(in)  :: property_ids(:)

    this%coordinates = coordinates
    this%number_associated_properties = number_associated_properties

    allocate( this%property_ids(number_associated_properties), stat=status )
    if ( status /= 0 ) stop 'Error allocating entity list, go buy more RAM!'
    this%property_ids = property_ids

  end subroutine


end module
