module PointElementModule
  use VarPrecision
  use ElementModule
  implicit none
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! Author: Greg Jones
  !! Date: 01-11-19
  !! Description: This module contains a class which contains an abstract element
  !!              that is used as the superclass for elements that may be defined
  !!              which make up the mesh.
  !! Dependencies: VariablePrecision and VertexModule
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  type, extends(elementC) :: pointElementC
    type(entityPointC), pointer :: point_entity
  contains
    procedure, public, pass :: initialise => initialise_point_element
  end type


contains

  subroutine initialise_point_element( this, element_id, element_type, entity_id, entity_list )
    type(pointElementC), intent(out) :: this

  end subroutine

end module
