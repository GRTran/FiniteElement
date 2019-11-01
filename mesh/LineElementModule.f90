module LineElementModule
  use VarPrecision
  use PointElementModule
  implicit none
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! Author: Greg Jones
  !! Date: 01-11-19
  !! Description: This module contains a class which contains an abstract element
  !!              that is used as the superclass for elements that may be defined
  !!              which make up the mesh.
  !! Dependencies: VariablePrecision and PointelementModule
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  type, extends(elementC) :: lineElementC
    type(entityLineC), pointer :: line_entity
    type(pointElementC), pointer :: point_elements(2)
  contains
    procedure, public, pass :: initialise => initialise_line_element
  end type


contains

  subroutine initialise_line_element( this, element_id, element_type, entity_id, entity_list )
    type(lineElementC), intent(out) :: this

  end subroutine

end module
