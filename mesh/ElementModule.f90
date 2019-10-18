module ElementModule
  use VariablePrecision
  use VertexModule
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! Author: Greg Jones
  !! Date: 17-10-19
  !! Description: This module contains a class which contains an abstract element
  !!              that is used as the superclass for elements that may be defined
  !!              which make up the mesh.
  !! Dependencies: VariablePrecision and VertexModule
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  type, abstract :: elementC
    type(vertexC), allocatable :: vertex(:)
    !! the vertices that are contained within the element
    type(topologicalC), allocatable :: solution_vector(:)
    !! the topological region that the element is contained within
    logical :: on_boundary
    integer :: dof
    !! the number of degrees of freedom within the system
    real(wp), allocatable :: element_id(:)
    !! the element IDs that this vertex is associated with
  contains
    procedure(initialise_element), deferred, public, pass :: abstract_initialise_element
  end type

  abstract interface
    subroutine initialise_element( this, ...)
      import elementC
      type(elementC), intent(out) :: this

    end subroutine
  end interface

contains

end module
