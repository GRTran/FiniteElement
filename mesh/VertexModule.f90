module VertexModule
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! Author: Greg Jones
  !! Date: 17-10-19
  !! Description: This module contains a class which creates a vertex as a point
  !!              within a mesh. It is the most low-level geometric construct.
  !!              A vertex has the ability to be both a boundary and non-boundary
  !! Dependencies: The VariablePrecision module
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  type vertexC
    real(wp) :: coordinates(3)
    !! the coordinates that geolocate the vertex within the domain
    real(wp), allocatable :: solution_vector(:)
    !! the actual output variables depend on the amount you want to solve for
    integer :: dof
    !! the number of degrees of freedom within the system
    real(wp), allocatable :: element_id(:)
    !! the element IDs that this vertex is associated with
  contains
    procedure, public, pass :: initialise_vector
  end type

contains

  subroutine initialise_vector( this, dof, coordinates )
    class(vertexC), intent(in) :: this
    integer       , intent(in) :: dof
    real(wp)      , intent(in) :: coordinates

    integer :: status

    this%dof = dof
    this%coordinates = coordinates

    allocate( this%solution_vector(dof), stat=status )
    if ( status /= 0 ) stop 'Error vertex: Cannot set max solution vectors, not enough RAM'
    this%solution_vector = 0._wp
  end subroutine

end module
