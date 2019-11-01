module ControlPointsModule
  use VarPrecision
  implicit none
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! Author: Greg Jones
  !! Date: 17-10-19
  !! Description: This module contains a class which creates a vertex as a point
  !!              within a mesh. It is the most low-level geometric construct.
  !!              A vertex has the ability to be both a boundary and non-boundary
  !! Dependencies: The VariablePrecision module
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  type controlPointC
    real(wp) :: coordinates(3)
    !! the coordinates that geolocate the vertex within the domain
    integer, allocatable :: solution_vector(:)
    !! the reference to an index of x within the domain
    integer :: dof
    !! the number of degrees of freedom within the system
    integer :: node_id
    !! the unique id of the node
    integer :: entity_tag
    !! the entity that the control point is held within
    real(wp), allocatable :: element_id(:)
    !! the element IDs that this vertex is associated with
  contains
    procedure, public, pass :: initialise => initialise_control_point
    procedure, public, pass :: associate => associate_elements
    procedure, public, pass :: get_coordinates_all => control_point_get_coordinates_all
    procedure, public, pass :: get_coordinate => control_point_get_coordinate
    procedure, public, pass :: get_id => get_control_point_id
    procedure, public, pass :: get_entity_tag
  end type

contains

  subroutine initialise_control_point( this, dof, entity_tag, node_id, coordinates )
    class(controlPointC), intent(out) :: this
    integer             , intent(in)  :: dof
    integer             , intent(in)  :: entity_tag
    integer             , intent(in)  :: node_id
    real(wp)            , intent(in)  :: coordinates(3)

    integer :: status

    this%dof = dof
    this%entity_tag = entity_tag
    this%coordinates = coordinates
    this%node_id = node_id

    allocate( this%solution_vector(dof), stat=status )
    if ( status /= 0 ) stop 'Error control_points: Cannot set max solution vectors, not enough RAM'
    this%solution_vector = 0._wp
  end subroutine

  subroutine associate_elements( this )
    class(controlPointC), intent(inout) :: this

  end subroutine

  function control_point_get_coordinates_all( this ) result( coordinates )
    class(controlPointC), intent(in) :: this
    real(wp)                         :: coordinates(3)

    coordinates = this%coordinates
  end function

  real(wp) function control_point_get_coordinate( this, index )
    class(controlPointC), intent(in) :: this
    integer             , intent(in) :: index

    control_point_get_coordinate = this%coordinates(index)
  end function

  integer function get_control_point_id( this )
    class(controlPointC), intent(in) :: this
    get_control_point_id = this%node_id
  end function

  integer function get_entity_tag( this )
    class(controlPointC), intent(in) :: this
    get_entity_tag = this%entity_tag
  end function

end module
