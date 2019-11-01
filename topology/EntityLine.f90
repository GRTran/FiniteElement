module EntityLine
  use EntityPoint

  implicit none
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! Author: Greg Jones
  !! Date: 17-10-19
  !! Description: This module contains a class which holds all information about
  !!              a point entity in the topological space.
  !! Dependencies: VariablePrecision, EntityModule
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  type, extends(entityC) :: entityLineC
    real(wp) :: coordinate_start(3)
    real(wp) :: coordinate_end(3)
    type(entityPointC), allocatable :: points(:)
    integer :: number_points
    integer, allocatable :: point_ids(:)
  contains
    procedure, public, pass :: initialise => initialise_line_entity
    procedure, public, pass :: associate_points
    procedure, public, pass :: get_coordinates_all
    procedure, public, pass :: get_coordinate => line_get_coordinate
    procedure, public, pass :: get_number_associated_points
    procedure, public, pass :: get_associated_point_id
  end type

contains

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! Initialise a line, defining coordinates, property and point ids
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  subroutine initialise_line_entity( this, id, coordinates, number_properties, property_ids, number_points, point_ids )
    class(entityLineC), intent(out) :: this
    real(wp)          , intent(in)  :: coordinates(6)
    integer           , intent(in)  :: id
    integer           , intent(in)  :: number_properties
    integer           , intent(in)  :: property_ids(:)
    integer           , intent(in)  :: number_points
    integer           , intent(in)  :: point_ids(:)

    integer :: status

    this%coordinate_start = coordinates(1:3)
    this%coordinate_end = coordinates(4:6)
    this%id = id
    this%number_associated_properties = number_properties

    allocate( this%property_ids(this%number_associated_properties), stat=status )
    if ( status /= 0 ) stop 'Error allocating entity list, go buy more RAM!'
    this%property_ids = property_ids

    this%number_points = number_points
    allocate( this%point_ids(number_points), stat=status )
    if ( status /= 0 ) stop 'Error allocating entity list, go buy more RAM!'
    allocate( this%points(number_points), stat=status )
    if ( status /= 0 ) stop 'Error allocating entity list, go buy more RAM!'
    this%point_ids = abs(point_ids)

  end subroutine

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! Associate the lines points to bound them
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  subroutine associate_points( this, point_list )
    class(entityLineC), intent(inout) :: this
    type(entityPointC), intent(in)    :: point_list(:)

    logical, allocatable :: found(:)
    integer :: status
    integer :: i, j

    allocate( found(size(this%point_ids)), stat=status )
    if ( status /= 0 ) stop 'Error allocating entity list, go buy more RAM!'
    found = .FALSE.

    ! loop through the list of points and find the id that matches the point ids
    ! specified within the line definition and point to them. Do in order so that the
    ! directionality is considered.
    do i = 1, this%number_points
      do j = 1, size(point_list)
        if ( this%point_ids(i) == point_list(j)%get_id() ) then
          this%points(i) = point_list(j)
          found(i) = .TRUE.
        endif
      enddo
    enddo

    if ( .not.all(found .eqv..TRUE.) ) stop 'Error associating points to lines, cannot find all point ids'
    deallocate( found, stat=status )
    if ( status /= 0 ) write(*,*) 'Warning: Error in deallocating logical found array'
  end subroutine

  function get_coordinates_all( this, start_end ) result( coords )
    !! return the x,y,z of either the start or the end coordinates based on whether integer is 0 or 1
    class(entityLineC), intent(in) :: this
    integer           , intent(in) :: start_end
    real(wp)                       :: coords(3)

    if (start_end == 0) then
      coords = this%coordinate_start
    elseif (start_end == 1) then
      coords = this%coordinate_end
    else
      stop 'Error incorrect start end specification in line get_coordinates, expect 0/1'
    endif
  end function

  real(wp) function line_get_coordinate( this, start_end, dim )
    !! returns a single coordinate for either the start or the end of the line
    class(entityLineC), intent(in) :: this
    integer           , intent(in) :: start_end
    integer           , intent(in) :: dim

    if ( dim > 3 .or. dim < 1 ) stop 'Error incorrect dimenson specification in line get_coordinates, expect 1/2/3'

    if (start_end == 0) then
      line_get_coordinate = this%coordinate_start(dim)
    elseif (start_end == 1) then
      line_get_coordinate = this%coordinate_end(dim)
    else
      stop 'Error incorrect start end specification in line get_coordinates, expect 0/1'
    endif
  end function

  integer function get_number_associated_points( this )
    class(entityLineC), intent(in) :: this
    get_number_associated_points = this%number_points
  end function

  integer function get_associated_point_id( this, index )
    !! returns the associated point id
    class(entityLineC), intent(in) :: this
    integer           , intent(in) :: index
    get_associated_point_id = this%points(index)%get_id()
  end function

end module
