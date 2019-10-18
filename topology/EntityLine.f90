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
    class(entityPointC), pointer :: points(:)
    integer :: number_points
    integer, allocatable :: point_ids(:)
  contains
    procedure, public, pass :: initialise => initialise_line_entity
    procedure, public, pass :: associate_points
  end type

contains

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! Initialise a line, defining coordinates, property and point ids
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  subroutine initialise_line_entity( this, coordinates, number_properties, property_ids, number_points, point_ids )
    class(entityLineC), intent(out) :: this
    real(wp)          , intent(in)  :: coordinates(6)
    integer           , intent(in)  :: number_properties
    integer           , intent(in)  :: property_ids(:)
    integer           , intent(in)  :: number_points
    integer           , intent(in)  :: point_ids(:)

    integer :: status

    this%coordinate_start = coordinates(1:3)
    this%coordinate_end = coordinates(4:6)

    this%number_associated_properties = number_properties

    allocate( this%property_ids(number_associated_properties), stat=status )
    if ( status /= 0 ) stop 'Error allocating entity list, go buy more RAM!'
    this%property_ids = property_ids

    this%number_points = number_points
    allocate( this%point_ids(number_points), stat=status )
    if ( status /= 0 ) stop 'Error allocating entity list, go buy more RAM!'
    this%point_ids = point_ids

  end subroutine

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! Associate the lines points to bound them
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  subroutine associate_points( this, point_list )
    class(entityLineC), intent(inout) :: this
    type(entityPointC), intent(in), target :: point_list(:)

    logical, allocatable :: found(:)
    integer :: status

    allocate( found(size(point_list)), stat=status )
    if ( status /= 0 ) stop 'Error allocating entity list, go buy more RAM!'

    ! loop through the list of points and find the id that matches the point ids
    ! specified within the line definition and point to them. Do in order so that the
    ! directionality is considered.
    do i = 1, this%number_points
      do j = 1, size(point_list)
        if ( this%point_ids(i) == point_list(j)%get_id() ) then
          this%points(i) => point_list(j)
          found(i) = .TRUE.
        endif
      enddo
    enddo

    if ( .not.all(found ==.TRUE.) ) stop 'Error associating points to lines, cannot find all point ids'
    deallocate( found, stat=status )
    if ( status /= 0 ) write(*,*) 'Warning: Error in deallocating logical found array'
  end subroutine


end module
