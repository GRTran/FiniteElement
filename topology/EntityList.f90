module EntityList
  use VarPrecision
  use EntityPoint
  use EntityLine
  implicit none
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! Author: Greg Jones
  !! Date: 17-10-19
  !! Description: This module contains a class which holds all information about
  !!              a general entity.
  !! Dependencies: VariablePrecision
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  type :: entityListC
    type(entityPointC), allocatable :: points(:)
    type(entityLineC), allocatable :: lines(:)
  contains
    procedure, public, pass :: read => read_entity_list
    procedure, public, pass :: associate => associate_entities_to_properties
    procedure, public, pass :: print_all => print_entity_data
    generic  , public       :: point_info => single_point_info, array_point_info
    procedure, public, pass :: single_point_info
    procedure, public, pass :: array_point_info
  end type

contains

  subroutine read_entity_list( this, file_name, file_ref )
    class(entityListC), intent(out) :: this
    character(len=*)  , intent(in)  :: file_name
    integer           , intent(in)  :: file_ref

    character(len=50) :: temp_string
    integer :: num_dimen_entities(4)
    integer :: status
    logical :: exists

    exists = .FALSE.
    inquire(file=file_name, exist=exists)
    if (.not.exists) then
      write(*,*) 'Error ', file_name, ' not found.'
      stop
    endif
    open(unit=file_ref, file=file_name)
    do while ( trim(temp_string) /= '$Entities' )
      read(file_ref, *) temp_string
    enddo
    read(file_ref, *) num_dimen_entities

    ! allocate appropriate entity arrays
    allocate( this%points(num_dimen_entities(1)), this%lines(num_dimen_entities(2)), &
              stat=status)
    if ( status /= 0 ) stop 'Error allocating entity list...'

    call read_points( this, file_ref )
    call read_lines( this, file_ref )


  end subroutine

  subroutine associate_entities_to_properties( this, properties )
    class(entityListC) , intent(inout) :: this
    class(propertySetC), intent(inout) :: properties

    integer :: i

    do i = 1, size(this%points)
      call this%points(i)%associate_properties(properties)
    enddo
    do i = 1, size(this%lines)
      call this%lines(i)%associate_properties(properties)
      call this%lines(i)%associate_points(this%points)
    enddo

  end subroutine


  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - !
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - !
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - !
  !                         Data Printing Routines                            !
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - !
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - !
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - !
  subroutine print_entity_data( this )
    !! print out all topological information about the points, lines, surfaces
    !! and volumes in user readable manner.
    class(entityListC)        , intent(in) :: this

    integer :: i, j

    ! point data to write
    write(*,*) '- - - - - - - - - - - - - - - - - - - -'
    write(*,*) 'POINTS'
    write(*,*) '- - - - - - - - - - - - - - - - - - - -'

    do i = 1, size(this%points)
      write(*,'(a,i0)') 'Point number: ', i
      write(*,'(a,3f8.2)') 'Coordinates: ', this%points(i)%get_coordinates()
      write(*,'(a,i0)') 'Number of associated properties: ', this%points(i)%get_number_associated_properties()
      do j = 1, this%points(i)%get_number_associated_properties()
        write(*,'(a,i0,a,a,al1)') 'Associated Property ', j, ' name: ', trim(this%points(i)%get_property_name_id(j)), ' is boundary: ', this%points(i)%is_boundary(j)
      enddo
      write(*,*)
    enddo
  end subroutine

  subroutine single_point_info( this, index )
    class(entityListC), intent(in) :: this
    integer           , intent(in) :: index

    integer :: j

    if ( index > size(this%points) .or. index < 0 ) stop 'Error incorrect specified index when getting point info'

    write(*,*) '- - - - - - - - - - - - - - - - - - - -'
    write(*,*) 'POINT INFO'
    write(*,*) '- - - - - - - - - - - - - - - - - - - -'
    write(*,'(a,i0)') 'Point number: ', index
    write(*,'(a,3f8.2)') 'Coordinates: ', this%points(index)%get_coordinates()
    write(*,'(a,i0)') 'Number of associated properties: ', this%points(index)%get_number_associated_properties()
    do j = 1, this%points(index)%get_number_associated_properties()
      write(*,'(a,i0,a,a,al1)') 'Associated Property ', j, ' name: ', trim(this%points(index)%get_property_name_id(j)), ' is boundary: ', this%points(index)%is_boundary(j)
    enddo
    write(*,*)
  end subroutine

  subroutine array_point_info( this, indices )
    class(entityListC), intent(in) :: this
    integer           , intent(in) :: indices(:)

    integer :: i, j

    do i = 1, size(indices)
      if ( indices(i) > size(this%points) .or. indices(i) < 0 ) stop 'Error incorrect specified index when getting point info'
      write(*,'(a,i0)') 'Point number: ', indices(i)
      write(*,'(a,3f8.2)') 'Coordinates: ', this%points(indices(i))%get_coordinates()
      write(*,'(a,i0)') 'Number of associated properties: ', this%points(indices(i))%get_number_associated_properties()
      do j = 1, this%points(indices(i))%get_number_associated_properties()
        write(*,'(a,i0,a,a,al1)') 'Associated Property ', j, ' name: ', trim(this%points(indices(i))%get_property_name_id(j)), ' is boundary: ', this%points(indices(i))%is_boundary(j)
      enddo
      write(*,*)
    enddo
  end subroutine
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - !
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - !
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - !
  !                       Internal Module Routines                            !
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - !
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - !
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - !
  subroutine read_points( entity_list, file_ref )
    type(entityListC), intent(inout) :: entity_list
    integer          , intent(in)    :: file_ref

    real(wp) :: coords(3)
    integer, allocatable :: property_ids(:)
    integer :: num_physical_links
    integer :: status
    integer :: index
    integer :: i


    do i = 1, size(entity_list%points)
      read(file_ref,*) index, coords, num_physical_links
      allocate( property_ids(num_physical_links), stat=status )
      if ( status /= 0 ) stop 'Error allocating entity list...'
      backspace(file_ref)
      read(file_ref,*) index, coords, num_physical_links, property_ids
      call entity_list%points(i)%initialise(index, coords, num_physical_links, property_ids)
      deallocate( property_ids, stat=status )
      if ( status /= 0 ) stop 'Error deallocating entity list'
    enddo

  end subroutine

  subroutine read_lines( entity_list, file_ref )
    type(entityListC), intent(inout) :: entity_list
    integer          , intent(in)    :: file_ref

    real(wp) :: coords(6)
    integer, allocatable :: property_ids(:)
    integer, allocatable :: point_ids(:)
    integer :: num_physical_links
    integer :: num_bounding_points
    integer :: status
    integer :: i
    integer :: index

    do i = 1, size(entity_list%lines)
      read(file_ref,*) index, coords, num_physical_links
      allocate( property_ids(num_physical_links), stat=status )
      if ( status /= 0 ) stop 'Error allocating entity list...'
      backspace(file_ref)

      read(file_ref,*) index, coords, num_physical_links, property_ids, num_bounding_points
      allocate( point_ids(num_bounding_points), stat=status )
      if ( status /= 0 ) stop 'Error allocating entity list...'
      backspace(file_ref)

      read(file_ref,*) index, coords, num_physical_links, property_ids, num_bounding_points, point_ids

      call entity_list%lines(i)%initialise(index, coords, num_physical_links, property_ids, num_bounding_points, point_ids)
      deallocate( property_ids, stat=status )
      if ( status /= 0 ) stop 'Error deallocating entity list... '
      deallocate( point_ids, stat=status )
      if ( status /= 0 ) stop 'Error deallocating entity list... '
    enddo

  end subroutine

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - !
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - !
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - !
  !                       Unit Testing Subroutine                             !
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - !
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - !
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - !

  subroutine entity_1d_unit_test( )
    type(entityListC) :: entity_list
    type(propertySetC) :: property_list
    call entity_list%read('test_data/1d_fem_test.msh',145)
    call property_list%read('test_data/1d_fem_test.msh',145)
    call entity_list%associate(property_list)


  end subroutine

end module
