module ControlPointListModule
  use ControlPointsModule
  implicit none

  type controlPointListC
    type(controlPointC), allocatable :: control_points(:)
  contains
    procedure, public, pass :: read => read_node_list
    procedure, public, pass :: print_all => control_points_print_all
  end type

contains

  subroutine read_node_list( this, file_name, file_ref, dof )
    !! opens a gmesh mesh file and reads in the node information. A set of control
    !! points are then created
    class(controlPointListC), intent(out) :: this
    character(len=*)        , intent(in)  :: file_name
    integer                 , intent(in)  :: file_ref
    integer                 , intent(in)  :: dof

    character(len=50) :: temp_string
    integer :: status
    logical :: exists
    integer :: num_entity_groups
    integer :: max_nodes, min_node_tag, max_node_tag
    integer :: i, j, node_count
    integer :: dim, entity_tag, parametric, num_nodes_in_block
    integer, allocatable :: node_ids(:)
    real(wp), allocatable :: coordinates(:,:)

    exists = .FALSE.
    inquire(file=file_name, exist=exists)
    if (.not.exists) then
      write(*,*) 'Error ', file_name, ' not found.'
      stop
    endif
    open(unit=file_ref, file=file_name)
    do while ( trim(temp_string) /= '$Nodes' )
      read(file_ref, *) temp_string
    enddo

    read(file_ref,*) num_entity_groups, max_nodes, min_node_tag, max_node_tag
    allocate( this%control_points(max_nodes), stat=status )
    if ( status /= 0 ) stop 'Error control point list: Cannot allocate control points, not enough RAM'

    node_count = 1

    do i = 1, num_entity_groups
      read(file_ref,*) dim, entity_tag, parametric, num_nodes_in_block
      allocate( node_ids(num_nodes_in_block), coordinates(3, num_nodes_in_block), stat=status )
      if ( status /= 0 ) stop 'Error control point list: Cannot allocate control points, not enough RAM'
      ! for each entity block read in all node ids within that block
      do j = 1, num_nodes_in_block
        read(file_ref,*) node_ids(j)
      enddo
      ! for each entity block read in all coordinates within that block
      do j = 1, num_nodes_in_block
        read(file_ref,*) coordinates(:,j)
      enddo
      ! initialise each node within the entity block
      do j = 1, num_nodes_in_block
        call this%control_points(node_count)%initialise(dof, entity_tag, node_ids(j), coordinates(:,j))
        node_count = node_count + 1
      enddo
      deallocate( node_ids, coordinates, stat=status )
      if ( status /= 0 ) stop 'Error control point list: Cannot deallocate control points.'
    enddo
    close(file_ref)
  end subroutine

  subroutine control_points_print_all( this )
    class(controlPointListC), intent(in) :: this

    integer :: i

    ! control point data to write
    write(*,*) '- - - - - - - - - - - - - - - - - - - -'
    write(*,'(a,i0)') 'CONTROL POINTS: ', size(this%control_points)
    write(*,*) '- - - - - - - - - - - - - - - - - - - -'

    do i = 1, size(this%control_points)
      write(*,'(a,i0)') 'Control point ID: ', this%control_points(i)%get_id()
      write(*,'(a,i0)') 'Entity tag: ', this%control_points(i)%get_entity_tag()
      write(*,'(a,3f8.2)') 'Coordinates: ', this%control_points(i)%get_coordinates_all()
      write(*,*) 
    enddo
  end subroutine


end module
