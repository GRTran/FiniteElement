module EntityList
  use VariablePrecision
  use PhysicalProperties
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
    procedure, public, pass :: read_entity_list
    procedure, public, pass :: associate_entities_to_properties
  end type

contains

  subroutine read_entity_list( this, file_name, file_ref )
    class(entityListC), intent(out) :: this
    character(len=*)  , intent(in)  :: file_name
    integer           , intent(in)  :: file_ref

    character(len=50) :: temp_string
    integer :: num_dimen_entities(4)
    integer :: status

    open(file_ref, file_name)
    do while ( trim(temp_string) /= '$Entities' )
      read(file_ref, *) temp_string
    enddo
    read(file_ref, *) num_dimen_entities

    ! allocate appropriate entity arrays
    allocate( this%points(num_dimen_entities(1)), this%lines(num_dimen_entities(2)), &
              stat=status)
    if ( status /= 0 ) stop 'Error allocating entity list, go buy more RAM!'

    call read_points( this, file_ref )
    call read_lines( this, file_ref )


  end subroutine

  subroutine associate_entities_to_properties( this, properties )
    class(entityListC) , intent(inout) :: this
    class(propertySetC), intent(inout) :: properties

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
  !                       Internal Module Routines                            !
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - !
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - !
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - !
  subroutine read_points( entity_list, file_ref )
    type(entityListC), intent(inout) :: entity_list
    integer          , intent(in)    :: file_ref

    real(wp) :: coords(3)

    do i = 1, size(entity_list%points)
      int_ents = 0
      read(file_ref,*) coords, num_physical_links
      allocate( property_ids(num_physical_links), stat=status )
      if ( status /= 0 ) stop 'Error allocating entity list, go buy more RAM!'
      backspace(file_ref)
      read(file_ref,*) coords, num_physical_links, property_ids
      call entity_list%points(i)%initialise(coords, num_physical_links, property_ids)
      deallocate( property_ids, stat=status )
      if ( status /= 0 ) stop 'Error allocating entity list, go buy more RAM!'
    enddo

  end subroutine

end module
