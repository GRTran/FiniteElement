module MeshModule1D
  use LineElementModule
  implicit none
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! Author: Greg Jones
  !! Date: 01-11-19
  !! Description: This module contains the full mesh structure
  !! Dependencies: VariablePrecision and various dimension element modules
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  type :: mesh1DC
    type(lineElementC), allocatable :: line_elements(:)
  contains
    procedure, public, pass :: read => read_mesh_from_file
  end type


contains

  subroutine read_mesh_from_file( this, file_name, file_ref )
    class(mesh1DC)  , intent(out) :: this
    character(len=*), intent(in)  :: file_name
    integer         , intent(in)  :: file_ref

    character(len=50) :: temp_string
    integer :: status
    logical :: exists

    integer :: num_element_groups, num_elements, start_element, end_element

    exists = .FALSE.
    inquire(file=file_name, exist=exists)
    if (.not.exists) then
      write(*,*) 'Error ', file_name, ' not found.'
      stop
    endif
    open(unit=file_ref, file=file_name)
    do while ( trim(temp_string) /= '$Elements' )
      read(file_ref, *) temp_string
    enddo

    read(file_ref, *) num_element_groups, num_elements, start_element, end_element

    element_count = start_element
    ! cycle through all element groups and allocate them
    do i = 1, num_element_groups

      element_count = element_count + 1
    enddo

  end subroutine

end module
