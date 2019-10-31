module PhysicalProperties
  use VarPrecision
  implicit none
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! Author: Greg Jones
  !! Date: 17-10-19
  !! Description: This module contains a class which holds all information about
  !!              the physical properties of the system which has been read in.
  !!              The physical properties of the system are distinguished from
  !!              boundary conditions using a "p_" is the gmesh specification.
  !! Dependencies: VariablePrecision
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  type propertyC
    integer :: id ! this is the id of the physical property
    character(len=50) :: name_id ! this is the name of the physical property
    integer :: entity_type
    logical :: isboundary

  contains
    procedure, public, pass :: property_id
    procedure, public, pass :: is_boundary => property_is_boundary
    procedure, public, pass :: get_property_name
  end type

  type :: propertySetC
    type(propertyC), allocatable :: property(:)
  contains
    procedure, public, pass :: read => read_property_list
    procedure, public, pass :: property_index
  end type

contains

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - !
  !                     Physical Set Property Functions                       !
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - !

  subroutine read_property_list( this, file_name, file_ref )
    !! read in the property list from a gmesh file and construct an array of
    !! property objects.
    class(propertySetC), intent(out) :: this
    character(len=*)   , intent(in)  :: file_name
    integer            , intent(in)  :: file_ref

    integer :: status
    integer :: number_properties
    integer :: i
    character(len=50) :: temp_string
    logical :: exists

    exists = .FALSE.
    inquire(file=file_name, exist=exists)
    if (.not.exists) then
      write(*,*) 'Error ', file_name, ' not found.'
      stop
    endif
    open(unit=file_ref, file=file_name)

    do while ( trim(temp_string) /= '$PhysicalNames' )
      read(file_ref, *) temp_string
    enddo
    read(file_ref, *) number_properties

    allocate( this%property(number_properties), stat=status )
    if( status /= 0 ) stop 'Error allocating list of properties, not enough RAM!'

    do i = 1, number_properties
      read(file_ref,*) this%property(i)%entity_type, this%property(i)%id, temp_string
      this%property(i)%name_id = trim(temp_string)
      if( this%property(i)%name_id(1:2) == 'b_') this%property(i)%isboundary = .TRUE.
    enddo


  end subroutine

  integer function property_index( this, property_id )
    !! return the index in the array of properties held within the property set object
    !! that matches the property id queried.
    class(propertySetC), intent(in) :: this
    integer            , intent(in)  :: property_id

    integer :: i

    property_index = -1
    do i = 1, size(this%property)
      if ( property_id == this%property(i)%property_id() ) then
        property_index = i
      endif
    enddo
  end function

  subroutine property_1d_unit_test()
    type(propertySetC) :: properties
    logical :: test_passed
    integer :: test_result
    integer :: i

    call properties%read('test_data/1d_fem_test.msh',145)
    test_result = properties%property(1)%entity_type
    test_result = test_result + properties%property(2)%entity_type
    test_result = test_result + 1 - properties%property(3)%entity_type
    do i = 1, 3
      test_result = test_result + i - properties%property(i)%id
    enddo
    if( test_result /= 0 ) stop 'Error in unit test of 1D FEM test mesh'
    if (properties%property(1)%name_id /= '"b_left"') stop 'Error in unit test of 1D FEM test mesh'
    if (properties%property(2)%name_id /= '"b_right"') stop 'Error in unit test of 1D FEM test mesh'
    if (properties%property(3)%name_id /= '"p_fuel"') stop 'Error in unit test of 1D FEM test mesh'

  end subroutine



  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - !
  !                         General Property Functions                        !
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - !
  integer function property_id( this )
    !! returns the property id for a particular property object
    class(propertyC), intent(in) :: this

    property_id = this%id
  end function

  character(len=50) function get_property_name( this )
    !! returns the name associated with the property
    class(propertyC), intent(in) :: this
    get_property_name = this%name_id
  end function

  logical function property_is_boundary( this )
    !! returns the logical stating whether the physical property is that of a boundary
    class(propertyC), intent(in) :: this
    property_is_boundary = this%isboundary
  end function

end module
