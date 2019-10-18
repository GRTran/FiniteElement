module PhysicalProperties
  use VariablePrecision
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

  type :: propertySetC
    type(propertyC), allocatable :: properties(:)
  contains
    procedure, public, pass :: property_index
  end type

  type propertyC
    integer :: id ! this is the id of the physical property
    character(len=50) :: name_id ! this is the name of the physical property
    integer :: dim
    logical :: isboundary

  contains
    procedure, public, pass :: property_id
  end type

contains

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - !
  !                     Physical Set Property Functions                       !
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - !
  integer function property_index( this, property_id )
    class(propertySetC), intent(out) :: this
    integer            , intent(in)  :: property_id

    integer :: i

    do i = 1, size(this%properties)
      if ( property_id == this%properties%property_id(i) ) then
        property_index = i
      endif
    enddo


  end function


  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - !
  !                         General Property Functions                        !
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - !
  integer function property_id( this )
    class(propertyC), intent(out) :: this

    property_id = this%id
  end function

end module
