program FEM
use EntityList
use ControlPointListModule
implicit none

type(entityListC) :: entities
type(propertySetC) :: properties
type(controlPointListC) :: control_points

call properties%read('../test_data/1d_fem_test.msh',145)
call entities%read('../test_data/1d_fem_test.msh',145)
call entities%associate(properties)

call control_points%read('../test_data/1d_fem_test.msh', 145, 1)


call entities%print_all()

call control_points%print_all()


end program
