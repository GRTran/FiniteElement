program FEM
use EntityList
implicit none

type(entityListC) :: entities
type(propertySetC) :: properties

call properties%read('../test_data/1d_fem_test.msh',145)
call entities%read('../test_data/1d_fem_test.msh',145)
call entities%associate(properties)


call entities%print_all()


end program
