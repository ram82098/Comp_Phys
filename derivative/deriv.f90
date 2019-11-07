Program Deriv1

Implicit none 

!Creates 2 allocatable arrays, one for function and one for its derivative 
Real, Allocatable :: y(:), yprime(:)
Real :: x,dx

Integer :: i, grid_pts

print*,"Enter number of grid points: "
read*, grid_pts

!Allocates memory for the arrays of whatever input size is used
Allocate(y(grid_pts),yprime(grid_pts))

!Step size 
dx = 10.0/(grid_pts -1)

!Loop t to update step size
do i = 1, grid_pts

x = (i-1)*dx
y(i) = cos(x)
end do 

Call deriv(y,grid_pts,dx,yprime)

Do i= 1,grid_pts
x=(i-1)*dx
print*,yprime(i), -sin(x)-yprime(i)
end do 

!Deallocating memory from array
Deallocate(y,yprime)

End program

!Subroutine used to perform the differentiation 
Subroutine deriv (a, np, h, aprime)

Implicit none

Integer, Intent(in) :: np
Real, Intent(in) :: a(np),h
Real, Intent(out) :: aprime(np)
Integer :: i
!loops for input
Do i = 1,(np-1)

!Calculation for derivative numerically 
aprime(i) = (a(i+1)-a(i))/h

end do 
!Sets all entries in array to 0
aprime(np)=0.0
End Subroutine deriv
