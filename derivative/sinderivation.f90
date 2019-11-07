Program sinderivation

Implicit none 

Real :: x0,dx,F,dF,dFdx,dfdx2
Integer :: i,j

dx=1.0

do i =1,5
dx = dx/10.0
x0 = 26.0

F = sin(x0)

dFdx=(sin(x0+dx) - sin(x0))/dx

dF = cos(x0)

end do

do j =1,5
dx = dx/10.0
x0 =26.0

dfdx2 = (sin(x0+dx)-sin(x0-dx))/(2*dx)
end do

print*,dFdx 
print*,abs(dFdx-cos(26.0))
print*,abs(dfdx2-cos(26.0))
stop

end program
