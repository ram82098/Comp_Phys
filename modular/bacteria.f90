Program Bacteria 

Implicit none 

Real :: y1,y2,a
print*,'Enter initial population of bacteria: '
read*,y1
a = 1.386

call growth(y1,y2,a)
print*,'The population after 3 hours is: '
print*,y2
end program 

subroutine growth(y1,y2,a)
implicit none 
real, intent(in) :: a,y1
real :: y2
integer :: i

do i=1,3
y2=y1*exp(a*i)
end do

return
end subroutine growth 
