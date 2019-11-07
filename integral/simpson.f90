Program Simpson

Implicit none 

Double Precision :: a,b,h,f,x

Integer :: i

a = 0.0;b=3.0;
h = (b-a)/6.0

i = (f(a)+4*f((a+b)/2.0 + f(b)))
print*,h*i


End Program 

Function f(x)

Implicit none

Double Precision :: f,x

f = (x*x) + 1.0
Return 
End function 
