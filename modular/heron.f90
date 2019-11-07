Program Heron 

Implicit none 

Real :: a,b,c,s
Real :: area

Logical :: e,t1,t2

print*, 'Enter 3 values for 3 sides of a triangle (>0): '
print*, ''
read*, a,b,c

t1 = (a>0) .and. (b>0) .and. (c>0)
t2 = (a+b>c) .and. (a+c>b) .and. (b+c>a)

if ((t1==true).and.(t2==true) then 
e=false
area(a,b,c)

else 
e=true
print*,'Error! Re-run program!'
print*,'Area of triangle: ',0

End program 

Function area(s,a,b,c)
implcit none 

real :: s,a,b,c,v
s = (a+b+c)/2.0
v = sqrt((s*(s-a))*(s-b)*(s-c))
return 
end function area
