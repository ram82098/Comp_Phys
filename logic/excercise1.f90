Program Excerscise 

Implicit none

Real :: x,y,z,pi,fx
parameter (pi=acos(-1.0))

print*,"Input values for x,y,z: "
read*, x,y,z

if (x<0.0) then 
fx=sqrt(x**3+y**3+z**3)
print*,fx
end if

else if (x==0) then 
fx=(pi/4.0)
print*,fx
end if

else then
fx=sin(x*y)+cos(x*z)
print*,fx
end if

stop 

end program

