Program average 

Implicit none 

Real :: x1,y1,z1
Real :: avg, v

Print*,'Enter three numbers: '
print*,''
read*, x1, y1, z1

v = ave(x1,y1,z1)

Print*,'The average of these 3 numbers are is: ',v

End Program 

Subroutine ave(x,y,z,avg)

Implicit none 
Real :: x,y,z
Real :: avg

avg = (x+y+z)/3.0

Return 

End subroutine ave
