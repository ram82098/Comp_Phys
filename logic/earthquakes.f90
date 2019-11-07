Program Earthquakes

Implicit none 

Integer :: x,w
real :: y1,y2,y3

print*,'Enter the year: '
read*,w

if (w == 1906) then
x=1
y1=8.3
end if 

if (w == 1911) then
x=1
y1=6.6
end if

if (w == 1923) then
x=1
y1=7.2
end if

if (w == 1925) then 
x=1
y1=6.3
end if

if (w == 1926) then 
x=1
y1=6.3
end if

if (w == 1927) then 
x=1
y1=7.7
end if

if (w == 1933) then 
x=1
y1=6.3
end if

if (w == 1940) then
x=1
y1=6.7
end if

if (w == 1952) then
x=1
y1=7.8
end if

if (w == 1968) then 
x=1
y1=6.4
end if

if (w == 1971) then 
x=1
y1=6.4
end if

if (w == 1979) then
x=1
y1=6.4
end if 

if (w == 1980) then
x=2
y1=7.0
y2=6.6
end if

if (w == 1983) then 
x=1
y1=6.5
end if

if (w == 1984) then 
x=1
y1=6.2
end if

if (w == 1986) then
x=1
y1=6.0
end if

if (w == 1987) then
x=1
y1=5.9
end if

if (w == 1989) then
x=1
y1=7.1
end if

if (w == 1991) then 
x=3
y1=7.1
y2=5.8
y3=6.1
end if

if (w == 1992) then 
x=2
y1=6.1
y2=6.9
end if

print*, 'Number of Earthquakes that year: ',x

if (x == 1) then 
print*,'Magnitude on the Richter scale: ',y1
end if

if (x == 2) then 
print*,'Magnitudes on the Richter scale: ',y1,y2
end if

if (x == 3) then 
print*,'Magnitudes on the Richter scale: ',y1,y2,y3
end if

stop 
End program











