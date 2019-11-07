Program orbits 

Implicit none 

Real :: x,y,vx,vy
Real :: t,tmax,dt
Real :: xn,yn,vxn,vyn
Real :: grav,Msun

Integer flag 

x = 1.0d0
y = 0.0d0
vx = 0.0d0
vy = 6.29d0
t = 0.0d0

grav = 39.47d0
Msun = 1.0d0

open(unit=1,file='xy-pos.dat',status='unknown')

print*,'Enter max orbital period (i.e. 1 year): '
read*,tmax

print*,'Enter time-step: '
read*,dt

do while (t .le. tmax)

call euler(x,y,vx,vy,t,tmax,dt,xn,yn,vxn,vyn,grav,Msun)
	
write(*,*) t,x,y
write(1,*) x,y

end do 

close(1)

stop 
end program orbits

subroutine euler(x,y,vx,vy,t,tmax,dt,xn,yn,vxn,vyn,grav,Msun)

Implicit none 

Real :: x,y,vx,vy
Real :: t,tmax,dt
Real :: xn,yn,vxn,vyn
Real :: grav,Msun

xn = x+vx*dt
	yn = y+vy*dt
	
	!updates x velocity
	vxn = vx - (grav*Msun*x*dt/(x**2+y**2)**1.5d0)


	!updates y velocity 
	vyn = vy - (grav*Msun*y*dt/(x**2+y**2)**1.5d0)

	!re-declare all variables changed by timestep 
	x = xn
	y = yn
	vx = vxn
	vy = vyn

	t = t + dt

end subroutine euler

