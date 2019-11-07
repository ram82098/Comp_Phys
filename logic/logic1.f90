Program Logic1

Implicit none 


	real :: a,x,y,z
	
	print*,"Input values for a,x,y,z: "
	read*,a,x,y,z

	If (y>=1,0) Then
	y=y+.5

	else
	y=y+.25

	end if
	if (z==1.0) Then
	Print*,z

	else if
	 (z==2.0)
	Print*,'z is:',2

	end if 

	If (a<0) Then
	a=-a
	print*,"a=",-a
	end if 

	If (x>0) print*,'x is positive'

stop

End Program

