!Module for our differential eqns:
!Module+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Module virmensbyte

       Implicit None
       Save
       Contains

       Function dpdr(r, mp ,d, flag, gama)

          Implicit None

          Double Precision :: e, cf, r, pie, d, term1
          Double Precision :: dmsun_mev, dmsun_km,gama

          Double Precision, Dimension(2) :: mp,dpdr

	  Integer:: flag, n

          pie=acos(-1.0)

!conversion factors:
          dmsun_mev = 1.115829d60
          dmsun_km  = 1.475
          cf        = (dmsun_km*1.e18)/dmsun_mev

!dmdr: (mass of object)
          dpdr(1) = 4.*pie*r*r*d*gama

!dpdr: (TOV-Relatavistic Case):

            term1=(1.-2.*mp(1)*cf/r)**gama
            dpdr(2)=-(d+mp(2))*(0.5*r+4*pie*r**3*mp(2)*(cf)-r*0.5*term1)&
                    /(r*r*(1.-2.*mp(1)*cf/r)**gama)

       End Function dpdr


!linear interpolation for EoS:
      Function interpolate1(n,refval,e,p)
         Implicit None
 !        use types, only: dp
         real*8 :: interpolate1
         integer :: n
         real*8, intent(in) :: refval ! reference value
         real*8, dimension(n), intent(in) :: e
         real*8, dimension(n), intent(in) :: p
         integer :: s1,s2 ! subscript holders
         s1 = maxloc(e,1,mask=e<refval) ! subscript of value below refval
         s2 = minloc(e,1,mask=e>refval) ! subscript of value above refval
         interpolate1 = (p(s2)-p(s1))/(e(s2)-e(s1))*(refval-e(s1))+p(s1)
      End Function interpolate1
End Module
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



!Start of main Program======================================================
       Program TOV

!---------------------------------------------------------------------------
! FORTRAN program to solve hydrostatic equilibrium eqns for both the
! newtonian case as well the general relativistic (TOV eqns) case using
! Runge Kutta 4th order methods.
!
! This code will calculate stellar properties such as mass, radius and
! densities of compact stars (neutron & quark stars) with a use of a
! given Equation of State (EoS).
!---------------------------------------------------------------------------

!use module:
       Use virmensbyte

!Declare your variables:
       Implicit None

       Double Precision :: dr, deltaec, d ,q,z
       Double Precision :: ec, pc, bag, r, pie, term1
       Double Precision :: dmsun_km, dmsun_mev, cf, gama
       Double Precision :: h, k1, k2, k3, k4, mp, dnew, dtemp

       Real*8, dimension(317) :: e
       Real*8, dimension(317) :: p

       Integer:: i, flag, n, s1, s2

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!variables definitions:
!
!dr=dr--stepsize, deltaec--change in central density
!ec--central density, pc--central pressure, e & p--density & pressure
!r--radius, pie=PI=3.14159...etc, bag=bag contant in Mev/fm^3
!dmsun_km, dmsun_mev--conversion factors, cf--conversion factor to Mev/fm^3
!k1,k2,k3,k4--rk4 variables
!md--2-dim array for mass & density
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!two dimensional arrays for rk4 variables:
       DIMENSION k1(2)
       DIMENSION k2(2)
       DIMENSION k3(2)
       DIMENSION k4(2)

!2D array for mass-pressure:
!note:  mp(2) is pressure & mp(1) is mass:
       DIMENSION mp(2)

!---------------------------------------------------------------------------

       mp(1)=0  !initial mass (i.e. I.C.)

!conversion factors:
       dmsun_mev = 1.115829d60
       dmsun_km  = 1.475

       cf = (dmsun_km*1.e18)/dmsun_mev

       print*,"Input value for gama: "
       read*,gama !deformation constant

       pie = acos(-1.0)  !Pi=3.14159...


!open data files:
          Open (Unit=25, File='ccnjl.dat', Status='old')
          Open (Unit=30, File='emvsr.dat', Status='unknown')
          Open (Unit=40, File='emvsd.dat', Status='unknown')
          Open (Unit=45, File='QvsM.dat', Status='unknown')
          Open (Unit=50, File='QvsR.dat', Status='unknown')
          Open (Unit=55, File='QvsZ.dat', Status='unknown')


!read in EoS data file:
       Do i =1,317
         Read(25,*) e(i), p(i)
       end do

!initialize central density of 1st stellar model

       ec=1955
       deltaec = 2.0

! Compute sequence of stellar models
       Do while (ec .ge. 99)
         d=ec
         mp(1)=0

         r = 1.e16  !initial radial distance in fermi

!Call EoS interpolate function:
         mp(2) = interpolate1(317,d,e,p)

       Do While (mp(2) .gt. 0.)

         dr = 1.e16  !step size in fermi



!********************************RK4****************************************
         h = dr  !stepsize

!rk4 variables:
         k1(:) = h*dpdr(r,mp(:), d, flag)
         dtemp = interpolate1(317,mp(2)+k1(2)/2.,p,e)
         k2(:) = h*dpdr(r+h/2.0,mp(:)+k1(:)/2.0, dtemp, flag)
         dtemp = interpolate1(317,mp(2)+k2(2)/2.,p,e)
         k3(:) = h*dpdr(r+h/2.0,mp(:)+k2(:)/2.0, dtemp, flag)
         dtemp = interpolate1(317,mp(2)+k3(2),p,e)
         k4(:) = h*dpdr(r+h,mp(:)+k3(:), dtemp, flag)

!approx. solution to dpdr:
         mp(:) = mp(:) + (k1(:) + 2.0*(k2(:)+k3(:)) + k4(:))/6.0

!***************************************************************************

	 q = (gama/3.)*((mp(1))**3)*(1-gama**2)
	 z = (gama*r)/1.e18
!Next density interpolate:
         d = interpolate1(317,mp(2),p,e)

         r = r + dr  !increment radius

       End Do

!***************************************************************************
!                         output on terminal:                              *
!***************************************************************************
       Print *,' R=', r/1.e18, 'km,', '  M=', mp(1)/dmsun_mev,'M_sun'

!output in data files:

!mass vs. equatorial radii:
          Write(30,*) r/1.e18, mp(1)/dmsun_mev

!mass vs. central densities:
          Write(40,*) ec, mp(1)/dmsun_mev

	  Write(45,*) q, mp(1)/dmsun_mev

	  Write(50,*) q, r/1.e18

	  Write(55,*) q, z

!***************************************************************************


!central density of next stellar model:

       ec = ec - deltaec

!format output:
20     Format(2x,f8.4,4x,f8.4)

       End Do

       STOP

       End Program TOV
!End of Main================================================================
