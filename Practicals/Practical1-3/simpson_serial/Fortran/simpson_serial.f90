! Module with constant values
module consts
  real (kind=8) :: pi
end module consts

!The syntax for the sin function in the Fortran language is: result=sin(x)
!x is value expressed in radians (not degrees).
subroutine degtorad(deg, rad)
  use consts
  real(kind=8), intent(in) :: deg
  real(kind=8), intent(out) :: rad
  rad=(pi*deg)/180.0
end subroutine degtorad

subroutine func(x, funcx)  
  real(kind=8), intent(in):: x
  real(kind=8), intent(out):: funcx
  funcx=sin(x)*sin(x)
end subroutine func

!Simpson's function
subroutine simpson(n, h, fx, area)
  use consts
  integer (kind=4), intent(in):: n 
  real(kind=8), intent(in) :: h
  real (kind=8), intent(in) :: fx(n+1)
  real(kind=8), intent(out) :: area
  real(kind=8) :: mult_rad

  !Apply Simpson’s Rule Formula 
  area=fx(1)+fx(n+1)
  !write(6, *) 'area= ', area
  do i=2, n, 1
    if(mod(i,2) .eq. 0) then 
      area=area+2*fx(i)
    else 
      area=area+4*fx(i)
    end if
  end do
  
  !Multiply area by h/3 after converting it to radians 
  call degtorad((h/3.0), mult_rad)
  area=mult_rad*area
 
end subroutine simpson

program Simpsoncode
  use consts
  implicit none
  integer (kind=4) :: i, n
  real (kind=8) :: a, b, h, deg, rad, area
  real (kind=8), allocatable :: x(:), fx(:)

  ! The value of pi 
  pi = atan(1.0) * 4.0

  !Enter the values for a, b and n
  !and calculate the value of h
  !Note: The interval [a,b] is split up into n subintervals of width h with n an even number.  
  write(6,*) 'Enter a, b and n'
  read(5,*) a, b, n

  if (mod(n,2) .ne. 0) then
    write(6,*) ' n should be even!'
    return
  endif
  h = (b-a)/n

  !Allocate x and fx vectors where x=[a x1 ... b]
  !and fx =[f(a) f(x1) ... f(b)]
  !and a=x0, b=xn
  !Note: the value of the pts and the value of the function at these pts
  !are stored as arrays
  allocate(x(n+1))
  allocate(fx(n+1))

  !Calculate the values of x and fx arrays 
  !Note: The values of xi should be in radians 
  do i=1,n+1
    deg = a+(i-1)*h
    call degtorad(deg, rad)
    x(i)=rad
    call func(x(i), fx(i))
    !write(6,*) 'n=', n, 'a=', a, 'b=', b, 'h=', h
    !write(6, *)'x[', i, ']= ', x(i), 'fx[', i, ']= ', fx(i)
  end do

  !Call simpson's function to find an approximated value for the area
  call simpson(n, h, fx, area)

  !Print the approximated and exact value of the integral 
  write(6,*) 'Approximated area by Simpsons rule is: ', area
  write(6,*) 'Exact result is: ', pi/4.0

  deallocate(x)
  deallocate(fx)
  stop
end program 

  
