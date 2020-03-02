! Author: Lucas Marques Moreno
! Date: 26/02/20
! Description: An exercise 
! to solve ODE with Euler's
! method

subroutine euler (y,f,x0,y0,h,xg)

    implicit none
    
    real, external :: f ! add the function in the main program

    real,intent(in) :: x0                  ! initial value of x
    real,intent(in) :: y0                  ! initial value of y
    real,intent(in) :: h                   ! interval of values of x
    real,intent(in) :: xg                  ! x value for solution
    
    real,intent(out) :: y                  ! solved function value
    
    real :: x
    integer ::n,i
    
    x = x0
    y = y0
    n=int(ceiling((xg-x)/h))
    do i=1,n
        x=x+h
        y=y+h*f(x,y)
    end do
    
    return
    end subroutine euler

subroutine rk4(y,f,x0,y0,h,xg)
    implicit none

    real, external :: f
    real,intent(in) :: x0                  ! initial value of x
    real,intent(in) :: y0                  ! initial value of y
    real,intent(in) :: h                   ! interval of values of x
    real,intent(in) :: xg                  ! x value for solution
    real :: k1,k2,k3,k4                    ! Runge Kutta constants
    real :: x,dy                           ! variables do solve
    integer ::n,i
    real,intent(out) :: y                  ! solved function value

    n=int(ceiling((xg-x)/h))

    x = x0
    y = y0
    do i=1,n
        x = x + h
        k1 = h*f(x,y)
        k2 = h*f(x+0.5*h,y+0.5*k1*h)
        k3 = h*f(x+0.5*h,y+0.5*k2*h)
        k4 = f(x+h,y+k3*h)
        dy = (1/6.)*(k1+k4+2*(k2+k3))

        y = y+dy
     
    end do



    end subroutine rk4