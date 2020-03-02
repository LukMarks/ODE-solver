! Author: Lucas Marques Moreno
! Date: 26/02/20
! Description: An exercise 
! to solve ODE with Euler's
! method

subroutine euler (y,f,x0,y0,h,xg)

    implicit none
    
    real, external :: f ! add the function in the main program
    
    real,intent(in) :: x0,y0,h,xg !declaring variables
    
    real,intent(out) :: y
    
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
