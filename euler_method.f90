! Author: Lucas Marques Moreno
! Date: 26/02/20
! Description: An exercise 
! to solve ODE with Euler's
! method
program euler
implicit none

real,external::f ! add the function in the main program
real :: x,y,h,xg !declaring variables
integer ::n,i
print *, "Insert the initial condtions for x"
read*, x
print *, "Insert the initial condtions for y"
read *,y
print *, "Insert the initial interval step for x"
read *,h
print *, "Insert the x soution point"
read *,xg


n=int(ceiling((xg-x)/h))
do i=1,n
    x=x+h
    y=y+h*f(x,y)
    print*,"Value of x: ",x,"Value of y: ",y
end do

print*,"Value of x: ",x,"Value of y: ",y

end program euler

real function f(x,y)
real :: x,y ! define variables
f = x+2*y ! here is the same as y'
end