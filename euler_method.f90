! Author: Lucas Marques Moreno
! Date: 26/02/20
! Description: An exercise 
! to solve ODE with Euler's
! method
program euler
implicit none

real,external::f ! add the function in the main program
real :: x,y,h !declaring variables
integer ::n
print *, "Insert the initial condtions for x"
read*, x
print *, "Insert the initial condtions for y"
read *,y
print *, "Insert the initial interval for x"
read *,h



end program euler

real function f(x,y)
real :: x,y
f = x+2*y
end