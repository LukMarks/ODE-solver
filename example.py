# Author: Lucas Marques Moreno
# Date: 02/03/2020
# Description: A brief exemple
# about how to use my ode solver

import ode
import callback as cb

async def f(x,y_l):
    f = x+2*y_l
    return

g = lambda x, y_l : x+2*y_l 

x0 = 0. # initial value for x
y0_l = 0. # initial value for y'
h =1e-6 # steps for the solution iterations
x = tuple([1.]) # solution the ODE at that point in x 
y = 0 # initial value for the solution
#y_euler = ode.euler(x0,f,x0,y0_l,h,x)
y_rk4 = ode.rk4(y,f,x0,y0_l,h,x)
print(y_rk4)
