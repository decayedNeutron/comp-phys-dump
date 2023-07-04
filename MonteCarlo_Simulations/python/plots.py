#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Mar 16 18:13:02 2023

@author: jaatPTM
"""

import matplotlib.pyplot as plt
from supp import dat_read
import numpy as np
a = dat_read("/home/jaatPTM/Comp_Phys/MCSimuls/IsingMore7ABS.dat")
b= dat_read("/home/jaatPTM/Comp_Phys/MCSimuls/IsingMore8ABS.dat")
c= dat_read('IsingMore9ABS.dat')
# cent = sum(a[:,1])/50000
# x = np.linspace(0,50000,num=50000)

x = 6
y = 35
# i = np.argmax(b[:,x])
# # y = np.ones(50000)*cent

plt.figure(figsize=(9,4))
plt.plot(a[:,0],(a[:,x]),color = 'red', label = "L = 7")
plt.plot(b[:,0],(b[:,x]),color = 'blue',label = "L = 8")
plt.plot(c[:,0],(c[:,x]),color = 'green', label = "L = 9")
# # i = np.argmax(a[:,x])
plt.plot([4.5,4.5], [0.25,0.75],"y--")
# plt.plot([a[y,0]],[a[y,x]],'ro', label = '$\chi = $ {:.2f}'.format(a[y,x]))
# i = np.argmax(b[:,x])
# plt.plot([4.2,4.5], [b[i,x],b[i,x]],"y--")
# plt.plot([b[y,0]],[b[y,x]],'bo',label = '$\chi = $ {:.2f}'.format(b[y,x]))
# i = np.argmax(c[:,x])
# plt.plot([4.3,4.5], [c[i,x],c[i,x]],"y--")
# plt.plot([c[y,0]],[c[y,x]],'go', label = '$\chi = $ {:.2f}'.format(c[y,x]))
# # plt.plot(x,y,label = 'Value of E/spin fulctuates around {:.5f}'.format(cent), color = 'blue')
# # plt.xlim(4.43,4.72)
# plt.ylim(0,1500)
# plt.title("$\chi$ (without taking avg of absolutes)v/s T",fontsize=18)
plt.legend()
# plt.grid()
# plt.xlabel("Temperature",fontsize=15)
# plt.ylabel("$\chi$",fontsize = 15)
# plt.savefig("chi_noise.png", dpi = 300)

# plt.savefig('Rand@405E.png', dpi = 300)
# cent = sum(a[:,2])/50000
# x = np.linspace(0,50000,num=50000)


plt.title("Binder's Cumulant v/s T", fontsize = 18)
plt.xlabel("Temperature", fontsize = 15)
plt.ylabel("Binder's Cumulant", fontsize = 15)
plt.grid()
plt.savefig("BC2.png",dpi = 300)

# y = np.ones(50000)*cent
# plt.figure(figsize=(9,4))
# plt.plot(a[:,0],a[:,2],color = 'blue',linewidth = 0.2)
# plt.plot(x,y,label = 'Value of M/spin fulctuates around {:.5f}'.format(cent), color = 'red')
# plt.ylim(-1,1)
# plt.title("Random Initial Condition @ kT = 4.05")
# plt.legend()
# plt.xlabel("Time/Iterations")
# plt.ylabel("M per spin")

# plt.savefig('Rand@405M.png', dpi = 300)