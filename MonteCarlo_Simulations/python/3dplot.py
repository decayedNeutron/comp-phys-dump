#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Mar 20 21:36:53 2023

@author: jaatPTM
"""

# Import libraries
import numpy as np
from supp import dat_read
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
 
# Change the Size of Graph using
# Figsize
fig = plt.figure(figsize=(7,6))
 
# Generating a 3D sine wave
ax = plt.axes(projection='3d')
 
# Creating array points using



a = dat_read('GRIS00.dat')
# b = dat_read('R1/GRID2_01.dat')

# # numpy
# for k in range(10):
#     for j in range(10):   
#         for i in range(10):
#             # x = np.ones(10)*i
#             # y = np.ones(10)*j
#             # z = np.arange(0, 10, 1)
#             a[i,j,:] = np.arange(0, 10, 1)
#         # ax.scatter(x, y, z, color='red')
# # To create a scatte

 
# To create a scatter graph
ax.scatter(a[:,0],a[:,1],a[:,2],color='red')
# ax.scatter(b[:,0],b[:,1],b[:,2],color='yellow')
 
# turn off/on axis
plt.axis('on')
plt.legend(["Up","Down"], fontsize = 10)
plt.title('10x10x10 lattice with all spins +1', fontsize = 10)
# show the graph
plt.savefig('10.png',dpi = 140)
plt.show()