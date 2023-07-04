#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Feb 16 04:32:21 2023

@author: jaatPTM
"""

import matplotlib.pyplot as plt
import numpy as np
from supp import dat_read
# d = dat_read('Gauss1000201.dat')
# dd = dat_read('Gauss1000210.dat')
# ddd = dat_read('Gauss1000300.dat')

# plt.figure(figsize = (8,5))
# plt.plot(d[:,0],d[:,1], '#f7ed57', label = '1st Iteration', alpha = 0.8)
# plt.plot(dd[:,0],dd[:,1], '#f7ed57', label = '5th Iteration', alpha = 0.7)
# plt.plot(ddd[:,0],ddd[:,1], '#f7ed57', label = '10th Iteration', alpha = 0.6)

for i in range(1,8):
    d1 = dat_read('NewGs'+str(i)+'.dat')
    i = i + 1
    plt.plot(d1[:,0],d1[:,1], color = (1,(255-i*30)/255,(0+20*i)/255), alpha = 0.9-(i)*0.05, label = 'converge at $10^{-'+str(i)+'}$')

# d2 = dat_read('Gaujjs.dat')

# plt.plot(d2[:,0],d2[:,1], 'r-', label = 'Final Solution')
plt.legend(fontsize = 10)
plt.xlabel('x')
plt.ylabel('y')
plt.grid()
plt.title('Gauss-Seidel Descent to Solution \n for $y\prime\prime - 5y\prime + 10y = 10x$', fontsize = 19)
plt.savefig('Gauss10jj0000.png', dpi = 350)