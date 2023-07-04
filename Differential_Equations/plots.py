#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Feb 15 17:27:45 2023

@author: jaatPTM
"""

import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from supp import dat_read


for i in range(0,6):
    d1 = dat_read('Pendulum_new'+str(i)+'.dat')
    v0 = d1[0,1]
    x0 = d1[0,0]

    plt.figure(figsize =(10,5))
    plt.plot(d1[:,2],d1[:,0])
    plt.xlabel('Time (s)', fontsize = 15)
    plt.ylabel('x (arb. units)', fontsize = 15)
    plt.title('Time graph for $x$ for $v_0 ={} $ , $x_0 = {}$'.format(v0,x0),fontsize = 20)
    plt.savefig('xt_'+str(i)+'.png', dpi = 350)

    plt.figure(figsize =(10,5))
    plt.plot(d1[:,2],d1[:,1],'r-')
    plt.xlabel('Time (s)', fontsize = 15)
    plt.ylabel('$v$ (arb. units)', fontsize = 15)
    plt.title('Time graph for $v$ for $v_0 ={} $ , $x_0 = {}$'.format(v0,x0),fontsize = 20)
    plt.savefig('vt_'+str(i)+'.png', dpi = 350)

    plt.figure(figsize = (8,8))
    plt.plot(d1[:,0],d1[:,1],'g-', linewidth = 1)
    plt.xlabel('$x$ (arb. units)', fontsize = 15)
    plt.ylabel('$v$ (arb. units)', fontsize = 15)
    plt.title('Phase Diagram for for $v_0 ={} $ , $x_0 = {}$'.format(v0,x0),fontsize = 20)
    plt.savefig('phase_'+str(i)+'.png', dpi = 350)