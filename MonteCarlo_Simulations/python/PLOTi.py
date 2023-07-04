#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Mar 23 15:56:05 2023

@author: jaatPTM
"""

import matplotlib.pyplot as plt
import numpy as np
from supp import dat_read


for i in [8,9,10]:
    
    a = dat_read("Is_ran_"+str(i)+"_39.dat")
    a[:,1:] = a[:,1:]*1000/i**3

    centE = np.mean(a[:,1])
    centM = np.mean(a[:,2])
    # plt.figure()
    # plt.hist(a[:,1],density=True,bins = 59,color = 'magenta')
    # plt.xlabel("E per spin")
    # plt.title("Energy fluctuations in {}x{}x{} lattice".format(i,i,i))
    # plt.savefig('HistE_{}.png'.format(i), dpi = 200)
    
    # plt.figure()
    # plt.hist(a[:,1],density=True,bins = 59,color = 'purple')
    # plt.xlabel("M per spin")
    # plt.title("Magnetisation fluctuations in {}x{}x{} lattice".format(i,i,i))
    # plt.savefig('HistM_{}.png'.format(i), dpi = 200)
    
    # plt.figure(figsize = (8,4))
    
    # plt.plot(a[:,0],a[:,1],color = "red", linewidth = 0.2)
    # plt.plot([0,50000],[centE,centE], color = 'blue', label  = "$\\frac{E}{spin}$ fluctuates around: "+"{a:.2f}".format(a=centE))
    # plt.ylim(-3,0)
    # plt.xlabel('Time/Monte Carlo Steps')
    # plt.ylabel('Instantaneous E per spin')
    # plt.legend()
    # plt.title('Energy fluctuations in {}x{}x{} Lattice'.format(i,i,i))
    # plt.grid()
    # plt.savefig('Last_{}.png'.format(i), dpi = 300)
    
    # plt.figure(figsize = (8,4))
    
    # plt.plot(a[:,0],a[:,2],color = "magenta", linewidth = 0.2)
    # plt.plot([0,50000],[centM,centM], color = 'cyan', label  = "$\\frac{M}{spin}$ fluctuates around: "+"{a:.2f}".format(a=centM))
    # plt.ylim(-1,1)
    # plt.xlabel('Time/Monte Carlo Steps')
    # plt.ylabel('Instantaneous M per spin')
    # plt.legend()
    # plt.title('Magetisation fluctuations in {}x{}x{} Lattice'.format(i,i,i))
    # plt.grid()
    # plt.savefig('LastM_{}.png'.format(i), dpi = 300)
    
    plt.figure()
    plt.title("Binder's Cumulant v/s T", fontsize = 18)
    plt.xlabel("Temperature", fontsize = 15)
    plt.ylabel("Binder's Cumulant", fontsize = 15)
    
    plt. plot(a[:,0], a[:,6], )
    