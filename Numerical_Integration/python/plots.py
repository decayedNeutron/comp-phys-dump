#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Feb  2 17:25:53 2023

@author: jaatPTM
"""

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd


def dat_read(file):
    file1 = open(file,'r')
    lines = file1.readlines()
    data = np.array([[0,0]],dtype='float64')
    for l in lines:
        row = l.strip().split()
        a = np.array([row],dtype = 'float64')
        data = np.append(data,a,axis = 0)
        
    return np.delete(data,0,0)
            

sca = pd.read_csv('errors.dat', header = None, delimiter='\t')
x = np.array([.166666666623350E-04,0.166666665357695E-06,0.166667479817306E-08,0.166608948859448E-10])
y = np.array([0.01,0.001,0.0001,0.00001])

data =  dat_read('errors_new.dat')

plt.figure()
plt.loglog(data[:,0],data[:,1],'ro')
plt.loglog(data[:,0],data[:,1],'r--',alpha = 0.6)
plt.title('Log-Log Plot for the errors')
plt.xlabel('Order of dx')
plt.ylabel('Error in the integral')
plt.savefig('LogLogPlot3.png',dpi = 400)


    