#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Feb  2 17:42:41 2023

@author: jaatPTM
"""

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from supp import dat_read

sca = pd.read_csv('uniform1M.dat', header = None, delimiter='tab')
x = sca.to_numpy()[:,0]
sca2 = pd.read_csv('exponent_check.dat', header = None)
y= sca2.to_numpy()[:,0]
sca3 = pd.read_csv('normal2.dat', header = None)
z= sca3.to_numpy()[:,0]


plt.figure()
plt.hist(x, bins = 50,density = True, color = '#44DEB0')
plt.title('Uniform Random Variable 1M Samples')
plt.savefig('Uniform1M.png',dpi = 400)

plt.figure()
plt.hist(y,bins= 50, density = True,color = '#70474B')
plt.title('Cutted to 1 Exponential Random Variable')
plt.savefig('Exponential1.png',dpi = 400)

plt.figure()
plt.hist(z,bins= 50, density = True,color = '#24423B')
plt.title('Normal Random Variable')
plt.savefig('Normal.png',dpi = 400)