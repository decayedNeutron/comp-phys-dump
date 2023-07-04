#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Mar 23 18:13:58 2023

@author: jaatPTM
"""


import matplotlib.pyplot as plt
import numpy as np
from supp import dat_read


a = dat_read("Is_Alln1_49Final.dat")
x = 2
cent = np.mean(a[10000,x])
plt.figure(figsize=(10,4))
plt.plot(a[10000:,0],a[10000:,x], 'r-', linewidth = 0.1)
plt.ylim(-1,1)
plt.title('All spin +1 condition @ $k_B T = 4.9$', fontsize = 18)
plt.xlabel("Time/ Monte Carlo Steps", fontsize = 14)
plt.ylabel("$\\frac{M}{spin}$", fontsize = 15)
plt.plot([10000,50000],[cent,cent], color = "blue", label = "$\\frac{M}{Spin}$ fluctuates around "+"{}".format(cent))
plt.legend(fontsize = 12)
plt.savefig("R1_49NewM.png", dpi = 300)