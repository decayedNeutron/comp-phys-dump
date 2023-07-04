#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Mar  2 12:43:40 2023

@author: jaatPTM
"""

from supp import dat_read
import matplotlib.pyplot as plt
import numpy as np


T = dat_read('TempProf2NewMul.dat')
plt.figure(figsize = (6,5))
plt.pcolor(T.transpose(), cmap="hot")
plt.colorbar()
plt.xlabel("X")
plt.ylabel("Y")
plt.title('Color Plot for the Temperature Profile')
plt.savefig('TempProf111.jpeg', dpi = 400)