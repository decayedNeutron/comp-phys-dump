#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Feb 16 02:58:37 2023

@author: jaatPTM
"""

import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from supp import dat_read


a = dat_read('ParticlesWrong2.dat')
t = a[:,0]
y = a[:,1]

# plt.figure(figsize =(10,5))
# plt.plot(t,y)
# plt.xlabel('Time (s)', fontsize = 15)
# plt.ylabel('y', fontsize = 15)
# plt.plot([25,25,27,27,25],[-0.25,0.8,0.8,-0.25,-0.25],'r--',alpha = 0.5, label = "Energy from other end reaches")
# plt.title('Position of 1st Particle with time', fontsize = 20)
# plt.legend()
# plt.savefig('FirstParticle.jpg', dpi = 400)
