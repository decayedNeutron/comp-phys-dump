#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Feb 27 09:40:04 2023

@author: jaatPTM
"""

import numpy as np


a = 0e0
b = 1e0
dx = 0.01e0
n = np.ceil((b-a)/dx)

d1 = (1e0 - 5e0*dx/2e0)
d2 = (1e0 + 5e0*dx/2e0)
d3 = (10.0e0*dx**2e0)
d4 = 1e0/(2e0-10e0*h**2)