#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Feb  9 23:09:14 2023

@author: jaatPTM
"""

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd


def dat_read(file):
    file1 = open(file,'r')
    lines = file1.readlines()
    r = len(lines[0].strip().split())
    data = np.array([np.zeros(r)],dtype='float64')
    for l in lines:
        row = l.strip().split()
        a = np.array([row],dtype = 'float64')
        data = np.append(data,a,axis = 0)
        
    return np.delete(data,0,0)

def dat_read2(file):
    
    df = pd.read_csv(file, header = None)
    p,r = df.shape
    r = len(df.iat[0,0].strip().split())
    data = np.array([np.zeros(r)],dtype='float64')
    for i in range(p):
        row = df.iat[i,0].strip().split()
        a = np.array([row],dtype = 'float64')
        data = np.append(data,a,axis = 0)
        
    return np.delete(data,0,0)

T = dat_read2('/home/jaatPTM/Comp_Phys/DiffEqns/Heat4.dat')