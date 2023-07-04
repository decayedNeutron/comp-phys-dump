#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Feb 16 01:14:45 2023

@author: jaatPTM
"""

import numpy as np
import matplotlib.animation as manimation
import matplotlib.pyplot as plt
import pandas as pd
from PIL import Image, ImageDraw, ImageFont

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

a = dat_read('ParticlesWrong3.dat')
t = a[:,0]
y1 = a[:,1]
images = []
images2 = []

width = 400
amp = 40
center = width // 2
color_1 = (0, 0, 0)
color_2 = (255, 255, 255)
color_3 = (168, 170, 173)
cent = np.array([center,center])
r = 10
for i in range(1,2000,10):
    im = Image.new('RGB', (width,width), color_1)
    draw = ImageDraw.Draw(im)
    for j in range(-6,7):
        k  = j+1
        if j+1 <= 0: k = 50 + j
        p = cent + np.array([j*24,amp*a[i,k]])   
        draw.ellipse((p[0]-r, p[1]-r, p[0]+r, p[1]+r), fill=color_2)
    # for j in range(-10,11):
    #     k  = j+1
    #     amp2 = amp*0.7
    #     r2 = r*0.49
    #     cent = cent + np.array([25,25])
    #     p = cent + np.array([j*24*0.7,amp2*a[i,25+k]])   
    #     draw.ellipse((p[0]-r2, p[1]-r2, p[0]+r2, p[1]+r2), fill=color_3)
    images2.append(im)
    # p = cent + np.array([24,amp*a[i,2]])   
    # draw.ellipse((p[0]-r, p[1]-r, p[0]+r, p[1]+r), fill=color_2)
    # p = cent + np.array([48,amp*a[i,3]])   
    # draw.ellipse((p[0]-r, p[1]-r, p[0]+r, p[1]+r), fill=color_2)
    # p = cent + np.array([72,amp*a[i,4]])   
    # draw.ellipse((p[0]-r, p[1]-r, p[0]+r, p[1]+r), fill=color_2)
    # p = cent + np.array([-24,amp*a[i,5]])   
    # draw.ellipse((p[0]-r, p[1]-r, p[0]+r, p[1]+r), fill=color_2)
    # p = cent + np.array([-48,amp*a[i,6]])   
    # draw.ellipse((p[0]-r, p[1]-r, p[0]+r, p[1]+r), fill=color_2)
    # p = cent + np.array([-72,amp*a[i,7]])   
    # draw.ellipse((p[0]-r, p[1]-r, p[0]+r, p[1]+r), fill=color_2)
    
    
images2[0].save('Beads.gif',save_all=True, append_images=images2[1:], optimize=False, duration=33, loop=0)
 