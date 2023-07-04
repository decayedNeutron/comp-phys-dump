#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Feb 12 21:02:47 2023

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

a = dat_read('Pendulum_fast2.dat')
the = a[:,0]
images = []
images2 = []

font = ImageFont.truetype('cmunrm.ttf', 25) 

width = 400
rod = 150
center = width // 2
color_1 = (0, 0, 0)
color_2 = (255, 255, 255)
max_radius = int(center * 1.5)
cent = np.array([center,center])
step = 8
r = 10
for i in range(1,5001,10):
    im = Image.new('RGB', (width,width), color_1)
    draw = ImageDraw.Draw(im)
    ts = np.sin(the[i])
    tc = np.cos(the[i])
    p = cent + np.array([rod*ts,rod*tc])
    draw.line((cent[0],cent[1],p[0],p[1]),width = 6, fill = (255,0,0))
    draw.ellipse((p[0]-r, p[1]-r, p[0]+r, p[1]+r), fill=color_2)
    
    draw.ellipse((center - 15, center - 15, center + 15, center + 15), fill=(255,125,0))
    draw.text((10,10), "X_0 = 2.0 V_0 = 1.9", font =  font)
    images2.append(im)
    
images2[0].save('Pendulum6_RK4.gif',save_all=True, append_images=images2[1:], optimize=False, duration=33, loop=0)
   
# for i in range(0, max_radius, step):
#     im = Image.new('RGB', (width, width), color_1)
#     draw = ImageDraw.Draw(im)
#     draw.ellipse((center - i, center - i, center + i, center + i), fill=color_2)
#     images.append(im)

# for i in range(0, max_radius, step):
#     im = Image.new('RGB', (width, width), color_2)
#     draw = ImageDraw.Draw(im)
#     draw.ellipse((center - i, center - i, center + i, center + i), fill=color_1)
#     images.append(im)

