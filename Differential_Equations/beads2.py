#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Feb 16 01:47:01 2023

@author: jaatPTM
"""

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
PI = np.pi
width = 400
amp = 40
center = width // 2
color_1 = (0, 0, 0)
color_2 = (255, 255, 255)
color_3 = (168, 170, 173)
font = ImageFont.truetype('cmunrm.ttf', 25) 
cent = np.array([center,center])
r = 10
for i in range(1,2000,10):
    im = Image.new('RGB', (width,width), color_1)
    
    draw = ImageDraw.Draw(im)
    draw.text((10,10),'{a:.2f} s'.format(a =t[i]),font = font)
    for j in range(16,40):
        k  = j+1
        cent = np.array([center,center])
        
     #   if j+1 <= 0: k = 50 + j
      #  if (j > 13 & j < 37): cent = cent + np.array([0,100])
        
        p = cent + np.array([180*np.sin(((2*PI*j)/50)+0.76*PI/3),amp*a[i,k]+50*np.cos(2*PI*j/50)]) 
        draw.ellipse((p[0]-r, p[1]-r, p[0]+r, p[1]+r), fill=(255,0+3*j,255-3*j),outline = color_1)
        draw.text((p[0]-5,p[1]-5),str(k),fill = color_1)
        
    for j in range(15,-1,-1):
        k  = j+1
        cent = np.array([center,center])
        
     #   if j+1 <= 0: k = 50 + j
      #  if (j > 13 & j < 37): cent = cent + np.array([0,100])
        
        p = cent + np.array([180*np.sin(((2*PI*j)/50)+0.76*PI/3),amp*a[i,k]+50*np.cos(2*PI*j/50)]) 
        draw.ellipse((p[0]-r, p[1]-r, p[0]+r, p[1]+r), fill=(255,0+3*j,255-3*j),outline = color_1)
        draw.text((p[0]-5,p[1]-5),str(k),fill = color_1)
        if i == 1: print(k,p,a[i,k])
    for j in range(49,39,-1):
        k  = j+1
        cent = np.array([center,center])
        
     #   if j+1 <= 0: k = 50 + j
      #  if (j > 13 & j < 37): cent = cent + np.array([0,100])
        
        p = cent + np.array([180*np.sin(((2*PI*j)/50)+0.76*PI/3),amp*a[i,k]+50*np.cos(2*PI*j/50)]) 
        draw.ellipse((p[0]-r, p[1]-r, p[0]+r, p[1]+r), fill=(255,0+3*j,255-3*j),outline = color_1)
        draw.text((p[0]-5,p[1]-5),str(k),fill = color_1)
        if i == 1: print(k,p,a[i,k])
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
    
    
images2[0].save('BeadsOrigp20000.gif',save_all=True, append_images=images2[1:], optimize=False, duration=20, loop=0)
 