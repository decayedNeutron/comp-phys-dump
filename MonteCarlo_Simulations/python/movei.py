#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Mar  2 17:03:42 2023

@author: jaatPTM
"""

import io

import numpy as np
from PIL import Image, ImageDraw
import matplotlib.pyplot as plt

from supp import dat_read
from mpl_toolkits.mplot3d import Axes3D
import pandas as pd
images = []

filename = 'GRID72.gif'


# print('.DAT Reading Done') 


for i in range(1,644):
    T = pd.read_csv('A4/GRID70'+str(i)+'.dat', header = None, sep = '\s+', dtype = 'float64')
    T = np.array(T)
    T2 = pd.read_csv('A4/GRID270'+str(i)+'.dat', header = None, sep = '\s+', dtype = 'float64')
    T2 = np.array(T2)
    plt.figure(figsize = (6,5))
    ax = plt.axes(projection='3d')
    ax.scatter(T[:,0],T[:,1],T[:,2],color='red')
    ax.scatter(T2[:,0],T2[:,1],T2[:,2],color='blue')
    plt.axis('off')
    plt.title('7x7x7 Lattice', fontsize = 12)
    plt.legend(["Up","Down"])
    img_buf = io.BytesIO()
    plt.savefig(img_buf)
    
    im = Image.open(img_buf)
    draw = ImageDraw.Draw(im)
    draw.text((15,10),"Temperature, kT: " + str(4.7 - 0.02*np.floor(i/14)), fill = (0,0,0))
    images.append(im)
    plt.close()
    print("\r", end = "")
    print('Figure {} Rendered'.format(i), end = '')
    print("\r", end = "")

# for i in range(101,50000,100):
#     T = pd.read_csv('R4/GRID_0'+str(i)+'.dat', header = None, sep = '\s+', dtype = 'float64')
#     T = np.array(T)
#     T2 = pd.read_csv('R4/GRID2_0'+str(i)+'.dat', header = None, sep = '\s+', dtype = 'float64')
#     T2 = np.array(T2)
#     plt.figure(figsize = (6,5))
#     ax = plt.axes(projection='3d')
#     ax.scatter(T[:,0],T[:,1],T[:,2],color='red')
#     ax.scatter(T2[:,0],T2[:,1],T2[:,2],color='blue')
#     plt.axis('off')
#     plt.legend(["Up","Down"])
#     img_buf = io.BytesIO()
#     plt.savefig(img_buf)
    
#     im = Image.open(img_buf)
#     draw = ImageDraw.Draw(im)
#     # draw.text((10,5),"Time: " + str(1*i), fill = (0,0,0))
#     images.append(im)
#     plt.close()
#     print("\r", end = "")
#     print('Figure {} Rendered'.format(i), end = '')
#     print("\r", end = "")

print("All Figures rendered")

print('Making the Final .GIF', end = "")

images[0].save(filename,save_all=True, append_images=images[1:], optimize=False, duration=50, loop=0)
img_buf.close()
print("\r", end = "")
print(filename + " is ready")
