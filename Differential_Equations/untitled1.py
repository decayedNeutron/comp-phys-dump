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

import pandas as pd
images = []

filename = 'B1100.gif'
T = pd.read_csv('Heat8.dat', header = None, sep = '\s+', dtype = 'float64')
T = np.array(T)
print('.DAT Reading Done') 


for i in range(1,1000):
    plt.figure(figsize = (6,5))
    plt.pcolor(T[100*i:100*(i+1),0:100],cmap = 'hot')
    plt.colorbar()
    plt.xlabel("X")
    plt.ylabel("Y")
    plt.title('Color Plot for the Temperature Profile')
    img_buf = io.BytesIO()
    plt.savefig(img_buf)

    im = Image.open(img_buf)
    draw = ImageDraw.Draw(im)
    draw.text((10,5),"Time: " + str(1*i), fill = (0,0,0))
    images.append(im)
    plt.close()
    print("\r", end = "")
    print('Figure {} Rendered'.format(i), end = '')
    print("\r", end = "")

print("All Figures rendered")

print('Making the Final .GIF', end = "")

images[0].save(filename,save_all=True, append_images=images[1:], optimize=False, duration=10, loop=0)
img_buf.close()
print("\r", end = "")
print(filename + " is ready")
