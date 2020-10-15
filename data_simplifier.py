#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jul 10 22:02:43 2020

@author: oscarbrooks
"""

import pandas as pd
import numpy as np
import tkinter as tk
from tkinter import filedialog
import seaborn as sns
import matplotlib as plt
# 194 209


#Slug_Flow-  196, 202, 208, 210, 214, 216

#Stratified_Flow-  198

#Plug_Flow- 200, 206, 212

#Annular_Flow- 204

#Bubble_Flow- 218, 220, 221, 222 , 223, 224, 225, 226, 227, 228
'''
df_194 = pd.read_csv("ET_images/194-ert.csv", header = None, skiprows=2).iloc[:, 2:]
df_196 = pd.read_csv("ET_images/196-ert.csv", header = None, skiprows=2).iloc[:, 2:]
df_198 = pd.read_csv("ET_images/198-ert.csv", header = None, skiprows=2).iloc[:, 2:]
df_200 = pd.read_csv("ET_images/200-ert.csv", header = None, skiprows=2).iloc[:, 2:]
df_202 = pd.read_csv("ET_images/202-ert.csv", header = None, skiprows=2).iloc[:, 2:]
df_204 = pd.read_csv("ET_images/204-ert.csv", header = None, skiprows=2).iloc[:, 2:]
df_206 = pd.read_csv("ET_images/206-ert.csv", header = None, skiprows=2).iloc[:, 2:]
df_208 = pd.read_csv("ET_images/208-ert.csv", header = None, skiprows=2).iloc[:, 2:]
df_209 = pd.read_csv("ET_images/209-ert.csv", header = None, skiprows=2).iloc[:, 2:]
df_210 = pd.read_csv("ET_images/210-ert.csv", header = None, skiprows=2).iloc[:, 2:]
df_212 = pd.read_csv("ET_images/212-ert.csv", header = None, skiprows=2).iloc[:, 2:]
df_214 = pd.read_csv("ET_images/214-ert.csv", header = None, skiprows=2).iloc[:, 2:]
df_216 = pd.read_csv("ET_images/216-ert.csv", header = None, skiprows=2).iloc[:, 2:]
df_218 = pd.read_csv("ET_images/218-ert.csv", header = None, skiprows=2).iloc[:, 2:]
df_220 = pd.read_csv("ET_images/220-ert.csv", header = None, skiprows=2).iloc[:, 2:]
df_221 = pd.read_csv("ET_images/221-ert.csv", header = None, skiprows=2).iloc[:, 2:]
df_222 = pd.read_csv("ET_images/222-ert.csv", header = None, skiprows=2).iloc[:, 2:]
df_223 = pd.read_csv("ET_images/223-ert.csv", header = None, skiprows=2).iloc[:, 2:]
df_224 = pd.read_csv("ET_images/224-ert.csv", header = None, skiprows=2).iloc[:, 2:]
df_225 = pd.read_csv("ET_images/225-ert.csv", header = None, skiprows=2).iloc[:, 2:]
df_226 = pd.read_csv("ET_images/226-ert.csv", header = None, skiprows=2).iloc[:, 2:]
df_227 = pd.read_csv("ET_images/227-ert.csv", header = None, skiprows=2).iloc[:, 2:]
df_228 = pd.read_csv("ET_images/228-ert.csv", header = None, skiprows=2).iloc[:, 2:]

bubble_p = pd.concat([df_228,df_227,df_226,df_225,df_224,df_223,df_222,df_221,df_220,df_218])
slug_p = pd.concat([df_196,df_202,df_208,df_210,df_214,df_216])
bubble_s = pd.concat([df_225,df_224,df_223])
slug_s = pd.concat([df_202,df_210,df_216,df_222,df_221,df_220])
plug_p = pd.concat([df_200,df_206,df_212])
strat_p = df_198
annular_p = df_204
transient_s = df_228
#df = slug_p
'''
'''
test = df.iloc[-40::,0].mean()
print(test)
test2 = df.iloc[-20:,0].mean()
print(test2)
print((test2+test)/2)
'''
experiment = 228
av = 30
df = pd.read_csv('ET_images/{}-ert.csv'.format(experiment), header = None, skiprows=2).iloc[:, 2:]

df = df.groupby(np.arange(len(df))//av).mean()
if 10000/av != int(10000/av):
    df.drop(df.tail(1).index,inplace=True)
print(df)

'''
print(df.iloc[-1:,0])
#df.drop(df.tail(1).index,inplace=True)
'''
def cov_mat(df):
    cm = df.iloc[:,0:316].corr()
    sns.heatmap(cm, cmap = sns.diverging_palette(220, 20, as_cmap=True) , vmin=-1, vmax=1, cbar_kws=dict(ticks=[-1,-0.75,-0.5,-0.25,0,0.25,0.5,0.75,1]) )

def sumdf(df):
    x = 0
    for i in range(len(df)):
        x += sum(df.iloc[i,:])
    return x

def avdf(df):
    x = []
    for i in range(len(df)):
        x.append(df.iloc[i,:].mean())
    return x, sumdf(x)/len(x)
'''
df2 = pd.DataFrame.reset_index(df.iloc[::40, :])
df = df2.drop('index', axis = 1)
df = df.T.reset_index(drop=True).T
'''

cnv = tk.Canvas(tk.Tk(), width = 150, height = 100)
cnv.pack()
def exportme ():
    global df
    path = filedialog.asksaveasfilename(defaultextension='.csv')
    df.to_csv(path, index = False, header=True)
saveAsButton_CSV = tk.Button(text='CLICK TO EXPORT', command=exportme)
cnv.create_window(75, 50, window=saveAsButton_CSV)
tk.Tk().mainloop()
