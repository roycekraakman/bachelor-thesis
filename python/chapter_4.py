from functools import reduce
import scipy.special
import pandas as pd
import numpy as np
import math


# Import Mittag-Leffler solution
import os
cwd = os.getcwd()
os.chdir(f"{cwd}/mittag_leffler_master")
from mittag_leffler import ml
os.chdir(cwd)


# Setup for GL approximation
def c(v, alpha):
    return (-1) ** (v - 1) * scipy.special.binom(alpha, v)
def gl(y, k, alpha):
    return sum([c(v, alpha) * y[k - v] for v in range(1, k + 1)])
def r(k, alpha):
    return (math.gamma(1) / math.gamma(1 - alpha)) * k ** (-alpha)


end_t = 1
alpha = 0.8
lam   = -4
y0    = 0.5


# Forward Grunwald-Letnikov
dfs   = []
t = index=np.linspace(0, end_t, 2000)
df = pd.DataFrame(index=t)
df["exact"] = y0 * ml(lam * t ** alpha, alpha)
df.index.name = "t"
dfs.append(df)

for step_size in [1, 0.5, 0.25, 0.125, 0.0625, 0.03125]:
    n_points = int(end_t / step_size + 1)
    
    y = np.zeros((n_points, 1))
    
    y[0] = y0
    
    for k in range(n_points - 1):
        y[k + 1] += step_size ** alpha * lam * y[k]
        y[k + 1] += gl(y, k + 1, alpha) + r(k + 1, alpha) * y[0]
        
    df = pd.DataFrame(index=np.linspace(0, end_t, n_points))
    df[f"h={step_size}"] = y
    df.index.name = "t"
    dfs.append(df)

df = reduce(lambda left, right: pd.merge(left, right, how="outer", left_index=True, right_index=True), dfs)
df.to_csv("../data/H4_forward_gl_decay.csv")


# Backward Grunwald-Letnikov
dfs   = []
t = index=np.linspace(0, end_t, 2000)
df = pd.DataFrame(index=t)
df["exact"] = y0 * ml(lam * t ** alpha, alpha)
df.index.name = "t"
dfs.append(df)


for step_size in [1, 0.5, 0.25, 0.125, 0.0625, 0.03125]:
    n_points = int(end_t / step_size + 1)
    
    y = np.zeros((n_points, 1))
    
    y[0] = y0
    
    for k in range(n_points - 1):
        y[k + 1] += gl(y, k + 1, alpha) + r(k + 1, alpha) * y[0]
        y[k + 1] /= (1 - lam * step_size ** alpha)
        
    df = pd.DataFrame(index=np.linspace(0, end_t, n_points))
    df[f"h={step_size}"] = y
    df.index.name = "t"
    dfs.append(df)

df = reduce(lambda left, right: pd.merge(left, right, how="outer", left_index=True, right_index=True), dfs)
df.to_csv("../data/H4_backward_gl_decay.csv")
