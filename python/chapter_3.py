from functools import reduce
import pandas as pd
import numpy as np
import math


# Forward Euler Scheme
end_t = 50
dfs   = []

for step_size in [5, 2.5, 1.25]:
    n_points = int(end_t / step_size + 1)
    
    S = np.zeros((n_points, 1))
    I = np.zeros((n_points, 1))
    R = np.zeros((n_points, 1))
    
    S[0] = 0.5
    I[0] = 0.3
    R[0] = 0.2
    beta = 0.5
    gamma = 0.2
    
    for k in range(n_points - 1):
        S[k + 1] = -step_size * beta * S[k] * I[k] + S[k]
        I[k + 1] = step_size * (beta * S[k] * I[k] - gamma * I[k]) + I[k]
        R[k + 1] = step_size * gamma * I[k] + R[k]
        
    df = pd.DataFrame(index=np.linspace(0, end_t, n_points))
    df[f"S_h={step_size}"] = S
    df[f"I_h={step_size}"] = I
    df[f"R_h={step_size}"] = R
    df.index.name = "t"
    dfs.append(df)

df = reduce(lambda left, right: pd.merge(left, right, how="outer", left_index=True, right_index=True), dfs)
df.to_csv("../data/H3_sir_forward_euler.csv")


# Nonstandard Scheme
end_t = 50
dfs   = []

phi = lambda h: h

for step_size in [5, 2.5, 1.25]:
    n_points = int(end_t / step_size + 1)
    
    S = np.zeros((n_points, 1))
    I = np.zeros((n_points, 1))
    R = np.zeros((n_points, 1))
    
    S[0] = 0.5
    I[0] = 0.3
    R[0] = 0.2
    beta = 0.5
    gamma = 0.2
    
    for k in range(n_points - 1):
        S[k + 1] = S[k] / (1 + phi(step_size) * beta * I[k])
        I[k + 1] = (phi(step_size) * beta * S[k + 1] * I[k] + I[k]) / (1 + phi(step_size) * gamma)
        R[k + 1] = phi(step_size) * gamma * I[k + 1] + R[k]
        
    df = pd.DataFrame(index=np.linspace(0, end_t, n_points))
    df[f"S_h={step_size}"] = S
    df[f"I_h={step_size}"] = I
    df[f"R_h={step_size}"] = R
    df.index.name = "t"
    dfs.append(df)

df = reduce(lambda left, right: pd.merge(left, right, how="outer", left_index=True, right_index=True), dfs)
df.to_csv("../data/H3_sir_nonstandard.csv")
