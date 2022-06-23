from functools import reduce
import scipy.special
import pandas as pd
import numpy as np
import math


# Helper GL approximation
def c(v, alpha):
    return (-1) ** (v - 1) * scipy.special.binom(alpha, v)
def gl(y, k, alpha):
    return sum([c(v, alpha) * y[k - v] for v in range(1, k + 1)])
def r(k, alpha):
    return (math.gamma(1) / math.gamma(1 - alpha)) * k ** (-alpha)


"""
Experiment 1.
"""
end_t = 50
alpha = 0.99

# NSE
dfs   = []
for step_size in [5, 2.5, 1.25, 0.625]:
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
        S[k + 1] = S[k] / (1 + step_size * beta * I[k])
        I[k + 1] = (step_size * beta * S[k + 1] * I[k] + I[k]) / (1 + step_size * gamma)
        R[k + 1] = step_size * gamma * I[k + 1] + R[k]
        
    df = pd.DataFrame(index=np.linspace(0, end_t, n_points))
    df[f"S_h={step_size}"] = S
    df[f"I_h={step_size}"] = I
    df[f"R_h={step_size}"] = R
    df.index.name = "t"
    dfs.append(df)

df = reduce(lambda left, right: pd.merge(left, right, how="outer", left_index=True, right_index=True), dfs)
df.to_csv("../data/H5_NSE_scen_1.csv")

# FGL
dfs   = []
for step_size in [5, 2.5, 1.25, 0.625]:
    n_points = int(end_t / step_size + 1)
    
    S = np.zeros((n_points, 1))
    I = np.zeros((n_points, 1))
    R = np.zeros((n_points, 1))
    
    S[0] = 0.5
    I[0] = 0.3
    R[0] = 0.2
    beta = 0.5 ** alpha
    gamma = 0.2 ** alpha
    
    for k in range(n_points - 1):
        S[k + 1] += -step_size ** alpha * beta * S[k] * I[k] 
        S[k + 1] += gl(S, k + 1, alpha) + r(k + 1, alpha) * S[0]
        
        I[k + 1] += step_size ** alpha * (beta * S[k] * I[k] - gamma * I[k]) 
        I[k + 1] += gl(I, k + 1, alpha) + r(k + 1, alpha) * I[0]
        
        R[k + 1] += step_size ** alpha * gamma * I[k] 
        R[k + 1] += gl(R, k + 1, alpha) + r(k + 1, alpha) * R[0]
        
    df = pd.DataFrame(index=np.linspace(0, end_t, n_points))
    df[f"S_h={step_size}"] = S
    df[f"I_h={step_size}"] = I
    df[f"R_h={step_size}"] = R
    df.index.name = "t"
    dfs.append(df)

df = reduce(lambda left, right: pd.merge(left, right, how="outer", left_index=True, right_index=True), dfs)
df.to_csv("../data/H5_FGL_scen_1.csv")

# NSGL
dfs   = []
for step_size in [5, 2.5, 1.25, 0.625]:
    n_points = int(end_t / step_size + 1)
    
    S = np.zeros((n_points, 1))
    I = np.zeros((n_points, 1))
    R = np.zeros((n_points, 1))
    
    S[0] = 0.5
    I[0] = 0.3
    R[0] = 0.2
    beta = 0.5 ** alpha
    gamma = 0.2 ** alpha
    
    for k in range(n_points - 1):
        S[k + 1] += gl(S, k + 1, alpha) + r(k + 1, alpha) * S[0]
        S[k + 1] /= 1 + step_size ** alpha * beta * I[k]
        
        I[k + 1] += gl(I, k + 1, alpha) + r(k + 1, alpha) * I[0]
        I[k + 1] += step_size ** alpha * beta * S[k + 1] * I[k]
        I[k + 1] /= 1 + step_size ** alpha * gamma
        
        R[k + 1] += gl(R, k + 1, alpha) + r(k + 1, alpha) * R[0]
        R[k + 1] += step_size ** alpha * gamma * I[k + 1]
        
    df = pd.DataFrame(index=np.linspace(0, end_t, n_points))
    df[f"S_h={step_size}"] = S
    df[f"I_h={step_size}"] = I
    df[f"R_h={step_size}"] = R
    df.index.name = "t"
    dfs.append(df)

df = reduce(lambda left, right: pd.merge(left, right, how="outer", left_index=True, right_index=True), dfs)
df.to_csv("../data/H5_NSGL_scen_1.csv")


"""
Experiment 2.
"""
end_t = 50
alpha = 0.7

# NSGL
dfs   = []
for step_size in [5, 2.5, 1.25, 0.625]:
    n_points = int(end_t / step_size + 1)
    
    S = np.zeros((n_points, 1))
    I = np.zeros((n_points, 1))
    R = np.zeros((n_points, 1))
    
    S[0] = 0.5
    I[0] = 0.3
    R[0] = 0.2
    beta = 0.5 ** alpha
    gamma = 0.2 ** alpha
    
    for k in range(n_points - 1):
        S[k + 1] += gl(S, k + 1, alpha) + r(k + 1, alpha) * S[0]
        S[k + 1] /= 1 + step_size ** alpha * beta * I[k]
        
        I[k + 1] += gl(I, k + 1, alpha) + r(k + 1, alpha) * I[0]
        I[k + 1] += step_size ** alpha * beta * S[k + 1] * I[k]
        I[k + 1] /= 1 + step_size ** alpha * gamma
        
        R[k + 1] += gl(R, k + 1, alpha) + r(k + 1, alpha) * R[0]
        R[k + 1] += step_size ** alpha * gamma * I[k + 1]
        
    df = pd.DataFrame(index=np.linspace(0, end_t, n_points))
    df[f"S_h={step_size}"] = S
    df[f"I_h={step_size}"] = I
    df[f"R_h={step_size}"] = R
    df.index.name = "t"
    dfs.append(df)

df = reduce(lambda left, right: pd.merge(left, right, how="outer", left_index=True, right_index=True), dfs)
df.to_csv("../data/H5_NSGL_scen_2.csv")

# NSGL-NF1
dfs = []
phi = lambda h: math.exp(h) - 1
for step_size in [5, 2.5, 1.25, 0.625]:
    n_points = int(end_t / step_size + 1)
    
    S = np.zeros((n_points, 1))
    I = np.zeros((n_points, 1))
    R = np.zeros((n_points, 1))
    
    S[0] = 0.5
    I[0] = 0.3
    R[0] = 0.2
    beta = 0.5 ** alpha
    gamma = 0.2 ** alpha
    
    for k in range(n_points - 1):
        S[k + 1] += gl(S, k + 1, alpha) + r(k + 1, alpha) * S[0]
        S[k + 1] /= 1 + phi(step_size) ** alpha * beta * I[k]
        
        I[k + 1] += gl(I, k + 1, alpha) + r(k + 1, alpha) * I[0]
        I[k + 1] += phi(step_size) ** alpha * beta * S[k + 1] * I[k]
        I[k + 1] /= 1 + phi(step_size) ** alpha * gamma
        
        R[k + 1] += gl(R, k + 1, alpha) + r(k + 1, alpha) * R[0]
        R[k + 1] += phi(step_size) ** alpha * gamma * I[k + 1]
        
    df = pd.DataFrame(index=np.linspace(0, end_t, n_points))
    df[f"S_h={step_size}"] = S
    df[f"I_h={step_size}"] = I
    df[f"R_h={step_size}"] = R
    df.index.name = "t"
    dfs.append(df)

df = reduce(lambda left, right: pd.merge(left, right, how="outer", left_index=True, right_index=True), dfs)
df.to_csv("../data/H5_NSGL_NF1_scen_2.csv")

# NSGL-NF2
dfs = []
phi = lambda h: 1 - math.exp(-h)

for step_size in [5, 2.5, 1.25, 0.625]:
    n_points = int(end_t / step_size + 1)
    
    S = np.zeros((n_points, 1))
    I = np.zeros((n_points, 1))
    R = np.zeros((n_points, 1))
    
    S[0] = 0.5
    I[0] = 0.3
    R[0] = 0.2
    beta = 0.5 ** alpha
    gamma = 0.2 ** alpha
    
    for k in range(n_points - 1):
        S[k + 1] += gl(S, k + 1, alpha) + r(k + 1, alpha) * S[0]
        S[k + 1] /= 1 + phi(step_size) ** alpha * beta * I[k]
        
        I[k + 1] += gl(I, k + 1, alpha) + r(k + 1, alpha) * I[0]
        I[k + 1] += phi(step_size) ** alpha * beta * S[k + 1] * I[k]
        I[k + 1] /= 1 + phi(step_size) ** alpha * gamma
        
        R[k + 1] += gl(R, k + 1, alpha) + r(k + 1, alpha) * R[0]
        R[k + 1] += phi(step_size) ** alpha * gamma * I[k + 1]
        
    df = pd.DataFrame(index=np.linspace(0, end_t, n_points))
    df[f"S_h={step_size}"] = S
    df[f"I_h={step_size}"] = I
    df[f"R_h={step_size}"] = R
    df.index.name = "t"
    dfs.append(df)

df = reduce(lambda left, right: pd.merge(left, right, how="outer", left_index=True, right_index=True), dfs)
df.to_csv("../data/H5_NSGL_NF2_scen_2.csv")


"""
Experiment 3.
"""
end_t = 50
step_size = 0.05

# NSGL
dfs   = []
for alpha in [0.95, 0.9, 0.8, 0.7]:
    for epsilon in [0.1, 0.01, 0.001, -0.1, -0.01, -0.001]:
        n_points = int(end_t / step_size + 1)

        S = np.zeros((n_points, 1))
        I = np.zeros((n_points, 1))
        R = np.zeros((n_points, 1))

        S[0] = 0.7
        I[0] = 0.2
        R[0] = 0.1
        gamma = 0.4 ** alpha
        beta = (gamma / S[0]) + epsilon
        

        for k in range(n_points - 1):
            S[k + 1] += gl(S, k + 1, alpha) + r(k + 1, alpha) * S[0]
            S[k + 1] /= 1 + step_size ** alpha * beta * I[k]

            I[k + 1] += gl(I, k + 1, alpha) + r(k + 1, alpha) * I[0]
            I[k + 1] += step_size ** alpha * beta * S[k + 1] * I[k]
            I[k + 1] /= 1 + step_size ** alpha * gamma

            R[k + 1] += gl(R, k + 1, alpha) + r(k + 1, alpha) * R[0]
            R[k + 1] += step_size ** alpha * gamma * I[k + 1]

        df = pd.DataFrame(index=np.linspace(0, end_t, n_points))
        df[f"I_alpha={alpha}_perturbation={epsilon}"] = I
        df.index.name = "t"
        dfs.append(df)

df = reduce(lambda left, right: pd.merge(left, right, how="outer", left_index=True, right_index=True), dfs)
df.to_csv("../data/H5_NSGL_scen_3.csv")


"""
Experiment 4.
"""
end_t = 300
step_size = 0.5

# NSGL
dfs = []
for alpha in [0.95, 0.9, 0.8, 0.7]:
    for s0 in [0.95, 0.9, 0.8, 0.7]:
        n_points = int(end_t / step_size + 1)

        S = np.zeros((n_points, 1))
        I = np.zeros((n_points, 1))
        R = np.zeros((n_points, 1))

        S[0] = s0
        I[0] = 1 - s0
        R[0] = 0
        beta = 3 ** alpha
        gamma = 0.3 ** alpha

        for k in range(n_points - 1):
            S[k + 1] += gl(S, k + 1, alpha) + r(k + 1, alpha) * S[0]
            S[k + 1] /= 1 + step_size ** alpha * beta * I[k]

            I[k + 1] += gl(I, k + 1, alpha) + r(k + 1, alpha) * I[0]
            I[k + 1] += step_size ** alpha * beta * S[k + 1] * I[k]
            I[k + 1] /= 1 + step_size ** alpha * gamma

            R[k + 1] += gl(R, k + 1, alpha) + r(k + 1, alpha) * R[0]
            R[k + 1] += step_size ** alpha * gamma * I[k + 1]

        df = pd.DataFrame(index=np.linspace(0, end_t, n_points))
        df[f"S_alpha={alpha}_S0={s0}"] = S
        df[f"I_alpha={alpha}_S0={s0}"] = I
        df.index.name = "t"
        dfs.append(df)

df = reduce(lambda left, right: pd.merge(left, right, how="outer", left_index=True, right_index=True), dfs)
df.to_csv("../data/H5_NSGL_scen_4.csv")


"""
Scenario 5.
"""
alphas = [0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99]
for alpha in alphas:
    print(alpha + 1/math.gamma(1-alpha))

end_t = 1
step_size = 0.005

dfs = []
for alpha in [0.99, 0.95, 0.9, 0.85, 0.75, 0.5, 0.25, 0.05, 0.01]:
    n_points = int(end_t / step_size + 1)
    
    N = np.zeros((n_points, 1))
    
    N[0] = 1
    
    for k in range(n_points - 1):
        N[k + 1] = gl(N, k + 1, alpha) + r(k + 1, alpha)
        
    df = pd.DataFrame(index=np.linspace(0, end_t, n_points))
    df[f"alpha={alpha}"] = N
    df.index.name = "t"
    dfs.append(df)

df = reduce(lambda left, right: pd.merge(left, right, how="outer", left_index=True, right_index=True), dfs)
df.to_csv("../data/H5_NSGL_scen_5_1.csv")

alpha = 0.5
dfs = []
for step_size in [0.005, 0.005 / 4, 0.005 / 4**2, 0.005 / 4**3]:
    n_points = int(end_t / step_size + 1)
    
    N = np.zeros((n_points, 1))
    
    N[0] = 1
    
    for k in range(n_points - 1):
        N[k + 1] = gl(N, k + 1, alpha) + r(k + 1, alpha)
        
    df = pd.DataFrame(index=np.linspace(0, end_t, n_points))
    df[f"h={step_size}"] = N
    df.index.name = "t"
    dfs.append(df)

df = reduce(lambda left, right: pd.merge(left, right, how="outer", left_index=True, right_index=True), dfs)
df.to_csv("../data/H5_NSGL_scen_5_2.csv")
