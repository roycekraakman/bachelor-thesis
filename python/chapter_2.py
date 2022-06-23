from functools import reduce
import pandas as pd
import numpy as np


def logistic_equation(y):
    return y * (1 - y)
def exact_solution(t, y0):
    return y0 / (y0 + np.exp(-t) - y0 * np.exp(-t))

y0 = 0.5


# Forward Euler Scheme
n_points = 50
dfs      = []

for step_size in [2.5, 1.5, 1, 0.5]:
    end_t = int(step_size * (n_points - 1))
    
    y = np.zeros((n_points, 1))
    y[0] = y0
    for k in range(n_points - 1):
        y[k + 1] = step_size * logistic_equation(y[k]) + y[k]
    
    df = pd.DataFrame(index=np.linspace(0, n_points - 1, n_points))
    df[f"h={step_size}"] = y
    df.index.name = "k"
    dfs.append(df)

df = reduce(lambda left, right: pd.merge(left, right, how="outer", left_index=True, right_index=True), dfs)
df.to_csv("../data/H2_logistic_equation_forward_euler.csv")


# Central Difference Scheme
n_points = 1500
dfs      = []

for step_size in [0.1, 0.05]:
    end_t = int(step_size * (n_points - 1))
    
    y = np.zeros((n_points, 1))
    y[0] = y0
    y[1] = y0 + step_size * logistic_equation(y[0])
    for k in range(1, n_points - 1):
        y[k + 1] = 2 * step_size * logistic_equation(y[k]) + y[k-1]
    
    df = pd.DataFrame(index=np.linspace(0, n_points - 1, n_points))
    df[f"h={step_size}"] = y
    df.index.name = "k"
    dfs.append(df)

df = reduce(lambda left, right: pd.merge(left, right, how="outer", left_index=True, right_index=True), dfs)
df.to_csv("../data/H2_logistic_equation_central_difference.csv")


# Exact Scheme
end_t = 5
dfs   = []

t = index=np.linspace(0, end_t, 1000)
df = pd.DataFrame(index=t)
df["exact"] = exact_solution(t, y0)
df.index.name = "t"
dfs.append(df)

for step_size in [5, 2.5, 1, 0.5]:
    n_points = int(end_t / step_size + 1)
    
    y = np.zeros((n_points, 1))
    y[0] = y0
    for k in range(n_points - 1):
        y[k + 1] = y[k] / (y[k] - y[k] * np.exp(-step_size) + np.exp(-step_size))
    
    df = pd.DataFrame(index=np.linspace(0, end_t, n_points))
    df[f"h={step_size}"] = y
    df.index.name = "t"
    dfs.append(df)

df = reduce(lambda left, right: pd.merge(left, right, how="outer", left_index=True, right_index=True), dfs)
df.to_csv("../data/H2_logistic_equation_exact.csv")


# Non-Local Central Difference Scheme
n_points = 1500
dfs      = []

for step_size in [0.1, 0.05]:
    end_t = int(step_size * (n_points - 1))
    
    y = np.zeros((n_points, 1));
    y[0] = y0
    y[1] = y0 + step_size * logistic_equation(y[0])
    for k in range(1, n_points - 1):
        y[k + 1] = (2 * step_size * y[k - 1] + y[k - 1]) / (1 + 2 * step_size * y[k - 1])
    
    df = pd.DataFrame(index=np.linspace(0, n_points - 1, n_points))
    df[f"h={step_size}"] = y
    df.index.name = "k"
    dfs.append(df)

df = reduce(lambda left, right: pd.merge(left, right, how="outer", left_index=True, right_index=True), dfs)
df.to_csv("../data/H2_logistic_equation_non_local_central_difference.csv")
