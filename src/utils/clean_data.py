import numpy as np
import pandas as pd

# --- General specifications ---
n_sub = 25   # i
n_que = 24   # j
n_rep = 10   # k

# --- Upload Data ---
dire_t = [f"data/sujeto_{i}_tiempo.csv" for i in range(1, n_sub+1)]
csvs_time = [pd.read_csv(path) for path in dire_t]

# --- Order raw data ---
t_choice = np.empty((n_sub, n_que, n_rep), dtype=int)

for i in range(n_sub):
    for j in range(1, n_que+1):
        subset = csvs_time[i].loc[csvs_time[i]["pair"] == j, ["biggerchosen"]]
        vals = subset["biggerchosen"].values
        if len(vals) != n_rep:
            raise ValueError(f"Sujeto {i+1}, pregunta {j} no tiene {n_rep} repeticiones.")
        t_choice[i, j-1, :] = vals

# --- Question specifications ---
x_ss = np.array([5150,5300,5450,5150,5300,5150,
                 6050,6200,6350,6050,6200,6050,
                 5150,5600,6050,5150,5600,5150,
                 1150,1250,1350,1150,1250,1150])

x_ll = np.array([5300,5450,5600,5450,5600,5600,
                 6200,6350,6500,6350,6500,6500,
                 5600,6050,6500,6050,6500,6500,
                 1250,1350,1450,1350,1450,1450])

t_ss = np.array([1,2,3,1,2,1,
                 7,8,9,7,8,7,
                 1,4,7,1,4,1,
                 1,4,7,1,4,1])

t_ll = np.array([2,3,4,3,4,4,
                 8,9,10,9,10,10,
                 4,7,10,7,10,10,
                 4,7,10,7,10,10])

# --- Guardar todo en un archivo NPZ ---
np.savez(
    "data/datos_preparados.npz",
    n_sub=n_sub,
    n_que=n_que,
    n_rep=n_rep,
    t_choice=t_choice,
    x_ss=x_ss,
    x_ll=x_ll,
    t_ss=t_ss,
    t_ll=t_ll
)

print("Archivo 'datos_preparados.npz'")

