import numpy as np
import pyjags

# ----------------------------------------------------------
# 1) Cargar datos preparados
# ----------------------------------------------------------
data_npz = np.load("datos_preparados.npz", allow_pickle=True)

n_sub = int(data_npz["n_sub"])
n_que = int(data_npz["n_que"])
n_rep = int(data_npz["n_rep"])
t_choice = data_npz["t_choice"].astype(int)
x_ss = data_npz["x_ss"].tolist()
x_ll = data_npz["x_ll"].tolist()
t_ss = data_npz["t_ss"].tolist()
t_ll = data_npz["t_ll"].tolist()

data = {
    "n_sub": n_sub,
    "n_que": n_que,
    "n_rep": n_rep,
    "t_choice": t_choice,
    "x_ss": x_ss,
    "x_ll": x_ll,
    "t_ss": t_ss,
    "t_ll": t_ll
}

# ----------------------------------------------------------
# 2) Modelo JAGS (ITCH)
# ----------------------------------------------------------
itch_model = """
model {
  prec <- 1/(10^2)

  for (i in 1:n_sub) {
    # Parámetros
    beta_1[i]   ~ dnorm(0, prec)
    beta_x_A[i] ~ dnorm(0, prec)
    beta_x_R[i] ~ dnorm(0, prec)
    beta_t_A[i] ~ dnorm(0, prec)
    beta_t_R[i] ~ dnorm(0, prec)

    for (j in 1:n_que) {
      # Funciones
      dif_x_A[i,j] <- beta_x_A[i] * (x_ll[j] - x_ss[j])
      dif_x_R[i,j] <- beta_x_R[i] * ((x_ll[j] - x_ss[j]) / (0.5 * (x_ll[j] + x_ss[j])))
      dif_t_A[i,j] <- beta_t_A[i] * (t_ll[j] - t_ss[j])
      dif_t_R[i,j] <- beta_t_R[i] * ((t_ll[j] - t_ss[j]) / (0.5 * (t_ll[j] + t_ss[j])))

      # Regresión
      y[i,j] <- beta_1[i] + dif_x_A[i,j] + dif_x_R[i,j] + dif_t_A[i,j] + dif_t_R[i,j]

      # Probabilidad de elegir la opción diferida
      theta[i,j] <- phi(y[i,j])

      # Observaciones
      for (h in 1:n_rep) {
        t_choice[i,j,h] ~ dbern(theta[i,j])
      }
    }
  }
}
"""

# ----------------------------------------------------------
# 3) Valores iniciales
# ----------------------------------------------------------
inits = {
    "beta_1":   [0.2]  * n_sub,
    "beta_x_A": [0.01] * n_sub,
    "beta_x_R": [0.02] * n_sub,
    "beta_t_A": [0.03] * n_sub,
    "beta_t_R": [0.03] * n_sub
}

# ----------------------------------------------------------
# 4) Correr el modelo
# ----------------------------------------------------------
model = pyjags.Model(
    code=itch_model,
    data=data,
    init=inits,
    chains=2,
    adapt=1000
)

samples = model.sample(
    5000,
    vars=["beta_1", "beta_x_A", "beta_x_R", "beta_t_A", "beta_t_R", "theta"],
    thin=2
)

# ----------------------------------------------------------
# 5) Guardar resultados
# ----------------------------------------------------------
np.savez_compressed(
    "outputs/itch_samples.npz",
    **samples
)

print("Resultados guardados en outputs/itch_samples.npz")

