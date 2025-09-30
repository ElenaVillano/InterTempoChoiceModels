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
# 2) Modelo JAGS (Direct Differences)
# ----------------------------------------------------------
dd_model = """
model {
  prec <- 1/(5^2)

  for (i in 1:n_sub) {
    # Parámetros
    sigma[i]  ~ dnorm(0, prec)T(0,)
    weight[i] ~ dnorm(0, prec)
    delta[i]  ~ dnorm(0, prec)

    for (j in 1:n_que) {
      # Funciones
      dif_x_R[i,j] <- (weight[i]) * (x_ll[j] - x_ss[j])
      dif_t_R[i,j] <- (1 - weight[i]) * (t_ll[j] - t_ss[j])

      # Diferencia
      dif[i,j] <- dif_x_R[i,j] - dif_t_R[i,j]

      # Probit
      y[i,j] <- (dif[i,j] - delta[i]) / sigma[i]

      # Probabilidad
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
    "sigma": [4.0] * n_sub,
    "delta": [2.0] * n_sub,
    "weight": [1.0] * n_sub
}

# ----------------------------------------------------------
# 4) Correr el modelo
# ----------------------------------------------------------
model = pyjags.Model(
    code=dd_model,
    data=data,
    init=inits,
    chains=2,
    adapt=1000
)

samples = model.sample(
    5000,
    vars=["sigma", "theta", "weight", "delta"],
    thin=2
)

# ----------------------------------------------------------
# 5) Guardar resultados
# ----------------------------------------------------------
np.savez_compressed(
    "outputs/dd_samples.npz",
    **samples
)

print("Resultados guardados en outputs/dd_samples.npz")

