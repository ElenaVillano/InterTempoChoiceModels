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
# 2) Modelo JAGS (Proportional Differences)
# ----------------------------------------------------------
pd_model = """
model {
  prec <- 1/(5^2)

  for (i in 1:n_sub) {
    # Parámetros
    sigma[i] ~ dnorm(0, prec)T(0,)
    delta[i] ~ dnorm(0, prec)

    for (j in 1:n_que) {
      # Diferencias proporcionales
      dif_x_R[i,j] <- (max(x_ll[j], x_ss[j]) - min(x_ll[j], x_ss[j])) / max(x_ll[j], x_ss[j])
      dif_t_R[i,j] <- (max(t_ll[j], t_ss[j]) - min(t_ll[j], t_ss[j])) / max(t_ll[j], t_ss[j])

      # Regresión
      dif[i,j] <- dif_x_R[i,j] - dif_t_R[i,j]
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
    "sigma": [0.2] * n_sub,
    "delta": [0.01] * n_sub
}

# ----------------------------------------------------------
# 4) Correr el modelo
# ----------------------------------------------------------
model = pyjags.Model(
    code=pd_model,
    data=data,
    init=inits,
    chains=2,
    adapt=1000
)

samples = model.sample(
    5000,
    vars=["sigma", "delta", "theta"],
    thin=2
)

# ----------------------------------------------------------
# 5) Guardar resultados
# ----------------------------------------------------------
np.savez_compressed(
    "outputs/pd_samples.npz",
    **samples
)

print("Resultados guardados en outputs/pd_samples.npz")

