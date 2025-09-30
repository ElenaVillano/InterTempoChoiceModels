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
# 2) Modelo JAGS (Trade-off)
# ----------------------------------------------------------
trade_model = """
model {
  prec <- 1/(5^2)
  t_epsilon_trade ~ dlnorm(0, prec)

  for (i in 1:n_sub) {
    # Parámetros
    t_gamma[i]    ~ dlnorm(0, prec)
    t_tau[i]      ~ dlnorm(0, prec)
    t_kappa[i]    ~ dlnorm(0, prec)
    t_vartheta[i] ~ dlnorm(1, prec)

    for (j in 1:n_que) {
      # Utilidad de recompensa
      v_ll[i,j] <- (1/t_gamma[i]) * log(1 + t_gamma[i] * x_ll[j])
      v_ss[i,j] <- (1/t_gamma[i]) * log(1 + t_gamma[i] * x_ss[j])

      # Utilidad de tiempo
      w_ll[i,j] <- (1/t_tau[i]) * log(1 + t_tau[i] * t_ll[j])
      w_ss[i,j] <- (1/t_tau[i]) * log(1 + t_tau[i] * t_ss[j])

      q_value[i,j] <- v_ll[i,j] - v_ss[i,j]
      q_time[i,j]  <- (t_kappa[i]) * log(1 + ((w_ll[i,j] - w_ss[i,j]) / t_vartheta[i])^t_vartheta[i])

      # Probabilidad de elegir la opción diferida
      theta[i,j] <- (q_value[i,j]^(1/t_epsilon_trade)) /
                    ((q_value[i,j]^(1/t_epsilon_trade)) + (q_time[i,j]^(1/t_epsilon_trade)))

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
    "t_kappa": [0.5] * n_sub,
    "t_vartheta": [1.5] * n_sub,
    "t_gamma": [0.9] * n_sub,
    "t_tau": [0.3] * n_sub,
    "t_epsilon_trade": 0.1
}

# ----------------------------------------------------------
# 4) Correr el modelo en PyJAGS
# ----------------------------------------------------------
model = pyjags.Model(
    code=trade_model,
    data=data,
    init=inits,
    chains=2,
    adapt=1000
)

samples = model.sample(
    5000,
    vars=["t_gamma", "t_tau", "t_vartheta", "t_kappa", "theta", "t_epsilon_trade"],
    thin=2
)

# ----------------------------------------------------------
# 5) Guardar resultados
# ----------------------------------------------------------
np.savez_compressed(
    "outputs/tradeoff_samples.npz",
    **samples
)

print("Resultados guardados en outputs/tradeoff_samples.npz")

