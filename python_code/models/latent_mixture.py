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
# 2) Modelo JAGS (Latente ITCH + DD + TRADE)
# ----------------------------------------------------------
latent_model = """
model {
  prec <- 1/(5^2)

  # Mezcla de modelos
  phi_z ~ ddirich(rep(1,3))
  t_epsilon_trade ~ dlnorm(0,prec)

  for (i in 1:n_sub) {
    z[i] ~ dcat(phi_z)

    # ITCH
    beta_1[i]   ~ dnorm(0,prec)
    beta_x_A[i] ~ dnorm(0,prec)
    beta_x_R[i] ~ dnorm(0,prec)
    beta_t_A[i] ~ dnorm(0,prec)
    beta_t_R[i] ~ dnorm(0,prec)

    # DD
    sigma[i]  ~ dnorm(0,prec)T(0,)
    weight[i] ~ dnorm(0,prec)
    delta[i]  ~ dnorm(0,prec)

    # TRADE
    t_gamma[i]    ~ dlnorm(0,prec)
    t_tau[i]      ~ dlnorm(0,prec)
    t_kappa[i]    ~ dlnorm(0,prec)
    t_vartheta[i] ~ dlnorm(1,prec)

    for (j in 1:n_que) {
      # ITCH
      dif_x_A[i,j] <- beta_x_A[i]*(x_ll[j]-x_ss[j])
      dif_x_R[i,j] <- beta_x_R[i]*((x_ll[j]-x_ss[j])/(0.5*(x_ll[j]+x_ss[j])))
      dif_t_A[i,j] <- beta_t_A[i]*(t_ll[j]-t_ss[j])
      dif_t_R[i,j] <- beta_t_R[i]*((t_ll[j]-t_ss[j])/(0.5*(t_ll[j]+t_ss[j])))
      y_itch[i,j] <- beta_1[i]+dif_x_A[i,j]+dif_x_R[i,j]+dif_t_A[i,j]+dif_t_R[i,j]
      theta[i,j,1] <- phi(y_itch[i,j])

      # DD
      dd_x_R[i,j] <- (weight[i])*(x_ll[j]-x_ss[j])
      dd_t_R[i,j] <- (1-weight[i])*(t_ll[j]-t_ss[j])
      dif[i,j] <- dd_x_R[i,j]-dd_t_R[i,j]
      y_dd[i,j] <- (dif[i,j]-delta[i])/sigma[i]
      theta[i,j,2] <- phi(y_dd[i,j])

      # TRADE
      v_ll[i,j] <- (1/t_gamma[i])*log(1+t_gamma[i]*x_ll[j])        
      v_ss[i,j] <- (1/t_gamma[i])*log(1+t_gamma[i]*x_ss[j])
      w_ll[i,j] <- (1/t_tau[i])*log(1+t_tau[i]*t_ll[j])
      w_ss[i,j] <- (1/t_tau[i])*log(1+t_tau[i]*t_ss[j])
      q_value[i,j] <- v_ll[i,j]-v_ss[i,j]
      q_time[i,j]  <- (t_kappa[i])*log(1+((w_ll[i,j]-w_ss[i,j])/t_vartheta[i])^t_vartheta[i])
      theta[i,j,3] <- (q_value[i,j]^(1/t_epsilon_trade)) /
                      ((q_value[i,j]^(1/t_epsilon_trade))+(q_time[i,j]^(1/t_epsilon_trade)))

      for (r in 1:n_rep) {
        t_choice[i,j,r] ~ dbern(theta[i,j,z[i]])
        pred_t_choice[i,j,r] ~ dbern(theta[i,j,z[i]])
      }
    }
  }
}
"""

# ----------------------------------------------------------
# 3) Valores iniciales
# ----------------------------------------------------------
inits = {
    # ITCH
    "beta_1":   [0.2]  * n_sub,
    "beta_x_A": [0.01] * n_sub,
    "beta_x_R": [0.02] * n_sub,
    "beta_t_A": [0.03] * n_sub,
    "beta_t_R": [0.03] * n_sub,
    # DD
    "sigma": [4.0] * n_sub,
    "delta": [2.0] * n_sub,
    "weight": [1.0] * n_sub,
    # TRADE
    "t_kappa":    [0.5] * n_sub,
    "t_vartheta": [1.5] * n_sub,
    "t_gamma":    [0.9] * n_sub,
    "t_tau":      [0.3] * n_sub,
    "t_epsilon_trade": 0.1
}

# ----------------------------------------------------------
# 4) Correr el modelo
# ----------------------------------------------------------
model = pyjags.Model(
    code=latent_model,
    data=data,
    init=inits,
    chains=2,
    adapt=1000
)

samples = model.sample(
    5000,
    vars=[
        "z", "theta", "pred_t_choice",
        "sigma", "weight", "delta",
        "beta_x_A", "beta_x_R", "beta_t_A", "beta_t_R",
        "t_gamma", "t_tau", "t_vartheta", "t_kappa"
    ],
    thin=2
)

# ----------------------------------------------------------
# 5) Guardar resultados
# ----------------------------------------------------------
os.makedirs("outputs", exist_ok=True)

np.savez_compressed(
    "outputs/latent_samples.npz",
    **samples
)

print("Resultados guardados en outputs/latent_samples.npz")

