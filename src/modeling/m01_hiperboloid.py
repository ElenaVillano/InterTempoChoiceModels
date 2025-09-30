import numpy as np
import pyjags

# ----------------------------------------------------------
# 1) Cargar los datos preparados
# ----------------------------------------------------------
data_npz = np.load("data/datos_preparados.npz", allow_pickle=True)

n_sub = int(data_npz["n_sub"])
n_que = int(data_npz["n_que"])
n_rep = int(data_npz["n_rep"])
t_choice = data_npz["t_choice"].astype(int)
x_ss = data_npz["x_ss"].tolist()
x_ll = data_npz["x_ll"].tolist()
t_ss = data_npz["t_ss"].tolist()
t_ll = data_npz["t_ll"].tolist()

# Empaquetar datos en el formato que espera JAGS
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
# 2) Modelo en sintaxis JAGS
# ----------------------------------------------------------
hyperboloid_model = """
model {
  prec <- 1/(5^2)
  epsilon ~ dnorm(0,prec)T(0,)

  for (i in 1:n_sub) {
    kappa[i] ~ dnorm(0,prec)T(0,)
    tau[i]   ~ dnorm(0,prec)

    for (j in 1:n_que) {
      value_ll[i,j] <- x_ll[j]/(1+kappa[i]*t_ll[j])^tau[i]
      value_ss[i,j] <- x_ss[j]/(1+kappa[i]*t_ss[j])^tau[i]

      theta[i,j] <- (value_ll[i,j]^(1/epsilon)) /
                    ((value_ll[i,j]^(1/epsilon))+(value_ss[i,j]^(1/epsilon)))

      for (h in 1:n_rep) {
        t_choice[i,j,h] ~ dbern(theta[i,j])
      }
    }
  }
}
"""

# ----------------------------------------------------------
# 3) Iniciales (análogas a R)
# ----------------------------------------------------------
inits = {
    "epsilon": 4.0,
    "tau": [2.0] * n_sub,
    "kappa": [1.0] * n_sub
}

# ----------------------------------------------------------
# 4) Correr el modelo en pyjags
# ----------------------------------------------------------
model = pyjags.Model(
    code=hyperboloid_model,
    data=data,
    init=inits,
    chains=2,
    adapt=1000
)

# Equivalente a iteraciones con burn-in y thinning
# Nota: JAGS en R usaba 1.5M iter, 900k burn-in, thin=200.
# Aquí usamos menos para prueba (ajusta según rendimiento).
samples = model.sample(
    5000,                   # número de iteraciones por cadena
    vars=["epsilon", "tau", "kappa", "theta"],
    thin=2
)


# ----------------------------------------------------------
# 5) Guardar resultados
# ----------------------------------------------------------
# Guardar en formato .npz para recargar en otro script
np.savez_compressed(
    "outputs/hyperboloid_samples.npz",
    **samples
)

print("Resultados guardados en outputs/hyperboloid_samples.npz")

