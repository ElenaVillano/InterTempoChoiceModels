import matplotlib.pyplot as plt

def plot_trace_grid(idata, var_name, ncols=5, colors=['orange','C1']):
    """
    Dibuja un grid de trazas por sujeto para un parámetro vectorial.

    Parameters
    ----------
    idata : arviz.InferenceData
        Resultados del muestreo en formato ArviZ.
    var_name : str
        Nombre del parámetro a graficar (ej. "tau", "kappa").
    ncols : int, default=5
        Número de columnas en el grid.
    colors : list, default=None
        Lista de colores para las cadenas. Si None, usa la paleta por defecto.
    """
    values = idata.posterior[var_name].values  # shape (chains, draws, subjects)
    n_chains, n_draws, n_subjects = values.shape
    
    nrows = int(np.ceil(n_subjects / ncols))
    fig, axes = plt.subplots(nrows, ncols, figsize=(3*ncols, 2*nrows), sharex=True, sharey=True)

    if colors is None:
        colors = [f"C{i}" for i in range(n_chains)]

    axes = axes.flatten()
    for i in range(n_subjects):
        ax = axes[i]
        for c in range(n_chains):
            ax.plot(values[c, :, i], color=colors[c], alpha=0.7, lw=0.5, label=f"Cadena {c+1}")
        ax.set_title(f"{var_name}[{i}]")
        if i == 0:
            ax.legend(fontsize=8)
    # Apagar ejes vacíos
    for j in range(n_subjects, len(axes)):
        axes[j].axis("off")

    plt.show()


def plot_posterior_grid(idata, var_name, ncols=5, colors=None, kind="kde"):
    """
    Dibuja un grid de distribuciones posteriores por sujeto para un parámetro vectorial.

    Parameters
    ----------
    idata : arviz.InferenceData
        Resultados del muestreo en formato ArviZ.
    var_name : str
        Nombre del parámetro a graficar (ej. "tau", "kappa").
    ncols : int, default=5
        Número de columnas en el grid.
    colors : list, default=None
        Lista de colores para las cadenas. Si None, usa la paleta por defecto.
    kind : {"kde", "hist"}, default="kde"
        Tipo de gráfico: kde (curva de densidad) o hist (histograma).
    """
    values = idata.posterior[var_name].values  # shape (chains, draws, subjects)
    n_chains, n_draws, n_subjects = values.shape

    nrows = int(np.ceil(n_subjects / ncols))
    fig, axes = plt.subplots(nrows, ncols, figsize=(3*ncols, 2.5*nrows), sharex=True, sharey=True)

    if colors is None:
        colors = [f"C{i}" for i in range(n_chains)]

    axes = axes.flatten()
    for i in range(n_subjects):
        ax = axes[i]
        for c in range(n_chains):
            data = values[c, :, i]
            if kind == "kde":
                sns.kdeplot(data, ax=ax, color=colors[c], fill=True, alpha=0.4, lw=1, label=f"Cadena {c+1}")
            elif kind == "hist":
                ax.hist(data, bins=40, color=colors[c], alpha=0.5, label=f"Cadena {c+1}")
        ax.set_title(f"{var_name}[{i}]")
        if i == 0:
            ax.legend(fontsize=8)
    # Apagar ejes vacíos
    for j in range(n_subjects, len(axes)):
        axes[j].axis("off")

    plt.show()
