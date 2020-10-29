## ----message=FALSE, warning=FALSE---------------------------------------------
library(epifitter)
library(magrittr)
library(ggplot2)
library(cowplot)

## -----------------------------------------------------------------------------
exp_model <- sim_exponential(
  N = 100,    # total time units 
  y0 = 0.01,  # initial inoculum
  dt = 10,    #  interval between assessments in time units
  r = 0.045,  #  apparent infection rate
  alpha = 0.2,# level of noise
  n = 7       # number of replicates
)
head(exp_model)

## -----------------------------------------------------------------------------
exp_plot = exp_model %>%
  ggplot(aes(time, y)) +
  geom_jitter(aes(time, random_y), size = 3,color = "gray", width = .1) +
  geom_line(size = 1) +
  theme_minimal_hgrid() +
  ylim(0,1)+
  labs(
    title = "Exponential",
    y = "Disease intensity",
    x = "Time"
  )
exp_plot

## -----------------------------------------------------------------------------
mono_model <- sim_monomolecular(
  N = 100,
  y0 = 0.01,
  dt = 5,
  r = 0.05,
  alpha = 0.2,
  n = 7
)
head(mono_model)

## -----------------------------------------------------------------------------
mono_plot = mono_model %>%
  ggplot(aes(time, y)) +
  geom_jitter(aes(time, random_y), size = 3, color = "gray", width = .1) +
  geom_line(size = 1) +
  theme_minimal_hgrid() +
  labs(
    title = "Monomolecular",
    y = "Disease intensity",
    x = "Time"
  )
mono_plot

## -----------------------------------------------------------------------------
logist_model <- sim_logistic(
  N = 100,
  y0 = 0.01,
  dt = 5,
  r = 0.1,
  alpha = 0.2,
  n = 7
)
head(logist_model)

## -----------------------------------------------------------------------------
logist_plot = logist_model %>%
  ggplot(aes(time, y)) +
  geom_jitter(aes(time, random_y), size = 3,color = "gray", width = .1) +
  geom_line(size = 1) +
  theme_minimal_hgrid() +
  labs(
    title = "Logistic",
    y = "Disease intensity",
    x = "Time"
  )
logist_plot

## -----------------------------------------------------------------------------
gomp_model <- sim_gompertz(
  N = 100,
  y0 = 0.01,
  dt = 5,
  r = 0.07,
  alpha = 0.2,
  n = 7
)
head(gomp_model)

## -----------------------------------------------------------------------------
gomp_plot = gomp_model %>%
  ggplot(aes(time, y)) +
  geom_jitter(aes(time, random_y), size = 3,color = "gray", width = .1) +
  geom_line(size = 1) +
  theme_minimal_hgrid() +
  labs(
    title = "Gompertz",
    y = "Disease intensity",
    x = "Time"
  )
gomp_plot

## ----fig.height=6, fig.width=8------------------------------------------------
plot_grid(exp_plot,
          mono_plot,
          logist_plot,
          gomp_plot)

