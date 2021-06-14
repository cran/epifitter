## ----message=FALSE, warning=FALSE---------------------------------------------
library(epifitter)
library(ggplot2)
library(dplyr)
library(magrittr)
library(cowplot)

## -----------------------------------------------------------------------------
dpcL <- sim_logistic(
  N = 100, # duration of the epidemics in days
  y0 = 0.01, # disease intensity at time zero
  dt = 10, # interval between assessments
  r = 0.1, # apparent infection rate
  alpha = 0.2, # level of noise
  n = 7 # number of replicates
)

## -----------------------------------------------------------------------------
head(dpcL)

## -----------------------------------------------------------------------------
ggplot(
  dpcL,
  aes(time, y,
    group = replicates
  )
) +
  geom_point(aes(time, random_y), shape = 1) + # plot the replicate values
  geom_point(color = "steelblue", size = 2) +
  geom_line(color = "steelblue") +
  labs(
    title = "Simulated 'complete' epidemics of sigmoid shape",
    subtitle = "Produced using sim_logistic()"
  )+
  theme_minimal_hgrid()

## -----------------------------------------------------------------------------
f_lin <- fit_lin(
  time = dpcL$time,  
  y = dpcL$random_y
)


## -----------------------------------------------------------------------------
f_lin

## -----------------------------------------------------------------------------
f_lin$stats_all

## -----------------------------------------------------------------------------
head(f_lin$data)

## -----------------------------------------------------------------------------
plot_lin <- plot_fit(f_lin,
  point_size = 2,
  line_size = 1
) 

# Default plots
plot_lin 

## -----------------------------------------------------------------------------
# Customized plots

plot_fit(f_lin,
  point_size = 2,
  line_size = 1,
  models = "Logistic")+
  theme_minimal_hgrid(font_size =18) +
  scale_x_continuous(limits = c(0,100))+
  scale_color_grey()+
   theme(legend.position = "none")+
  labs(
    x = "Time",
    y = "Proportion disease "
    
  )

## ----message=FALSE, warning=FALSE---------------------------------------------
f_nlin <- fit_nlin(
  time = dpcL$time,
  y = dpcL$random_y,
  starting_par = list(y0 = 0.01, r = 0.03)
)

f_nlin

## -----------------------------------------------------------------------------
plot_fit(f_nlin) +
  theme_minimal_hgrid()#changing plot theme

## -----------------------------------------------------------------------------

dpcL2 = dpcL %>% 
  mutate(random_y = random_y * 0.8)

## ----message=FALSE, warning=FALSE---------------------------------------------
f_nlin2 <- fit_nlin2(
  time = dpcL2$time,
  y = dpcL2$random_y,
  starting_par = list(y0 = 0.01, r = 0.2, K =  0.6)
)
f_nlin2
plot_fit(f_nlin2)

## -----------------------------------------------------------------------------
epi1 <- sim_gompertz(N = 60, y0 = 0.001, dt = 5, r = 0.1, alpha = 0.4, n = 4)
epi2 <- sim_gompertz(N = 60, y0 = 0.001, dt = 5, r = 0.12, alpha = 0.4, n = 4)
epi3 <- sim_gompertz(N = 60, y0 = 0.003, dt = 5, r = 0.14, alpha = 0.4, n = 4)

multi_epidemic <- bind_rows(epi1,
  epi2,
  epi3,
  .id = "DPC"
)
head(multi_epidemic)

## -----------------------------------------------------------------------------

p_multi <- ggplot(multi_epidemic,
       aes(time, y, shape = DPC, group = DPC))+
  geom_point(size =2)+
  geom_line()+
  theme_minimal_grid(font_size =18) +
   labs(
    x = "Time",
    y = "Proportion disease "
    
  )
p_multi

## ----fig.height=10, fig.width=6-----------------------------------------------
p_multi +
  facet_wrap(~ DPC, ncol = 1)

## -----------------------------------------------------------------------------
multi_fit <- fit_multi(
  time_col = "time",
  intensity_col = "random_y",
  data = multi_epidemic,
  strata_cols = "DPC"
)

## -----------------------------------------------------------------------------
head(multi_fit$Parameters)

## -----------------------------------------------------------------------------
head(multi_fit$Data)

## -----------------------------------------------------------------------------
multi_fit2 <- fit_multi(
  time_col = "time",
  intensity_col = "random_y",
  data = multi_epidemic,
  strata_cols = "DPC",
  nlin = TRUE)
head(multi_fit2$Parameters)

## -----------------------------------------------------------------------------
multi_fit_K <- fit_multi(
  time_col = "time",
  intensity_col = "random_y",
  data = multi_epidemic,
  strata_cols = "DPC",
  nlin = T,
  estimate_K = T
)

## -----------------------------------------------------------------------------
head(multi_fit_K$Parameters)

## -----------------------------------------------------------------------------
multi_fit$Data %>%
  ggplot(aes(time, predicted, color = DPC)) +
  geom_point(aes(time, y), color = "gray") +
  geom_line(size = 1) +
  facet_grid(DPC ~ model, scales = "free_y") +
  theme_minimal_hgrid()+
  coord_cartesian(ylim = c(0, 1))

## -----------------------------------------------------------------------------
multi_fit$Data %>%
  filter(model == "Gompertz") %>%
  ggplot(aes(time, predicted, color = DPC)) +
  geom_point(aes(time, y),
    color = "gray",
    size = 2
  ) +
  geom_line(size = 1.2) +
  theme_minimal_hgrid() +
  labs(
    x = "Time",
    y = "Disease Intensity"
  )

## -----------------------------------------------------------------------------
multi_fit$Parameters %>%
  filter(model == "Gompertz") %>%
  ggplot(aes(DPC, r)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = r_ci_lwr, ymax = r_ci_upr),
    width = 0,
    size = 1
  ) +
  labs(
    x = "Time",
    y = "Apparent infection rate"
  ) +
  theme_minimal_hgrid()

