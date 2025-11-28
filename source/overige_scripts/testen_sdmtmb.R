# Testen sdmTMB----

data <- mi_data_analyse %>%
  mutate(
    X = st_coordinates(.)[,1]/1000,
    Y = st_coordinates(.)[,2]/1000
  ) %>%
  st_drop_geometry()

inv_dist <- as.matrix(dist(data[, c("X", "Y")]))
diag(inv_dist) <- 0
Moran.I(data$mmif, inv_dist, na.rm = T)

mesh <- make_mesh(data, xy_cols = c("X", "Y"), n_knots = 100)
plot(mesh)
fit <- sdmTMB(formula = mmif ~ s(EKC_hydromorf, k=3) + jaar + groep + statuut + (1 | meetplaats),
              data = data %>%
                mutate(meetplaats = as.factor(meetplaats)) %>%
                na.omit(),
              mesh = mesh,
              spatial = "off")
r.squaredGLMM(fit)
dharma_residuals(object = fit, simu)
