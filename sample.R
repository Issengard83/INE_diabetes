#### LOAD PACKAGES ####
install.load::install_load("pwr","tidyverse")

# Tamaño muestral calculado----
# Power: 90%, IC: 95%, Diff. medias: 0.5, SD: 1.4
pwr.t.test(n = NULL, d = .5/1.4, power = .9, sig.level = .05, type = "two.sample")

# Potencia para el N muestreado (media y sd teóricas)
pwr.t.test(n = 104, d = .5/1.4, power = NULL, sig.level = .05, type = "two.sample")
