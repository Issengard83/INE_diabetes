library(tidyverse)
library(readxl)
library(nortest)
library(dlookr)
library(rstatix)
library(ggpubr)

datos <- read_excel("DATOS DBT- PAMI - 26-4-2023.xlsx", sheet = "base valorizada")

datos |> drop_na(HbAc1_inicial, HbAc1_final) |> count(GRUPO)

datos_completos <- datos |> drop_na(HbAc1_inicial, HbAc1_final) |> 
  mutate(GRUPO = factor(GRUPO))


lillie.test(datos_completos$HbAc1_inicial)
lillie.test(datos_completos$HbAc1_final)

plot_normality(datos_completos, HbAc1_inicial, HbAc1_final)

plot_outlier(datos_completos, HbAc1_inicial, HbAc1_final)

## antes - despues

datos_completos <- datos_completos |>  
  pivot_longer(cols = c(HbAc1_inicial, HbAc1_final),
               names_to = c("x", "momento"), 
               names_sep = "\\_",
               values_to = "HbAc1") |> 
  select(-x) |> mutate(momento = factor(momento, levels = c("inicial", "final")))

datos_completos %>%
  group_by(GRUPO, momento) %>%
  get_summary_stats(HbAc1, type = "median_iqr")

datos_completos |> filter(GRUPO == 1) |> 
  ggpaired(x = "momento", y = "HbAc1", 
         order = c("inicial", "final"),
         ylab = "HbA1c", xlab = "Momento")

datos_completos |> filter(GRUPO == 2) |> 
  ggpaired(x = "momento", y = "HbAc1", 
         order = c("inicial", "final"),
         ylab = "HbA1c", xlab = "Momento")

datos_completos |> filter(GRUPO == 1) |> 
  wilcox_test(HbAc1 ~ momento, paired = T) |> 
  add_significance()

datos_completos |> filter(GRUPO == 1) |> 
  wilcox_effsize(HbAc1 ~ momento, paired = T) 

datos_completos |> filter(GRUPO == 2) |> 
  wilcox_test(HbAc1 ~ momento, paired = T) |> 
  add_significance()

datos_completos |> filter(GRUPO == 2) |> 
  wilcox_effsize(HbAc1 ~ momento, paired = T) 

### comparacion inicial

datos_completos |> filter(momento == "inicial") |> 
  ggplot(aes(x = GRUPO, y = HbAc1)) +
  geom_boxplot(outlier.alpha = 0) +
  geom_jitter(width = 0.2)

datos_completos |> filter(momento == "inicial") |> 
  wilcox_test(HbAc1 ~ GRUPO) |> 
  add_significance()

## comparación final

datos_completos |> filter(momento == "final") |> 
  ggplot(aes(x = GRUPO, y = HbAc1)) +
  geom_boxplot(outlier.alpha = 0) +
  geom_jitter(width = 0.2)

datos_completos |> filter(momento == "final") |> 
  wilcox_test(HbAc1 ~ GRUPO) |> 
  add_significance()


## buen y mal control

datos_completos <- datos_completos |>  
  mutate(Control_glucemico = if_else(HbAc1 > 7, "mal", "buen"))

datos_completos |>  filter(GRUPO == 1) |> 
  pairwise_mcnemar_test(Control_glucemico ~ momento|ID)

datos_completos |>  filter(GRUPO == 2) |> 
  pairwise_mcnemar_test(Control_glucemico ~ momento|ID)

tabla_inicial <- table(datos_completos$GRUPO[datos_completos$momento == "inicial"],
                       datos_completos$Control_glucemico[datos_completos$momento == "inicial"])
  
chisq_test(tabla_inicial)

tabla_final <- table(datos_completos$GRUPO[datos_completos$momento == "final"],
                       datos_completos$Control_glucemico[datos_completos$momento == "final"])

chisq_test(tabla_final)
