#' ---
#' title: ""
#' author: ""
#' output: word_document
#' ---

#### CARGA PAQUETES ####
install.load::install_load("tidyverse","readxl","compareGroups","nortest","dlookr",
                           "glmmTMB","visreg","DHARMa")
#### CARGA DATOS ####
DF = read_excel("datos_dbt_limpios.xlsx") %>% 
  filter(!is.na(HbAc1_inicial) & !is.na(HbAc1_final)) %>% 
  # Modifica niveles variables
  mutate(
    Nivel_ed = case_when(Max_nivel_ed==2 ~"Primario",
                         Max_nivel_ed %in% c(3:4) ~ "Secundario/Universitario",
                         TRUE ~ "Sin instrucción/Especial") %>% 
      fct_relevel("Sin instrucción/Especial", after = Inf),
    Drogas_hpo = case_when(N_drogas_hpo>1 ~ "2 o más drogas",
                           N_drogas_hpo==1 ~ "1 droga",
                           TRUE ~ "Ninguna"),
    # Hemoglobina glicosilada final
    HbAc1_final_mod = if_else(HbAc1_final<=5, 8.3, HbAc1_final),
    # Crea variables respuesta binomiales
    Control_gluc_cat1 = if_else(HbAc1_final<=5|HbAc1_final>7, 0, 1),
    Control_gluc_cat2 = if_else(HbAc1_final<=5|HbAc1_final>7.5, 0, 1),
    Control_gluc_cat3 = if_else(HbAc1_final<=5|HbAc1_final>8, 0, 1),
    Act_fis_final = if_else(Act_fisica_final=="No",0,1),
    Fuma_final2 = if_else(Fuma_final=="Si",1,0)) %>% 
  mutate_at(c("Sueldo","Max_nivel_ed"), as.factor) %>% 
  mutate_at("Rango_IMC", fct_relevel, "PSN", after=1) %>% 
  mutate_at("Sueldo_modif", fct_relevel, ">69000", after = Inf)

# Normalidad VR
lillie.test(DF$HbAc1_inicial)
lillie.test(DF$HbAc1_final_mod)

plot_normality(DF, HbAc1_inicial, HbAc1_final_mod)

plot_outlier(DF, HbAc1_inicial, HbAc1_final_mod)

#### Análisis para VR continua (HbAc1 final) ####
# Test de asociación (P<=0.10 entra al modelo de regresión)
with(DF, kruskal.test(HbAc1_final_mod, Grupo))

with(DF, kruskal.test(HbAc1_final_mod, Rango_edad))

with(DF, kruskal.test(HbAc1_final_mod, Sexo))

with(DF, kruskal.test(HbAc1_final_mod, Nivel_ed))

with(DF, kruskal.test(HbAc1_final_mod, Vive_solo))

with(DF, kruskal.test(HbAc1_final_mod, Viene_acomp))

with(DF, kruskal.test(HbAc1_final_mod, Sit_conyugal)) #P=0.062

with(DF, kruskal.test(HbAc1_final_mod, Rango_IMC))

with(DF, kruskal.test(HbAc1_final_mod, Act_fisica_inicial))

with(DF, kruskal.test(HbAc1_final_mod, Fuma_inicial)) # P=0.095

with(DF, kruskal.test(HbAc1_final_mod, HTA))

with(DF, kruskal.test(HbAc1_final_mod, ACV))

with(DF, kruskal.test(HbAc1_final_mod, Enf_coronaria))

with(DF, kruskal.test(HbAc1_final_mod, Tipo_tto))

with(DF, kruskal.test(HbAc1_final_mod, Insu_rapida))

with(DF, kruskal.test(HbAc1_final_mod, Drogas_hpo))

### Modelo regresión lineal----
DF1 = DF %>% filter(!is.na(Sit_conyugal) & !is.na(Fuma_inicial))

fit = glmmTMB(HbAc1_final_mod ~ Grupo + Sit_conyugal + Fuma_inicial + (1|Grupo),
              data = DF1)
summary(fit)

# Selección modelos
drop1(fit)
fit = update(fit, ~.-Grupo)

drop1(fit)
summary(fit)

# Test residuales
simulateResiduals(fit, plot = T)

# Visualiza regresión
visreg(fit, gg = T, scale = "response")

#### Análisis para VR binomial (control final) ####
### Tests de asociación (Control glucosa>7: malo)----
descrTable(Control_gluc_cat1 ~ Rango_edad + Sexo + Nivel_ed +
             Vive_solo + Viene_acomp + Sit_conyugal + Rango_IMC + Perim_abdominal_inicial +
             Act_fisica_inicial + Fuma_inicial + HTA + ACV + Enf_coronaria +
             Tipo_tto + Insu_rapida + Drogas_hpo, data = DF, method = 4)%>% 
  strataTable("Grupo")


### Tests de asociación (Control glucosa>7.5: malo)----
descrTable(Control_gluc_cat2 ~ Rango_edad + Sexo + Nivel_ed +
             Vive_solo + Viene_acomp + Sit_conyugal + Rango_IMC + Perim_abdominal_inicial +
             Act_fisica_inicial + Fuma_inicial + HTA + ACV + Enf_coronaria +
             Tipo_tto + Insu_rapida + Drogas_hpo, data = DF, method = 4) %>% 
  strataTable("Grupo")

### Tests de asociación (Control glucosa>8: malo)----
descrTable(Control_gluc_cat3 ~ Rango_edad + Sexo + Nivel_ed +
             Vive_solo + Viene_acomp + Sit_conyugal + Rango_IMC + Perim_abdominal_inicial +
             Act_fisica_inicial + Fuma_inicial + HTA + ACV + Enf_coronaria +
             Tipo_tto + Insu_rapida + Drogas_hpo, data = DF, method = 4) %>% 
  strataTable("Grupo")

#### Act. física al final del ensayo ####
### Tests asociación----
descrTable(Act_fisica_final ~ Grupo + Rango_edad + Sexo + Nivel_ed +
             Vive_solo + Viene_acomp + Sit_conyugal + Rango_IMC + Perim_abdominal_inicial +
             Act_fisica_inicial + Fuma_inicial + HTA + ACV + Enf_coronaria +
             Tipo_tto + Insu_rapida + Drogas_hpo, data = DF, method = 4)

### Modelo full----
DF1 = DF %>% filter(!is.na(Act_fisica_final) & !is.na(Enf_coronaria))

fit1 = glmmTMB(Act_fis_final ~ Grupo + Sexo + Enf_coronaria + Insu_rapida +
                 (1|Grupo), data = DF1, family = binomial)

summary(fit1)

# Selección modelos
drop1(fit1)
fit1 = update(fit1, ~.-Grupo)

drop1(fit1)
fit1 = update(fit1, ~.-Insu_rapida)

drop1(fit1)
fit1 = update(fit1, ~.-Enf_coronaria)
summary(fit1)

#### Fumador al final del ensayo ####
### Tests asociación----
descrTable(Fuma_final ~ Grupo + Rango_edad + Sexo + Nivel_ed +
             Vive_solo + Viene_acomp + Sit_conyugal + Rango_IMC + Perim_abdominal_inicial +
             Act_fisica_inicial + Fuma_inicial + HTA + ACV + Enf_coronaria +
             Tipo_tto + Insu_rapida + Drogas_hpo, data = DF, method = 4)

### Modelo full----
DF1 = DF %>% filter(!is.na(Fuma_final2) & !is.na(Drogas_hpo))

fit1 = glmmTMB(Fuma_final2 ~ Grupo + Drogas_hpo +
                 (1|Grupo), data = DF1, family = binomial)

summary(fit1)

# Selección modelos
drop1(fit1)
fit1 = update(fit1, ~.-Grupo)


drop1(fit1)
summary(fit1)
