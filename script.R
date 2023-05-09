#### CARGA PAQUETES ####
install.load::install_load("tidyverse","readxl","compareGroups","nortest","dlookr",
                           "rstatix","ggpubr","glmmTMB","visreg")
#### CARGA DATOS ####
DF = read_excel("datos_dbt_limpios.xlsx") %>% 
  filter(!is.na(HbAc1_inicial) & !is.na(HbAc1_final)) %>% 
  mutate(Drogas_hpo_cat = case_when(N_drogas_hpo>1 ~ "2 o más drogas",
                                  N_drogas_hpo==1 ~ "1 droga",
                                  TRUE ~ "Ninguna"),
         Nivel_ed = case_when(Max_nivel_ed==2 ~"Primario",
                              Max_nivel_ed==3 ~ "Secundario",
                              Max_nivel_ed==4 ~ "Terciario/Universitario",
                              TRUE ~ "Sin instrucción/Especial") %>% 
           fct_relevel("Sin instrucción/Especial", after = Inf),
         Nivel_ed2 = fct_collapse(Nivel_ed, "Secundario/Superior" = c("Secundario","Terciario/Universitario")),
         Control_glucem1 = if_else(HbAc1 > 7, "malo", "bueno"),
         ) %>% 
  mutate_at(c("Sueldo","Max_nivel_ed"), as.factor) %>% 
  mutate_at("Rango_IMC", fct_relevel, "PSN", after=1) %>% 
  mutate_at("Sueldo_modif", fct_relevel, ">69000", after = Inf)

# Normalidad VR
lillie.test(DF$HbAc1_inicial)
lillie.test(DF$HbAc1_final)

plot_normality(DF, HbAc1_inicial, HbAc1_final)

plot_outlier(DF, HbAc1_inicial, HbAc1_final)

# Tablas descriptivos----
descrTable(Grupo ~ Rango_edad + Sexo + Nivel_ed + Nivel_ed2 + Sueldo_modif + 
             Vive_solo + Viene_acomp + Sit_conyugal + Rango_IMC + Act_fisica_Inicial + Fuma +
             HTA + ACV + Enf_coronaria + Tipo_tto + Insu_rapida +
             Biguadina + Sulfonilureas + Gliptinas + Drogas_hpo_cat + Perim_abdominal + 
             HbAc1_inicial + HbAc1_final, 
           data = DF, method = 4, byrow = F, show.all = T) #%>%
  # export2word("tab1.docx")

#### Base para el análisis----
DF1 = DF %>% pivot_longer(cols = c(HbAc1_inicial, HbAc1_final),
                          values_to = "HbAc1", names_to = "Etapa") %>% 
  mutate_at("Etapa", function(x){str_remove_all(x,".*\\_") %>% str_to_title %>% as_factor}) %>% 
  mutate_at("Grupo", as_factor)

# Exploración interacciones
descrTable(Nivel_ed ~ HbAc1_final, data = DF) %>% strataTable("Grupo")

descrTable(Nivel_ed2 ~ HbAc1_final, data = DF) %>% strataTable("Grupo")

descrTable(Rango_edad ~ HbAc1_final, data = DF) %>% strataTable("Grupo")

descrTable(Tipo_tto ~ HbAc1_final, data = DF) %>% strataTable("Grupo")

# Modelo regresión c/nivel educativo categorizado 1----
fit1 = glmmTMB(HbAc1 ~ Grupo + Nivel_ed + Rango_edad + Tipo_tto +
                 Grupo:Nivel_ed + (1|Etapa), data = DF1)
summary(fit1)

# Modelo regresión c/nivel educativo categorizado 2 (secundario y universitario juntos)----
fit2 = glmmTMB(HbAc1 ~ Grupo + Nivel_ed2 + Rango_edad + Tipo_tto +
                 Grupo:Nivel_ed2 + (1|Etapa), data = DF1)
summary(fit2)
drop1(fit2)

fit2.1 = update(fit2, ~.-Grupo:Nivel_ed2)
summary(fit2.1)
drop1(fit2.1)
