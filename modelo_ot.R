#####################################################################
## Descripcion: Modelos OTS
##
## Fecha: 2020-11-03
## Autor: GP
#####################################################################

library(dplyr, pos = 3)
library(broom, pos = 4)
library(modelr, pos = 5)
library(lubridate, pos = 6)
library(ggplot2, pos = 7)
library(MASS, pos = 900)
library(arm, pos = 901)
library(clipr)
library(readxl)
library(zoo)
library(janitor)
library(tidyr)
x <- dt_modelo_A$penetracion 


interpolacion <- function(x){

  n <- x %>% length()
  x <- x %>% na.approx()
  x[n] <- (x[n-1]-x[n-2])+x[n-1]
  
  return(x)
}

cambio_año <- function(x,año){
  year(x) <- año
  return(x)
}

###############################################################

## Seccion: funciones auxiliares

###############################################################

ajuste <- function(modelo) {
  modelo %>%
    broom::glance() %>%
    summarise(Ajuste = 1 - .$deviance / .$null.deviance)
}

colores <- c("Ajuste" = "#DF2A35", "Real" = "#172744")

dt <- readr::read_rds("cache/dt.rds")
dt$fecha <- dt$fecha %>% as.Date() 
macro <- c("gdp",	"tasa_de_inflacion","tasa_de_desempleo","confianza_del_consumidor",
           "poblacion","ingreso_real_disponible")

dt_ot <- dt %>% filter( !unidad %in% macro)

dt_macro <- dt %>% filter(fabricante=="A") %>% filter(unidad %in% macro|
                                                              unidad=="venta_en_unidades")

lista_fabricantes <- dt_ot %>%
  pivot_wider(names_from = unidad, values_from = valor) %>%
  #mutate(ppu = ventas_valor_col / venta_en_unidades) %>%
  # select(
  #   fecha, fabricante, venta_en_unidades, tdps, ppu
  # ) %>%
  split(.$fabricante)




###############################################################

## Seccion: modelo A

###############################################################

dt_modelo_A <- lista_fabricantes$A  %>% select_if(~sum(!is.na(.)) > 0) 
 
  # mutate(
  #   nov19 = factor(
  #     if_else(fecha == "2019-11-01" , 1, 0)
  #   )
  # ) %>%
  # identity()
# 2.1 actualización datos 2020 --------------------------------------------
names <- read_xlsx("data/Layout SOM OT Chile 2021VF II.xlsx",
               sheet="A Escenario I Full") %>% colnames()



nuevo <- read_xlsx("data/Layout SOM OT Chile 2021VF II.xlsx",sheet="A Escenario I Full") %>%
   mutate_at(vars(ppu),~./1000) %>% 
  mutate(venta_en_unidades=ventas/ppu) %>% select(-ventas) %>% 
  select(fecha, venta_en_unidades,everything()) %>% 
  filter(!is.na(venta_en_unidades)) %>% select(1:13) 



dt_modelo_A <- dt_modelo_A %>% select(c("venta_en_unidades",names[-c(2,14,15,16)])) %>% 
  filter(fecha<as.Date("2020-01-01")) %>%
  select(fecha, venta_en_unidades,everything()) %>% bind_rows(nuevo) %>% 
  mutate_at(vars(fecha),as.Date) %>% 
  mutate(
    nov19 = factor(
      if_else(fecha == "2019-11-01" , 1, 0)
    )
  ) 


dt_modelo_A$penetracion <- dt_modelo_A$penetracion %>% interpolacion()
dt_modelo_A$frecuencia <- dt_modelo_A$frecuencia %>% interpolacion()


# 2.1 Modelo --------------------------------------------------------------

fmla_A <-
  as.formula(
    log(venta_en_unidades) ~
      log(ppu) +
      log(td_ps) +
      #log(inventarios) +
 
      #log(gasto_por_acto)+
      log(frecuencia)+
      log(penetracion)+
      
      log(csd_price)+
      
      log(d_a)+
      log(listas_promocionales)+
      #log(number_tiendas)+
      #log(lista_de_proveedores_e_innovaciones+0.001)+
      log(otv_grps+0.001) +
      #log(ptv_grps+0.001) +
       #log(otv_investment+0.001)+
       log(ptv_investment+0.001)+######por que
       log(investment_secondary_media+0.001)+
       #log(digital_investment+0.001)+
       log(digital_impressions+0.001)+
      factor(month(fecha)) +
      nov19+
      NULL
  )

set.seed(42)

modelo_A <- bayesglm(
  fmla_A,
  data = dt_modelo_A,
  family = gaussian(link = "identity"),
  maxit = 1000,

  prior.mean = c(
    -0.5, # ppu
    0.5, # tdps
   # 1, # inventarios_unidades

    #0, # gasto_por_acto
    1, # frecuencia
    0, # penetracion
    0,  # csd_price
    0,  # d_a
    0,  # listas_promocionales
    #0,  # number_tiendar
    #0,  # lista_de_proveedores_e_innovaciones
    0,  # otv_grps
    #0,  # ptv_grps
     #0,  # otv_investment
     0,  # ptv_investment
     0,  # investment_secondary_media
     #0,  # digital_investment
     0,  # digital_impressions
    0, # febrero
    0, # marzo
    0, # abril
    0, # mayo
    0, # junio
    0, # julio
    0, # agosto
    0, # septiembre
    0, # octubre
    0, # noviembre
    0, # diciembre

     0, # nov19
    NULL
  ),
  prior.scale = c(
    1, # ppu
    1, # tdps
   # .01, # inventarios_unidades

    #1, # gasto_por_acto
    1, # frecuencia
    1, # penetracion
    1,  # csd_price
    1,  # d_a
    1,  # listas_promocionales
    #1,  # number_tiendar
    #1,  # lista_de_proveedores_e_innovaciones
    1,  # otv_grps
    #1,  # ptv_grps
     #1,  # otv_investment
     1,  # ptv_investment
     1,  # investment_secondary_media
     #1,  # digital_investment
     1,  # digital_impressions
    1, # febrero
    1, # marzo
    1, # abril
    1, # mayo
    1, # junio
    1, # julio
    1, # agosto
    1, # septiembre
    1, # octubre
    1, # noviembre
    1, # diciembre

    1, # nov19
    NULL
  ),
  prior.df = c(
    1, # ppu
    5, # tdps
   # 1, # inventarios_unidades

    #1, # gasto_por_acto
    1, # frecuencia
    1, # penetracion
    1,  # csd_price
    1,  # d_a
    1,  # listas_promocionales
    #1,  # number_tiendar
   # 1,  # lista_de_proveedores_e_innovaciones
    1,  # otv_grps
    #1,  # ptv_grps
     #1,  # otv_investment
     1,  # ptv_investment
     1,  # investment_secondary_media
     #1,  # digital_investment
     1,  # digital_impressions
    1, # febrero
    1, # marzo
    1, # abril
    1, # mayo
    1, # junio
    1, # julio
    1, # agosto
    1, # septiembre
    1, # octubre
    1, # noviembre
    1, # diciembre

    1, # nov19
    NULL
  )
)


# 2.2 Ajuste ----------------------------------------------------

summary(modelo_A) 

modelo_A$coefficients %>% as.data.frame() %>% write_clip()
ajuste(modelo_A)

# Autocorrelacion
pacf(modelo_A$residuals)
acf(modelo_A$residuals)

# Colinealidad
car::vif(modelo_A)

# Residuos
dt_modelo_A %>%
  add_residuals(modelo_A) %>%
  ggplot(aes(x = fecha, y = resid)) +
  geom_line(color = "#172744") +
  geom_hline(yintercept = 0, linetype = 2, color = "#DF2A35") +
  scale_x_date(breaks = seq(
    from = as_date("2017-01-01"),
    to = as_date("2021-01-01"),
    by = "1 month"
  )) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90),
    axis.line.x = element_line(color = "#262626"),
    axis.line.y = element_line(color = "#262626")
  )

# Ajuste
dt_modelo_A %>%
  add_predictions(modelo_A) %>%
  ggplot(aes(x = fecha)) +
  geom_line(aes(y = exp(pred), color = "Ajuste")) +
  geom_line(aes(y = venta_en_unidades, color = "Real")) +
  scale_color_manual(values = colores) +
  scale_x_date(breaks = seq(
    from = as_date("2017-01-01"),
    to = as_date("2021-01-01"),
    by = "2 months"
  )) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(size=18,angle = 90),
    panel.grid = element_blank(),
    axis.line.x = element_line(color = "#262626"),
    axis.line.y = element_line(color = "#262626"),
    legend.text = element_text(size=18),
    axis.text.y = element_text(size = 18))
  

ggsave("graphs/ot/ajuste_A.png",
       height = 12.5,
       width = 35,
       units = "cm"
)

dt_modelo_A %>%
  add_residuals(modelo_A) %>%
  add_predictions(modelo_A) %>%
  ggplot(aes(x = pred, y = resid / sd(resid))) +
  geom_point()

dt_modelo_A %>%
  add_residuals(modelo_A) %>%
  ggplot(aes(x = factor(month(fecha)), y = resid)) +
  geom_hline(yintercept = 0, color = "red", linetype = 2) +
  geom_boxplot()

dt_modelo_A %>%
  add_residuals(modelo_A) %>%
  ggplot(aes(x = factor(year(fecha)), y = resid)) +
  geom_hline(yintercept = 0, color = "red", linetype = 2) +
  geom_boxplot()



############################################################## .

## Seccion: 3. B ####

############################################################### .

dt_modelo_B <- lista_fabricantes$B  %>% select_if(~sum(!is.na(.)) > 0) %>% 
  na.omit() 

# 2.1 actualización datos 2020 --------------------------------------------

names <- read_xlsx("data/Layout SOM OT Chile 2021VF II.xlsx",
                   sheet = "B") %>% colnames()



nuevo <- read_xlsx("data/Layout SOM OT Chile 2021VF II.xlsx",sheet="B") %>%
 
  mutate(venta_en_unidades=ventas/ppu) %>% select(-ventas) %>% 
  select(fecha, venta_en_unidades,everything()) %>% 
  filter(!is.na(venta_en_unidades))  



dt_modelo_B <- dt_modelo_B %>% select(c("venta_en_unidades",names[-c(5)])) %>% 
  filter(fecha<=as.Date("2020-01-01")) %>%
  select(fecha, venta_en_unidades,everything()) %>% bind_rows(nuevo) %>% 
  mutate_at(vars(fecha),as.Date) %>%  mutate(
    nov19 = factor(
      if_else(fecha == "2019-11-01" , 1, 0)
    )
  ) %>% mutate(fecha=if_else(year(fecha)==2021,cambio_año(fecha,2020),
                      fecha))
  






# 3.1 Modelo --------------------------------------------------------------

fmla_B <-
  as.formula(
    log(venta_en_unidades) ~
      log(ppu) +
      log(td_ps) +
      #log(inventarios) +
      log(csd_price) +
      #log(penetracion)+
     # log(frecuencia)+
      #log(gasto_por_acto)+
      factor(month(fecha)) +
      factor(year(fecha)) +
      nov19+
      NULL
  )

set.seed(42)

modelo_B <- bayesglm(
  fmla_B,
  data = dt_modelo_B,
  family = gaussian(link = "identity"),
  maxit = 1000,
  prior.mean = c(
    -0.5, # ppu
    0.5, # tdps
    #0.1, # inventarios
    10, #csd_price
   # 0, #penetración
    #0, #frecuencia
   # 0, # gasto por acto
    0, # febrero
    0, # marzo
    0, # abril
    0, # mayo
    0, # junio
    0, # julio
    0, # agosto
    0, # septiembre
    0, # octubre
    0, # noviembre
    0, # diciembre
    0, # 2018
    0, # 2019
    0, # 2020
    0,
    NULL
  ),
  prior.scale = c(
    1, # ppu
    1, # tdps
    #1, # inventarios_unidades
    1, #csd_price
    #1, #penetración
   # 1, #frecuencia
    #1, #gasto por acto
    1, # febrero
    1, # marzo
    1, # abril
    1, # mayo
    1, # junio
    1, # julio
    1, # agosto
    1, # septiembre
    1, # octubre
    1, # noviembre
    1, # diciembre
    1, # 2018
    1, # 2019
    1, # 2020
    1,
    NULL
  ),
  prior.df = c(
    1, # ppu
    1, # tdps
    #1, # inventarios_unidades
    1, #csd_price
   # 1, #penetración
    #1, #frecuencia
   # 1, #gasto por acto
    1, # febrero
    1, # marzo
    1, # abril
    1, # mayo
    1, # junio
    1, # julio
    1, # agosto
    1, # septiembre
    1, # octubre
    1, # noviembre
    1, # diciembre
    1, # 2018
    1, # 2019
    1, # 2020
    1,
    NULL
  )
)


# 3.2 Ajuste ----------------------------------------------------

summary(modelo_B)

ajuste(modelo_B)
modelo_B$coefficients %>% as.data.frame() %>% write_clip()
# Autocorrelacion
pacf(modelo_B$residuals)
acf(modelo_B$residuals)

# Colinealidad
car::vif(modelo_B)

# Residuos
dt_modelo_B %>%
  add_residuals(modelo_B) %>%
  ggplot(aes(x = fecha, y = resid)) +
  geom_line(color = "#172744") +
  geom_hline(yintercept = 0, linetype = 2, color = "#DF2A35") +
  scale_x_date(breaks = seq(
    from = as_date("2017-01-01"),
    to = as_date("2020-03-01"),
    by = "1 month"
  )) +
  theme_minimal() +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(size=18,angle = 90),
    panel.grid = element_blank(),
    axis.line.x = element_line(color = "#262626"),
    axis.line.y = element_line(color = "#262626"),
    legend.text = element_text(size=18),
    axis.text.y = element_text(size = 18))

# Ajuste
dt_modelo_B %>%
  add_predictions(modelo_B) %>%
  ggplot(aes(x = fecha)) +
  geom_line(aes(y = exp(pred), color = "Ajuste")) +
  geom_line(aes(y = venta_en_unidades, color = "Real")) +
  scale_color_manual(values = colores) +
  scale_x_date(breaks = seq(
    from = as_date("2017-01-01"),
    to = as_date("2021-01-01"),
    by = "2 months"
  )) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(size=18,angle = 90),
    panel.grid = element_blank(),
    axis.line.x = element_line(color = "#262626"),
    axis.line.y = element_line(color = "#262626"),
    legend.text = element_text(size=18),
    axis.text.y = element_text(size = 18))


 ggsave("graphs/ot/ajuste_B.png",
        height = 12.5,
        width = 35,
        units = "cm"
 )

dt_modelo_B %>%
  add_residuals(modelo_B) %>%
  add_predictions(modelo_B) %>%
  ggplot(aes(x = pred, y = resid / sd(resid))) +
  geom_point()

dt_modelo_B %>%
  add_residuals(modelo_B) %>%
  ggplot(aes(x = factor(month(fecha)), y = resid)) +
  geom_hline(yintercept = 0, color = "red", linetype = 2) +
  geom_boxplot()

dt_modelo_B %>%
  add_residuals(modelo_B) %>%
  ggplot(aes(x = factor(year(fecha)), y = resid)) +
  geom_hline(yintercept = 0, color = "red", linetype = 2) +
  geom_boxplot()




############################################################## .

## Seccion: 6. C ####

############################################################### .

dt_modelo_C <- lista_fabricantes$C  %>% select_if(~sum(!is.na(.)) > 0) %>% 
  na.omit() 

# 2.1 actualización datos 2020 --------------------------------------------

names <- read_xlsx("data/Layout SOM OT Chile 2021VF II.xlsx",
                   sheet = "C") %>% colnames()



nuevo <- read_xlsx("data/Layout SOM OT Chile 2021VF II.xlsx",sheet="C") %>%
  mutate(venta_en_unidades=ventas/ppu) %>% select(-ventas) %>% 
  select(fecha, venta_en_unidades,everything()) %>% 
  filter(!is.na(venta_en_unidades))  



dt_modelo_C <- dt_modelo_C %>% select(c("venta_en_unidades",names[-c(4)])) %>% 
  filter(fecha<=as.Date("2020-01-01")) %>%
  select(fecha, venta_en_unidades,everything()) %>% bind_rows(nuevo) %>% 
  mutate_at(vars(fecha),as.Date)  %>% mutate(
    nov19 = factor(
      if_else(fecha == "2019-11-01" , 1, 0)
    )
  ) %>%
  mutate(
    dec18 = factor(
      if_else(fecha == "2018-12-01" , 1, 0)
    )
  ) %>% mutate(fecha=if_else(year(fecha)==2021,cambio_año(fecha,2020),
                             fecha))




# 6.1 Modelo --------------------------------------------------------------

fmla_C <-
  as.formula(
    log(venta_en_unidades) ~
      log(ppu) +
      log(distribucion) +
      factor(month(fecha)) +
      factor(year(fecha)) +
      nov19+
      dec18+
      NULL
  )

set.seed(42)

modelo_C <- bayesglm(
  fmla_C,
  data = dt_modelo_C,
  family = gaussian(link = "identity"),
  maxit = 1000,
  prior.mean = c(
    -0.5, # ppu
    0.5, # distribucion
    0, # febrero
    0, # marzo
    0, # abril
    0, # mayo
    0, # junio
    0, # julio
    0, # agosto
    0, # septiembre
    0, # octubre
    0, # noviembre
    0, # diciembre
    0, # 2018
    0, # 2019
    0, # 2020
    0,
    0,
    NULL
  ),
  prior.scale = c(
    0.3, # ppu
    1, # distribucion
    1, # febrero
    1, # marzo
    1, # abril
    1, # mayo
    1, # junio
    1, # julio
    1, # agosto
    1, # septiembre
    1, # octubre
    1, # noviembre
    1, # diciembre
  
    1, # 2018
    1, # 2019
    1, # 2020
    1,
    1,
    NULL
  ),
  prior.df = c(
    Inf, # ppu
    1, # distribucion
    1, # febrero
    1, # marzo
    1, # abril
    1, # mayo
    1, # junio
    1, # julio
    1, # agosto
    1, # septiembre
    1, # octubre
    1, # noviembre
    1, # diciembre
    
    1, # 2018
    1, # 2019
    1, # 2020
    1,
    1,
    NULL
  )
)


# 6.2 Ajuste ----------------------------------------------------

summary(modelo_C)

 ajuste(modelo_C)
 
 modelo_B$coefficients %>% as.data.frame() %>% write_clip()
# Se utiliza la correlación como ajuste porque las ventas son muy planas

dt_modelo_C %>%
  add_predictions(modelo_C) %>%
  summarise(cor(venta_en_unidades, pred))
modelo_C$coefficients %>% as.data.frame() %>% write_clip()

# Autocorrelacion
pacf(modelo_C$residuals)
acf(modelo_C$residuals)

# Colinealidad
car::vif(modelo_C)

# Residuos
dt_modelo_C %>%
  add_residuals(modelo_C) %>%
  ggplot(aes(x = fecha, y = resid)) +
  geom_line(color = "#172744") +
  geom_hline(yintercept = 0, linetype = 2, color = "#DF2A35") +
  scale_x_date(breaks = seq(
    from = as_date("2017-01-01"),
    to = as_date("2021-01-01"),
    by = "1 month"
  )) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90),
    axis.line.x = element_line(color = "#262626"),
    axis.line.y = element_line(color = "#262626")
  )

# Ajuste
dt_modelo_C %>%
  add_predictions(modelo_C) %>%
  ggplot(aes(x = fecha)) +
  geom_line(aes(y = exp(pred), color = "Ajuste")) +
  geom_line(aes(y = venta_en_unidades, color = "Real")) +
  scale_color_manual(values = colores) +
  scale_x_date(breaks = seq(
    from = as_date("2017-01-01"),
    to = as_date("2021-01-01"),
    by = "2 months"
  )) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "", y = "") +
  theme_minimal() +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(size=18,angle = 90),
    panel.grid = element_blank(),
    axis.line.x = element_line(color = "#262626"),
    axis.line.y = element_line(color = "#262626"),
    legend.text = element_text(size=18),
    axis.text.y = element_text(size = 18))

 ggsave("graphs/ot/ajuste_C.png",
        height = 12.5,
        width = 35,
        units = "cm"
 )

dt_modelo_C %>%
  add_residuals(modelo_C) %>%
  add_predictions(modelo_C) %>%
  ggplot(aes(x = pred, y = resid / sd(resid))) +
  geom_point()

dt_modelo_C %>%
  add_residuals(modelo_C) %>%
  ggplot(aes(x = factor(month(fecha)), y = resid)) +
  geom_hline(yintercept = 0, color = "red", linetype = 2) +
  geom_boxplot()

dt_modelo_C %>%
  add_residuals(modelo_C) %>%
  ggplot(aes(x = factor(year(fecha)), y = resid)) +
  geom_hline(yintercept = 0, color = "red", linetype = 2) +
  geom_boxplot()


############################################################## .

## Seccion: 7. Guardar ####

############################################################### .

readr::write_rds(dt_modelo_A, "cache/ot/dt_modelo_A.rds")
readr::write_rds(modelo_A, "cache/ot/modelo_A.rds")

readr::write_rds(
  dt_modelo_B, "cache/ot/dt_modelo_B.rds"
)
readr::write_rds(
  modelo_B, "cache/ot/modelo_B.rds"
)

readr::write_rds(dt_modelo_C, "cache/ot/dt_modelo_C.rds")
readr::write_rds(modelo_C, "cache/ot/modelo_C.rds")

beepr::beep()

###############################################################

## Seccion: pruebas del shiny

###############################################################
library(readxl)

dt1 <- read_excel("data/salty_ot_example.xlsx", 
                               sheet = "A") %>% filter(is.na(ventas)) 
dt1$nov19 <- dt1 %>% pull(nov19) %>% as.factor()
dt1 <- dt1  %>% mutate(cambio=ifelse(year(fecha)=="2021",1,0))
dt1$fecha <- dt1 %>% pull(fecha) %>% as.Date(origin="1970-01-01 UTC")
dt1 <- dt1  %>% mutate(fecha=ifelse(year(fecha)=="2021",fecha-365,fecha))
dt1$fecha <- dt1 %>% pull(fecha) %>% as.Date(origin="1970-01-01 UTC")
if("dec18" %in% colnames(dt1)){
  dt1$dec18 <- dt1 %>% pull(dec18) %>% as.factor()
}
dt1 <- dt1 %>%add_predictions(modelo_A) %>% 
  mutate(
    fit_venta_en_unidades = exp(pred) * ppu
  )

dt1 <- dt1  %>% mutate(fecha=ifelse(cambio==1,fecha+365,fecha))
dt1$fecha <- dt1 %>% pull(fecha) %>% as.Date(origin="1970-01-01 UTC")
  
  
  

dt2  <- read_excel("data/salty_ot_example.xlsx", 
                               sheet = "B")%>% filter(is.na(ventas))

dt2$nov19 <- dt2 %>% pull(nov19) %>% as.factor()
dt2 <- dt2  %>% mutate(cambio=ifelse(year(fecha)=="2021",1,0))
dt2$fecha <- dt2 %>% pull(fecha) %>% as.Date(origin="1970-01-01 UTC")
dt2 <- dt2  %>% mutate(fecha=ifelse(year(fecha)=="2021",fecha-365,fecha))
dt2$fecha <- dt2 %>% pull(fecha) %>% as.Date(origin="1970-01-01 UTC")
if("dec18" %in% colnames(dt2)){
  dt2$dec18 <- dt2 %>% pull(dec18) %>% as.factor()
}
dt2 <- dt2 %>%add_predictions(modelo_B) %>% 
mutate(
  fit_venta_en_unidades = exp(pred) * ppu
)
dt2 <- dt2  %>% mutate(fecha=ifelse(cambio==1,fecha+365,fecha))
dt2$fecha <- dt2 %>% pull(fecha) %>% as.Date(origin="1970-01-01 UTC")
  

dt3 <- read_excel("data/salty_ot_example.xlsx", 
                               sheet = "C")%>% filter(is.na(ventas))

dt3$nov19 <- dt3 %>% pull(nov19) %>% as.factor()
dt3 <- dt3  %>% mutate(cambio=ifelse(year(fecha)=="2021",1,0))
dt3$fecha <- dt3 %>% pull(fecha) %>% as.Date(origin="1970-01-01 UTC")
dt3 <- dt3  %>% mutate(fecha=ifelse(year(fecha)=="2021",fecha-365,fecha))
dt3$fecha <- dt3 %>% pull(fecha) %>% as.Date(origin="1970-01-01 UTC")
if("dec18" %in% colnames(dt3)){
  dt3$dec18 <- dt3 %>% pull(dec18) %>% as.factor()
}
dt3 <- dt3 %>%add_predictions(modelo_C) %>% 
  mutate(
    fit_venta_en_unidades = exp(pred) * ppu
  )

dt3 <- dt3  %>% mutate(fecha=ifelse(cambio==1,fecha+365,fecha))
dt3$fecha <- dt3 %>% pull(fecha) %>% as.Date(origin="1970-01-01 UTC")
  

dt3 %>% mutate_if(is.numeric,~round(.,digits = 2))

###############################################################

## Seccion: pruebas layout

###############################################################
library(clipr)

base1 <- readRDS("cache/ot/dt_modelo_A.rds") %>% filter(fecha>=as.Date("2020-01-01")) 
base1 %>% mutate(ventas=venta_en_unidades*ppu) %>% 
  select(ventas,td_ps,csd_price,	penetracion,	frecuencia,	otv_grps,
 ptv_investment,	investment_secondary_media,
 digital_impressions,
 listas_promocionales,
 d_a,	ppu) %>% write_clip()

dt1 <- readxl::read_xlsx("data/Layout SOM OT Chile 2021_v2.xlsx",sheet="A")
modelo1 <- readRDS("cache/ot/modelo_A.rds")

dt2 <- readxl::read_xlsx("data/Layout SOM OT Chile 2021_v2.xlsx",sheet = "B")
modelo2 <- readRDS("cache/ot/modelo_B.rds")

dt3 <- readxl::read_xlsx("data/Layout SOM OT Chile 2021_v2.xlsx",sheet = "C")
modelo3 <- readRDS("cache/ot/modelo_C.rds")

make_predictions <- function(dt, modelo) {
  # dt$nov19 <- dt %>%
  #   rep(0,nrow(.)) %>%
  #   as.factor(levels=c(0,1))
  dt <- dt %>% mutate(cambio = ifelse(year(fecha) == "2021", 1, 0))
  dt$fecha <- dt %>%
    dplyr::pull(fecha) %>%
    as.Date(origin = "1970-01-01 UTC")
  dt <- dt %>% mutate(fecha = ifelse(year(fecha) == "2021", fecha - 365, fecha))
  dt$fecha <- dt %>%
    dplyr::pull(fecha) %>%
    as.Date(origin = "1970-01-01 UTC")
  
  # if ("dec18" %in% colnames(dt)) {
  #   dt$dec18 <- dt %>%
  #     dplyr::pull(dec18) %>%
  #     as.factor()
  # }
  
  dt1 <- dt %>%
    dplyr::mutate(
      nov19= factor(0, levels = c(0, 1)),
      dec18= factor(0, levels = c(0, 1))
      # nov18_ene19 = factor(0, levels = c(0, 1)),
      # oct17_ene18 = factor(0, levels = c(0, 1)),
      # mar_sep18 = factor(0, levels = c(0, 1)),
      # nov18_jul19 = factor(0, levels = c(0, 1)),
      # jun18_ene19 = factor(0, levels = c(0, 1)),
      # ene19_oct19 = factor(0, levels = c(0, 1)),
      # may18_oct18 = factor(0, levels = c(0, 1)),
      # mar19_ago19 = factor(0, levels = c(0, 1)),
      # mar18_sep18 = factor(0, levels = c(0, 1)),
      # ago18_ene19 = factor(0, levels = c(0, 1)),
      # ago18_dic18 = factor(0, levels = c(0, 1)),
      # abr18_jul18 = factor(0, levels = c(0, 1)),
      # jul19_oct19 = factor(0, levels = c(0, 1)),
      # temblor = factor(0, levels = c(0, 1)),
      # abr19_ago19 = factor(0, levels = c(0, 1)),
      # abr17_sep17 = factor(0, levels = c(0, 1)),
      # ene18_mar18 = factor(0, levels = c(0, 1)),
      # sismo = factor(0, levels = c(0, 1)),
      # sep18_feb19 = factor(0, levels = c(0, 1)),
      # ago17_dic17 = factor(0, levels = c(0, 1)),
      # may18_sep18 = factor(0, levels = c(0, 1)),
      # jul17_oct17 = factor(0, levels = c(0, 1)),
      # feb19_ago19 = factor(0, levels = c(0, 1)),
      # ago18_feb19 = factor(0, levels = c(0, 1))
    ) %>%
    modelr::add_predictions(modelo) %>%
    dplyr::mutate(
      ventas = if_else(is.na(ventas), exp(pred) * ppu, ventas)
    ) %>%
    select(fecha, ventas, cambio)
  dt1 <- dt1 %>% mutate(fecha = ifelse(cambio == 1, fecha + 365, fecha))
  dt1$fecha <- dt1 %>%
    dplyr::pull(fecha) %>%
    as.Date(origin = "1970-01-01 UTC")
  out <- dt1 %>% select(-cambio)
  
  if (!is.null(dt[["pep_growth"]])) {
    out[["pep_growth"]] <- dt[["pep_growth"]]
  }
  
  return(out)
}

dt_modelo_A <- make_predictions(dt1,modelo1) %>% write_clip()
dt_modelo_B <- make_predictions(dt2,modelo2)
dt_modelo_C <- make_predictions(dt3,modelo3)

suma_venta_en_unidades <- dt_modelo_A$ventas +
  dt_modelo_B$ventas +
  dt_modelo_C$ventas

dt_A_som <- dt_modelo_A %>%
  mutate(
    som = ventas / suma_venta_en_unidades,
    
  )




