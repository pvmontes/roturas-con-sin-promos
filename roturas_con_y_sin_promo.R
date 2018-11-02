#Roturas con y sin promociones.

library(dplyr)

library(readxl)

setwd("C:/Users/maria.purificacion/Documents/promociones-roturas")

datos_promo <- read_excel("hr_promociones_2018.xlsx") 

datos_totales <- read_excel("hr_servicios_totales_2018.xlsx")



#We obtain the number of unique id_servicio from datos_promo.
# datos_promo %>%
#             summarise(n_distinct(id_servicio))



datos_no_promo <- anti_join(datos_totales, datos_promo, by = "id_servicio")


servicios_por_centro_articulo_no_promo <- datos_no_promo %>%
  group_by(codCentro, codArticulo) %>%
  count(codCentro) %>%
  arrange(codCentro)

colnames(servicios_por_centro_articulo_no_promo) <- c("codCentro", "codArticulo", "num_servicios_no_promo")

# View(servicios_por_centro_articulo_no_promo)

# sum(servicios_por_centro_articulo_no_promo$num_servicios_no_promo)



servicios_por_centro_articulo_promo <- datos_promo %>%
  group_by(codCentro, codArticulo) %>%
  # group_by(codCentro) %>%
  count(codCentro) %>%
  arrange(codCentro)

colnames(servicios_por_centro_articulo_promo) <- c("codCentro", "codArticulo", "num_servicios_promo")

# View(servicios_por_centro_articulo_promo)


# servicios_por_ensena <- datos %>%
#                         group_by(codEnsena) %>%
#                         count(codEnsena) %>%
#                         arrange(codEnsena)
# 
# 
# View(servicios_por_ensena)

roturas_no_promo <- datos_no_promo %>%
  arrange(codCentro, codArticulo, fechaCalc) %>%
  filter(roturaLineal == 1 | roturaAlmacen == 1) %>%
  select(codCentro, codArticulo, fechaCalc, roturaLineal, roturaAlmacen) 


roturas_promo <- datos_promo %>%
  arrange(codEnsena, codCentro, codArticulo, fecha) %>%
  filter(roturaLineal == 1 | roturaAlmacen == 1) %>%
  select(codEnsena, codCentro, codArticulo, fecha, roturaLineal, roturaAlmacen) 

# str(roturas)
# 
# 
# View(roturas)

# nrow(roturas)
# 
# table(roturas$codCentro)
# View(transform(table(roturas$codCentro)))


roturas_fq_no_promo <- roturas_no_promo %>%
  count(codCentro, codArticulo) %>%
  # arrange(desc(n))
  arrange(codCentro)

colnames(roturas_fq_no_promo) <- c("codCentro", "codArticulo", "num_roturas_no_promo")


roturas_fq_promo <- roturas_promo %>%
  count(codCentro, codArticulo) %>%
  # arrange(desc(n))
  arrange(codCentro)

colnames(roturas_fq_promo) <- c("codCentro", "codArticulo", "num_roturas_promo")


# sum(roturas_fq_promo$num_roturas_promo)

# View(roturas_fq_promo)


roturas_por_servicios_no_promo <- merge(roturas_fq_no_promo, servicios_por_centro_articulo_no_promo, by = c("codCentro", "codArticulo"))

colnames(roturas_por_servicios_no_promo) <- c("codCentro", "codArticulo", "num_roturas_no_promo", "num_servicios_no_promo")

View(roturas_por_servicios_no_promo)

roturas_por_servicios_promo <- merge(roturas_fq_promo, servicios_por_centro_articulo_promo, by = c("codCentro", "codArticulo"))

colnames(roturas_por_servicios_promo) <- c("codCentro", "codArticulo", "num_roturas_promo", "num_servicios_promo")

roturas_por_servicios_promo$porcent_roturas <- round((roturas_por_servicios_promo$num_roturas / roturas_por_servicios_promo$num_servicios_promo)*100, 2)

View(roturas_por_servicios_promo)


roturas_totales <- merge(roturas_por_servicios_no_promo, roturas_por_servicios_promo, by = c("codCentro", "codArticulo"))

# View(roturas_totales)



write.csv(roturas_totales, "roturas_totales.csv")



#To knowing the article name we would use a table from Database.
























#Code chunks.

# View(roturas_por_servicios[order(roturas_por_servicios$porcent_roturas, decreasing = TRUE),])

roturas_tidy <- roturas_por_servicios[order(roturas_por_servicios$porcent_roturas, decreasing = TRUE),]

# nrow(roturas_tidy)

write.csv(roturas_tidy, "roturas_hr_17_tidy.csv")

# View(roturas_fq)


roturas_por_servicios_mas_de_20 <- roturas_por_servicios %>%
  filter(num_servicios >= 20)


View(roturas_por_servicios_mas_de_20)

