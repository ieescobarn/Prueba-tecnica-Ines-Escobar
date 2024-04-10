# ++++++++++++++++++++++++++++++++++++++++++++++++++ #
# -------------------------------------------------- #
#----Prueba técnica - ADRES -------------------------- 
#----Versión R: 4.3.2 --------------------------------
#----Versión Rstudio: 2023.09.1+494 ------------------
#----Por: Inés Enai Escobar Navas --------------------
# -------------------------------------------------- # 
# ++++++++++++++++++++++++++++++++++++++++++++++++++ #

# ++++++++++++++++++++++++++++ #
#----1. Alistamiento Consola----
# ++++++++++++++++++++++++++++ #

rm(list = ls()) # Limpia el ambiente de trabajo.

graphics.off() # Limpia visualizador de plots

shell("cls") # Limpia la consola

options(scipen=999) # Quitar notación científica

# ++++++++++++++++++++++++++++++++++++++++++++++++ #
#----2. Instalar y cargar o sólo cargar paquetes---- 
# ++++++++++++++++++++++++++++++++++++++++++++++++ #

librerias <- c("plyr","tidyverse","lubridate", "stringi","data.table","splitstackshape", "gmodels", "car",
               "DataExplorer","magrittr","tseries","zoo","reshape2","FactoClass", "RSocrata",
               "readxl","openxlsx", "janitor","RSocrata","car","gridExtra", "RSQLite") 

for (i in 1:length(librerias)) {
  if(librerias[i] %in% rownames(installed.packages()) == FALSE) {
    install.packages(librerias[i])
    print(paste("La librería",librerias[i],"se instaló correctamente."))
    lapply(librerias, require, character.only = TRUE)
    print(paste("La librería",librerias[i],"se cargó correctamente."))
  } else {
    lapply(librerias, require, character.only = TRUE)
    print(paste("La librería",librerias[i],"se cargó correctamente."))
  }
}

# ++++++++++++++++++++++++ #
#----3. Fijar Directorio----
# ++++++++++++++++++++++++ #

setwd("C:/Users/el_ki/OneDrive/Documentos/ADRES")

# +++++++++++++++++++++ #
#----4. Cargar datos----
# +++++++++++++++++++++ #

list.files()

path_DDBB <- "DB_ADRES_ieescobarn.db" # Se especifica el archivo donde se encuentra la base de datos de SQLite

conexion <- dbConnect(SQLite(), dbname = path_DDBB) # Se realiza el proceso de conexión

dbListTables(conexion) # Genera un print de las tablas asociadas a la base de datos de SQLite

conexion_municipios <- dbGetQuery(conexion, "SELECT * FROM Municipios") # Se extrae la tabla municipios

conexion_prestadores <- dbGetQuery(conexion, "SELECT * FROM Prestadores")  # Se extrae la tabla prestadores

dbListFields(conexion, "nac2022") # Genera un print de las variables asociadas a la tabla "nac2022" (Nacimientos del año 2022 fuente DANE)

conexion_nacimientos_dane2022 <- dbGetQuery(conexion, "SELECT COD_DPTO, COD_MUNIC FROM nac2022")  # Se extrae la tabla nac2022

tabla_municipios <- as.data.frame(conexion_municipios) # Se procesa la tabla "municipios" para que se convierta
                                                       # en data frame.

tabla_prestadores <- as.data.frame(conexion_prestadores) # Se procesa la tabla "prestadores" para que se convierta
                                                         # en data frame.

tabla_nacimientos_dane2022 <- as.data.frame(conexion_nacimientos_dane2022) # Se procesa la tabla "nac2022" para que se convierta en 
                                                                           # data frame

web_divipola_municipios <- "https://www.datos.gov.co/api/odata/v4/gdxc-w37w"

datos_divipola_municipios <- read.socrata(web_divipola_municipios) %>% 
  slice(1:1121)

web_divipola_depto <- "https://www.datos.gov.co/api/odata/v4/vcjz-niiq"

datos_divipola_depto <- read.socrata(web_divipola_depto)

web_registro_especial_prestadores <- "https://www.datos.gov.co/api/odata/v4/c36g-9fc2"

datos_registro_especial_prestadores <- read.socrata(web_registro_especial_prestadores)

# ++++++++++++++++++++++++++++ #
#----5. Tratamiento de datos----
# ++++++++++++++++++++++++++++ #

datos_registro_especial_prestadores <- datos_registro_especial_prestadores %>% 
  group_by(codigoprestador, municipio_prestador, municipioprestadordesc, departamentoprestadordesc) %>% 
  summarise(Total = n()) %>% 
  select(-Total)

Municipios <- tabla_municipios %>% 
  mutate(Dep = as.character(Dep)) %>% 
  left_join(datos_divipola_depto %>% mutate(codigo_departamento = as.character(codigo_departamento)), 
            by = c("Dep" = "codigo_departamento")) %>% 
  select(-Departamento) %>%
  select(nombre_departamento, everything()) %>% 
  rename(Departamento = nombre_departamento) %>% 
  left_join(datos_divipola_municipios, by = c("Depmun" = "cod_mpio")) %>% 
  mutate(Region = gsub("Regi[^A-Za-z0-9]n", "Región", Region, ignore.case = TRUE)) %>% 
  select(-Municipio, -cod_depto,-dpto,-tipo_municipio) %>% 
  rename(Municipio = nom_mpio) %>% 
  select(Departamento, Dep,Municipio, everything())
  
Prestadores <- tabla_prestadores %>%
  mutate(codigo_habilitacion = as.character(codigo_habilitacion)) %>% 
  left_join(datos_registro_especial_prestadores, by = c("codigo_habilitacion" = "codigoprestador"))

Prestadores_01_01_01 <- Prestadores %>% 
  filter(is.na(municipio_prestador)) %>% 
  filter(grepl("[^A-Za-z0-9]", muni_nombre)) %>% 
  filter(grepl("\\s", muni_nombre)) %>% 
  mutate(
    muni_nombre = gsub("AGUST[^A-Za-z0-9]N CODAZZI", "AGUSTÍN CODAZZI", muni_nombre, ignore.case = TRUE), 
    muni_nombre = gsub("BARRANCA DE UP[^A-Za-z0-9]A", "BARRANCA DE UPÍA", muni_nombre, ignore.case = TRUE), 
    muni_nombre = gsub("CARMEN DE APICAL[^A-Za-z0-9]", "CARMEN DE APICALÁ", muni_nombre, ignore.case = TRUE), 
    muni_nombre = gsub("CI[^A-Za-z0-9]NAGA DE ORO",  "CIÉNAGA DE ORO", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("CIUDAD BOL[^A-Za-z0-9]VAR" , "CIUDAD BOLÍVAR", muni_nombre, ignore.case = TRUE), 
    muni_nombre = gsub("DON MAT[^A-Za-z0-9]AS" , "DON MATÍAS", muni_nombre, ignore.case = TRUE), 
    muni_nombre = gsub("EL CARMEN DE BOL[^A-Za-z0-9]VAR", "EL CARMEN DE BOLÍVAR", muni_nombre, ignore.case = TRUE), 
    muni_nombre = gsub("EL RET[^A-Za-z0-9]N", "EL RETÓN", muni_nombre, ignore.case = TRUE), 
    muni_nombre = gsub("EL TABL[^A-Za-z0-9]N DE G[^A-Za-z0-9]MEZ", "EL TABLÓN DE GÓMEZ", muni_nombre, ignore.case = TRUE), 
    muni_nombre = gsub("LA UNI[^A-Za-z0-9]N", "LA UNIÓN", muni_nombre, ignore.case = TRUE), 
    muni_nombre = gsub("PIJI[^A-Za-z0-9]O DEL CARMEN", "PIJIÑO DEL CARMEN", muni_nombre, ignore.case = TRUE), 
    muni_nombre = gsub("PUERTO AS[^A-Za-z0-9]S" , "PUERTO ASÍS", muni_nombre, ignore.case = TRUE), 
    muni_nombre = gsub("PUERTO BERR[^A-Za-z0-9]O" , "PUERTO BERRÍO", muni_nombre, ignore.case = TRUE), 
    muni_nombre = gsub("PUERTO BOYAC[^A-Za-z0-9]" , "PUERTO BOYACÁ", muni_nombre, ignore.case = TRUE), 
    muni_nombre = gsub("PUERTO GAIT[^A-Za-z0-9]N" , "PUERTO GAITÁN", muni_nombre, ignore.case = TRUE), 
    muni_nombre = gsub("SAN AGUST[^A-Za-z0-9]N" , "SAN AGUSTÍN", muni_nombre, ignore.case = TRUE), 
    muni_nombre = gsub("SAN ANDR[^A-Za-z0-9]S" , "SAN ANDRÉS", muni_nombre, ignore.case = TRUE), 
    muni_nombre = gsub("SAN JER[^A-Za-z0-9]NIMO" , "SAN JERÓNIMO", muni_nombre, ignore.case = TRUE), 
    muni_nombre = gsub("SAN JOS[^A-Za-z0-9] DE LA MONTA[^A-Za-z0-9]A" , "SAN JOSÉ DE LA MONTAÑA", muni_nombre, ignore.case = TRUE), 
    muni_nombre = gsub("SAN JOS[^A-Za-z0-9] DEL FRAGUA" , "SAN JOSÉ DEL FRAGUA", muni_nombre, ignore.case = TRUE), 
    muni_nombre = gsub("SAN JOS[^A-Za-z0-9] DEL GUAVIARE" , "SAN JOSÉ DEL GUAVIARE", muni_nombre, ignore.case = TRUE), 
    muni_nombre = gsub("SAN JUAN DE URAB[^A-Za-z0-9]" , "SAN JUAN DE URABÁ", muni_nombre, ignore.case = TRUE), 
    muni_nombre = gsub("SAN MART[^A-Za-z0-9]N" , "SAN MARTÍN", muni_nombre, ignore.case = TRUE), 
    muni_nombre = gsub("SANTA LUC[^A-Za-z0-9]A" , "SANTA LUCÍA", muni_nombre, ignore.case = TRUE), 
    muni_nombre = gsub("SANTAF[^A-Za-z0-9] DE ANTIOQUIA" , "SANTAFÉ DE ANTIOQUIA", muni_nombre, ignore.case = TRUE), 
    muni_nombre = gsub("SANTO TOM[^A-Za-z0-9]S" , "SANTO TOMÁS", muni_nombre, ignore.case = TRUE)) 

Prestadores_01_01_02 <- Prestadores %>% 
  filter(is.na(municipio_prestador)) %>% 
  filter(grepl("[^A-Za-z0-9]", muni_nombre)) %>% 
  filter(!grepl("\\s", muni_nombre)) %>% 
  mutate(
    muni_nombre = gsub("ACAC[^A-Za-z0-9]AS", "ACACÍAS", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("ACAND[^A-Za-z0-9]", "ACANDÍ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("ALB[^A-Za-z0-9]N", "ALBÁN", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("APARTAD[^A-Za-z0-9]" , "APARTADÓ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("ARIGUAN[^A-Za-z0-9]" , "ARIGUANÍ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("BELALC[^A-Za-z0-9]ZAR" , "BELALCÁZAR", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("BOGOT[^A-Za-z0-9]" , "BOGOTÁ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("CAJIC[^A-Za-z0-9]" , "CAJICÁ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("C[^A-Za-z0-9]CERES" , "CÁCERES", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("C[^A-Za-z0-9]CUTA" , "CÚCUTA", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("CERET[^A-Za-z0-9]" , "CERETÉ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("CH[^A-Za-z0-9]A" , "CHÍA", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("CHACHAG[^A-Za-z0-9][^A-Za-z0-9]" , "CHACHAGÜÍ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("CHARAL[^A-Za-z0-9]" , "CHARALÁ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("CHIGOROD[^A-Za-z0-9]" , "CHIGORODÓ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("CHIN[^A-Za-z0-9]" , "CHINÚ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("CHINCHIN[^A-Za-z0-9]" , "CHINCHINÁ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("CHIN[^A-Za-z0-9]COTA" , "CHINÁCOTA", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("CHIQUINQUIR[^A-Za-z0-9]" , "CHIQUINQUIRÁ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("CHITAG[^A-Za-z0-9]" , "CHITAGÁ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("CHOCONT[^A-Za-z0-9]" , "CHOCONTÁ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("CI[^A-Za-z0-9]NAGA" , "CIÉNAGA", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("COCORN[^A-Za-z0-9]" , "COCORNÁ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("CONVENCI[^A-Za-z0-9]N" , "CONVENCIÓN", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("COVE[^A-Za-z0-9]AS" , "COVEÑAS", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("C[^A-Za-z0-9]RDOBA" , "CÓRDOBA", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("FACATATIV[^A-Za-z0-9]" , "FACATATIVÁ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("FUNDACI[^A-Za-z0-9]N" , "FUNDACIÓN", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("FUSAGASUG[^A-Za-z0-9]" , "FUSAGASUGÁ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("GACHET[^A-Za-z0-9]" , "GACHETÁ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("GARZ[^A-Za-z0-9]N" , "GARZÓN", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("G[^A-Za-z0-9]IC[^A-Za-z0-9]N" , "GÜICÁN", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("GIR[^A-Za-z0-9]N" , "GIRÓN", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("GUALMAT[^A-Za-z0-9]N" , "GUALMATÁN", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("IBAGU[^A-Za-z0-9]" , "IBAGUÉ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("IN[^A-Za-z0-9]RIDA" , "INÍRIDA", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("JAMUND[^A-Za-z0-9]" , "JAMUNDÍ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("JARD[^A-Za-z0-9]N" , "JARDÍN", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("JERICÓ[^A-Za-z0-9]" , "JERICÓ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("LEBR[^A-Za-z0-9]JA" , "LEBRIJA", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("L[^A-Za-z0-9]RIDA" , "LÉRIDA", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("MAGANGU[^A-Za-z0-9]" , "MAGANGUÉ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("MAG[^A-Za-z0-9]I" , "MAGÜI", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("MEDELL[^A-Za-z0-9]N" , "MEDELLÍN", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("MIT[^A-Za-z0-9]" , "MITÚ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("M[^A-Za-z0-9]LAGA" , "MÁLAGA", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("MONIQUIR[^A-Za-z0-9]" , "MONIQUIRÁ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("MONTEL[^A-Za-z0-9]BANO" , "MONTELÍBANO", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("MONTER[^A-Za-z0-9]A" , "MONTERÍA", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("NARI[^A-Za-z0-9]O" , "NARIÑO", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("NECH[^A-Za-z0-9]" , "NECHÍ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("OCA[^A-Za-z0-9]A" , "OCAÑA", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("PAT[^A-Za-z0-9]A" , "PATÍA", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("P[^A-Za-z0-9]CORA" , "PÁCORA", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("SAMAC[^A-Za-z0-9]" , "SAMACÁ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("PE[^A-Za-z0-9]OL" , "PEÑOL", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("POPAY[^A-Za-z0-9]N" , "POPAYÁN", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("QUIBD[^A-Za-z0-9]" , "QUIBDÓ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("QUINCH[^A-Za-z0-9]A" , "QUINCHÍA", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("SAHAG[^A-Za-z0-9]N" , "SAHAGÚN", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("SALDA[^A-Za-z0-9]A" , "SALDAÑA", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("SAMAN[^A-Za-z0-9]" , "SAMANÁ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("SANDON[^A-Za-z0-9]" , "SANDONÁ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("S[^A-Za-z0-9]CHICA" , "SÁCHICA", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("SIBATÉ[^A-Za-z0-9]" , "SIBATÉ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("SINC[^A-Za-z0-9]" , "SINCÉ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("SOAT[^A-Za-z0-9]" , "SOATÁ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("SOP[^A-Za-z0-9]" , "SOPÓ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("SUPAT[^A-Za-z0-9]" , "SUPATÁ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("TIMAN[^A-Za-z0-9]" , "TIMANÁ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("TOCANCIP[^A-Za-z0-9]" , "TOCANCIPÁ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("T[^A-Za-z0-9]QUERRES" , "TÚQUERRES", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("TULU[^A-Za-z0-9]" , "TULUÁ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("VALPARA[^A-Za-z0-9]SO" , "VALPARAÍSO", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("VEGACH[^A-Za-z0-9]" , "VEGACHÍ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("VILLAMAR[^A-Za-z0-9]A" , "VILLAMARÍA", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("VILLAPINZ[^A-Za-z0-9]N" , "VILLAPINZÓN", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("YAGUAR[^A-Za-z0-9]" , "YAGUARÍ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("YAL[^A-Za-z0-9]" , "YALÍ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("YOLOMB[^A-Za-z0-9]" , "YOLOMBÓ", muni_nombre, ignore.case = TRUE),
    muni_nombre = gsub("ZIPAQUIR[^A-Za-z0-9]" , "ZIPAQUIRÁ", muni_nombre, ignore.case = TRUE))

Prestadores_01_01 <- rbind(Prestadores_01_01_01, Prestadores_01_01_02)

Prestadores_01_02 <- Prestadores %>% 
  filter(is.na(municipio_prestador)) %>% 
  filter(!grepl("[^A-Za-z0-9]", muni_nombre))

Prestadores_01 <- rbind(Prestadores_01_01, Prestadores_01_02)

Prestadores_02 <- Prestadores %>% 
  filter(!is.na(municipio_prestador)) 

rm(Prestadores_01_01,Prestadores_01_01_01,Prestadores_01_01_02,Prestadores_01_02)

Prestadores <- rbind(Prestadores_01, Prestadores_02) %>% 
  arrange(muni_nombre, municipio_prestador) %>% 
  fill(municipio_prestador, municipioprestadordesc, departamentoprestadordesc) %>% 
  select(-muni_nombre) %>% 
  rename(muni_nombre = municipioprestadordesc) %>% 
  select(-tido_codigo, -departamentoprestadordesc, -fecha_cierre) %>% 
  select(depa_nombre, muni_nombre, everything()) %>% 
  mutate(ese = ifelse(is.na(ese), "Sin información", ese),
         fax = ifelse(is.na(fax), "Sin información", fax),
         gerente = ifelse(is.na(gerente), "Sin información", gerente),
         nivel = ifelse(is.na(nivel), "Sin información", nivel),
         caracter = ifelse(is.na(caracter), "Sin información", caracter),
         dv = ifelse(is.na(dv), "Sin información", dv),
         telefono_adicional = ifelse(is.na(telefono_adicional), "Sin información", telefono_adicional),
         email_adicional = ifelse(is.na(email_adicional), "Sin información", email_adicional),
         rep_legal = ifelse(is.na(rep_legal), "Sin información", rep_legal),
         direccion = ifelse(is.na(direccion), "Sin información", direccion),
         telefono = ifelse(is.na(telefono), "Sin información", telefono),
         email = ifelse(is.na(email), "Sin información", email),
         numero_sede_principal = ifelse(is.na(numero_sede_principal), "Sin información", numero_sede_principal),
         nits_nit = as.numeric(nits_nit)) 

clpr_nombre_spread <- Prestadores %>% 
  group_by(municipio_prestador, depa_nombre, muni_nombre, clpr_nombre) %>% 
  summarise(Total = n()) %>% 
  spread(key=clpr_nombre, value=Total)

clase_persona_spread <- Prestadores %>% 
  group_by(municipio_prestador, depa_nombre, muni_nombre, clase_persona) %>% 
  summarise(Total = n()) %>% 
  spread(clase_persona, Total) 

naju_nombre_spread <- Prestadores %>% 
  group_by(municipio_prestador, depa_nombre, muni_nombre, naju_nombre) %>% 
  summarise(Total = n()) %>% 
  spread(naju_nombre, Total) 

nivel_spread <- Prestadores %>% 
  group_by(municipio_prestador, depa_nombre, muni_nombre, nivel) %>% 
  summarise(Total = n()) %>% 
  spread(nivel, Total) 

datos_nacimientos_2022 <- tabla_nacimientos_dane2022 %>% 
  group_by(COD_DPTO, COD_MUNIC) %>% 
  summarise(Nacimientos_2022 = n()) %>% 
  mutate(COD_MUNIC = ifelse(nchar(COD_MUNIC) == 2, paste0("0",COD_MUNIC),
                            ifelse(nchar(COD_MUNIC) == 1, paste0("00",COD_MUNIC),COD_MUNIC))) %>% 
  mutate(municipio_prestador = paste0(COD_DPTO, COD_MUNIC)) %>%
  mutate(municipio_prestador = as.numeric(municipio_prestador)) %>%
  select(municipio_prestador, everything())
  
Datos <- clpr_nombre_spread %>% 
  left_join(clase_persona_spread) %>% 
  left_join(nivel_spread) %>% 
  left_join(naju_nombre_spread) %>% 
  left_join(Municipios, by = c("municipio_prestador" = "Depmun")) %>% 
  left_join(datos_nacimientos_2022 %>% select(municipio_prestador, Nacimientos_2022)) %>% 
  rename(nivel_1 = `1`,
         nivel_2 = `2`,
         nivel_3 = `3`) %>% 
  rename_with(~ gsub(" ", "_", .), contains(" ")) %>% # La variables con espacios se renombran con guión.
  ungroup() %>% 
  select(-depa_nombre) %>% 
  rename(depa_nombre = Departamento) %>% 
  mutate(depa_nombre = ifelse(is.na(depa_nombre),"CAUCA",depa_nombre)) %>% 
  select(municipio_prestador, depa_nombre, muni_nombre, everything()) %>% 
  ungroup() %>% 
  mutate_if(is.numeric, ~ replace_na(., 0)) %>%  # Reemplazar todos los NA por cero en las variables numéricas.
  mutate(Tasa_servicio_nivel_1_xc100milhab = nivel_1 / Poblacion * 100000,
         Tasa_servicio_nivel_2_xc100milhab = nivel_2 / Poblacion * 100000,
         Tasa_servicio_nivel_3_xc100milhab = nivel_3 / Poblacion * 100000,
         Cobertura_nivel_1 = Superficie / nivel_1,
         Cobertura_nivel_2 = Superficie / nivel_2,
         Cobertura_nivel_3 = Superficie / nivel_3)

nombres_columnas <- colnames(Datos)

nombres_columnas_modificados <- gsub(" ", "_", nombres_columnas)

names(Datos) <- nombres_columnas_modificados

nombres_modificados <- make.names(names(Datos))

names(Datos) <- nombres_modificados

datos_departamento <- Datos %>% 
  group_by(depa_nombre) %>% 
  summarise(IPS_promedio = mean(`Instituciones_Prestadoras_de_Servicios_de_Salud_._IPS`, na.rm = TRUE),
            Profesional_Independiente_promedio = mean(Profesional_Independiente, na.rm = TRUE),
            Transporte_promedio = mean(Transporte_Especial_de_Pacientes, na.rm = TRUE),
            nivel_1_promedio = mean(nivel_1, na.rm = TRUE),
            nivel_2_promedio = mean(nivel_2, na.rm = TRUE),
            nivel_3_promedio = mean(nivel_3, na.rm = TRUE),
            Tasa_servicio_nivel_1_xc100milhab_promedio = mean(Tasa_servicio_nivel_1_xc100milhab, na.rm = TRUE),
            Tasa_servicio_nivel_2_xc100milhab_promedio = mean(Tasa_servicio_nivel_2_xc100milhab, na.rm = TRUE),            
            Tasa_servicio_nivel_3_xc100milhab_promedio = mean(Tasa_servicio_nivel_3_xc100milhab, na.rm = TRUE),
            Cobertura_servicio_nivel_1 = mean(Cobertura_nivel_1, na.rm = TRUE),
            Cobertura_servicio_nivel_2 = mean(Cobertura_nivel_2, na.rm = TRUE),
            Cobertura_servicio_nivel_3 = mean(Cobertura_nivel_3, na.rm = TRUE))

# ++++++++++++++++++++++++++++++++++++++ #
#----6. Anàlisis exploratorio de datos----
# ++++++++++++++++++++++++++++++++++++++ #

create_report(Datos)

# ++++++++++++++++++++++++++++++ #
#----7. Análisis descriptivos----
# ++++++++++++++++++++++++++++++ #

#---- 7.1. Medidas de tendencia central ----

media_nivel2 <- mean(Datos$nivel_2) # Promedio prestadores nivel 2
media_nivel2
mediana_nivel2 <- median(Datos$nivel_2) # Mediana prestadores nivel 2
mediana_nivel2
moda_nivel1 <- names(sort(table(Datos$nivel_1), decreasing = TRUE)[1]) # Moda prestadores nivel 1
moda_nivel1

#---- 7.2. Medidas de dispersión ----

desviacion_estandar_nivel1 <- sd(Datos$nivel_1) # Desviación estándar prestadores nivel 1
desviacion_estandar_nivel1
range_intercuartilico_nivel1 <- range(Datos$nivel_1) # Rango intercuartílico prestadores nivel 1 
range_intercuartilico_nivel1

#---- 7.3. Visualización de datos ----

Plot_01 <- Datos %>% 
  ggplot(aes(x = Instituciones_Prestadoras_de_Servicios_de_Salud_._IPS,
                                y = Poblacion)) +
  geom_point(shape = "circle filled", size = 1.5, colour = "#EF562D") +
  labs(x = "Instituciones Prestadoras de Salud", y = "Población",
    title = "Relación entre Población y Número de Instituciones Prestadoras de Salud",
    caption = "Creado por Inés Enai Escobar Navas") + 
    theme_gray()

Plot_01

ggsave(Plot_01, file="RelacionentrePoblacionyNumeroInstitucionesPrestadorasdeSalud.bmp", width = 13.66, height = 7.68, dpi = 150)

Plot_02 <- Datos %>% 
  ggplot(aes(x = Profesional_Independiente, y = Poblacion)) +
  geom_point(shape = "circle open", size = 1.5, colour = "#228B22") +
  labs(x = "Profesional independiente", y = "Población",
       title = "Relación entre Población y Profesionales Independentes",
       caption = "Creado por Inés Enai Escobar Navas") +
  theme_gray()

Plot_02

ggsave(Plot_02, file="RelacionentrePoblacionyProfesionalesIndependentes.bmp", width = 13.66, height = 7.68, dpi = 150)

Plot_03 <- Datos %>% 
  group_by(depa_nombre) %>% 
  summarise(Instituciones_Prestadoras_de_Servicios_de_Salud_._IPS = mean(Instituciones_Prestadoras_de_Servicios_de_Salud_._IPS)) %>% 
  ggplot(aes(x = reorder(depa_nombre, Instituciones_Prestadoras_de_Servicios_de_Salud_._IPS))) + 
  geom_text(stat = "count", aes(label = ..count..)) +   # Agregar etiquetas
  labs(x = "Departamento",y = "Número de instituciones prestadoras de salud",
       title = "Distribución promedio de instituciones prestadoras de salud por departamento",
       caption = "Creado por Inés Enai Escobar Navas") +
  coord_flip() +
  theme_minimal()

Plot_03

ggsave(Plot_03, file="DistribucionPromediodeInstitucionesPrestadorasdeSaludporDepartamento.bmp", width = 13.66, height = 7.68, dpi = 150)

Plot_04 <- Datos %>% 
  ggplot(aes(x = depa_nombre, y = Profesional_Independiente)) +
  geom_point(shape = "circle open", size = 1.5, colour = "#A850BD") +
  labs(x = "Departamento", y = "Número de profesionales independientes",
       title = "Distribución de profesionales independientes por departamento",
       caption = "Creado por Inés Enai Escobar Navas") +
  coord_flip() +
  theme_minimal()
         
Plot_04

ggsave(Plot_04, file="DistribuciondeProfesionalesIndependientesporDepartamento.bmp", width = 13.66, height = 7.68, dpi = 150)

Plot_05 <- Datos %>% 
  ggplot(aes(x = Poblacion, y = nivel_1)) +
  geom_point(shape = "circle open", size = 1.5, colour = "#A43939") +
  labs(x = "Población", y = "Prestadores nivel 1",
       title = "Relación entre población y prestadores de nivel 1",
       caption = "Creado por Inés Enai Escobar Navas") +
  coord_flip() +
  theme_gray()
         
Plot_05

ggsave(Plot_05, file="RelacionentrePoblacionyPrestadoresdeNivel_1.bmp", width = 13.66, height = 7.68, dpi = 150)

Plot_06 <- Datos %>% 
  ggplot(aes(x = Poblacion, y = nivel_2)) +
  geom_point(shape = "circle open", size = 1.5, colour = "#9349A4") +
  labs(x = "Población", y = "Prestadores nivel 2",
       title = "Relación entre población y prestadores de nivel 2",
       caption = "Creado por Inés Enai Escobar Navas") +
  coord_flip() +
  theme_minimal()
         
Plot_06

ggsave(Plot_06, file="RelacionentrePoblacionyPrestadoresdeNivel_2.bmp", width = 13.66, height = 7.68, dpi = 150)

Plot_07 <- Datos %>% 
  ggplot(aes(x = Poblacion, y = nivel_3)) +
  geom_point(shape = "circle open", size = 1.5, colour = "#228B22") +
  labs(x = "Población", y = "Prestadores nivel 3", title = "Relación entre población y prestadores de nivel 3",
       caption = "Creado por Inés Enai Escobar Navas") +
  coord_flip() +
  theme_linedraw()
         
Plot_07

ggsave(Plot_07, file="RelacionentrePoblacionyPrestadoresdeNivel_3.bmp", width = 13.66, height = 7.68, dpi = 150)

Plot_08 <- Datos %>% 
  ggplot(aes(x = Superficie, y = nivel_1)) +
  geom_point(shape = "circle open", size = 1.5, colour = "#EF562D") +
  labs(x = "Superficie", y = "Prestadores nivel 1",
       title = "Relación entre superficie y prestadores de nivel 1",
       caption = "Creado por Inés Enai Escobar Navas") +
  coord_flip() +
  theme_linedraw()
         
Plot_08

ggsave(Plot_08, file="RelacionentreSuperficieyPrestadoresdeNivel_1.bmp", width = 13.66, height = 7.68, dpi = 150)

Plot_09 <- Datos %>% 
  ggplot(aes(x = Superficie, y = nivel_2)) +
  geom_point(shape = "circle open", size = 1.5, colour = "#FF69B4") +
  labs(x = "Superficie", y = "Prestadores nivel 2",
       title = "Relación entre superficie y prestadores de nivel 2",
       caption = "Creado por Inés Enai Escobar Navas") + 
  coord_flip() +
  theme_linedraw()
         
Plot_09

ggsave(Plot_09, file="RelacionentreSuperficieyPrestadoresdeNivel_2.bmp", width = 13.66, height = 7.68, dpi = 150)
         
Plot_10 <- Datos %>% 
  ggplot(aes(x = Superficie, y = nivel_3)) +
  geom_point(shape = "circle open", size = 1.5, colour = "#46337E") +
  labs(x = "Superficie", y = "Prestadores nivel 3", title = "Relación entre superficie y prestadores de nivel 3",
       caption = "Creado por Inés Enai Escobar Navas") +
  coord_flip() +
  theme_linedraw()
         
Plot_10
         
ggsave(Plot_10, file="RelacionentreSuperficieyPrestadoresdeNivel_3.bmp", width = 13.66, height = 7.68, dpi = 150)

# ---- Boxplot ----

boxplot(Datos$nivel_1, main = "Boxplot de Prestadores de nivel 1")

# ---- Histograma ----

Histograma <- ggplot(Datos, aes(x = nivel_1)) +   # Definir el conjunto de datos y la variable
  geom_histogram(fill = "skyblue", color = "black") +   # Agregar el histograma
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +   # Agregar etiquetas
  labs(title = "Histograma de prestadores de nivel 1 a nivel nacional", x = "prestadores de nivel 1", 
       y = "Frecuencia", caption = "Creado por Inés Enai Escobar Navas") +   # Etiquetas de los ejes y título
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(Datos$nivel_1), max(Datos$nivel_1), by = 1))   # Etiquetas en el eje x

ggsave(Histograma, file="Histograma_de_prestadores_de_nivel_1_a_nivel_nacional.bmp", width = 13.66, height = 7.68, dpi = 150)

# Resumen de frecuencias

tabla_frecuencias <- table(Datos$nivel_1)

# Identificación de outliers (datos atípicos)

# +++++++++++++++++++++++++++ #
#----8. Modelo de regresión----
# +++++++++++++++++++++++++++ #

#----8.1 Estimación modelo nacimientos ----

#----8.1.1. Aplicación Regresión Stepwise Backward ----

#----8.1.1.1. Paso 1 ----

glimpse(Datos)

m_01 <- lm(Nacimientos_2022 ~ Instituciones_Prestadoras_de_Servicios_de_Salud_._IPS + 
             Objeto_Social_Diferente_a_la_Prestaci.n_de_Servicios_de_Salud + Profesional_Independiente + 
             Transporte_Especial_de_Pacientes + JURIDICO + NATURAL + nivel_1 + nivel_2 +  nivel_3 + P.blica + Privada + 
             Superficie + Poblacion + Tasa_servicio_nivel_1_xc100milhab + Tasa_servicio_nivel_2_xc100milhab + 
             Tasa_servicio_nivel_3_xc100milhab + Cobertura_nivel_1 + Cobertura_nivel_2 + Cobertura_nivel_3, data = Datos)

m_01

summary(m_01)

#----8.1.1.2. Paso 2 ----

m_02 <- lm(Nacimientos_2022 ~ Instituciones_Prestadoras_de_Servicios_de_Salud_._IPS + 
             Objeto_Social_Diferente_a_la_Prestaci.n_de_Servicios_de_Salud + Profesional_Independiente + 
             Transporte_Especial_de_Pacientes + JURIDICO + NATURAL + nivel_1 + nivel_2 +  nivel_3 + P.blica + Privada + 
             Superficie + Poblacion + Tasa_servicio_nivel_1_xc100milhab + Tasa_servicio_nivel_2_xc100milhab + 
             Tasa_servicio_nivel_3_xc100milhab + Cobertura_nivel_1, data = Datos)

m_02

summary(m_02)

#----8.1.1.3. Paso 3 ----

m_03 <- lm(Nacimientos_2022 ~ Instituciones_Prestadoras_de_Servicios_de_Salud_._IPS + 
             Objeto_Social_Diferente_a_la_Prestaci.n_de_Servicios_de_Salud + Profesional_Independiente + 
             Transporte_Especial_de_Pacientes + JURIDICO + NATURAL + nivel_1 + nivel_2 +  nivel_3 + P.blica + Privada + 
             Superficie + Poblacion + Tasa_servicio_nivel_1_xc100milhab + Tasa_servicio_nivel_2_xc100milhab + 
             Tasa_servicio_nivel_3_xc100milhab, data = Datos)

m_03

summary(m_03)

#----8.1.1.4. Paso 4 ----

m_04 <- lm(Nacimientos_2022 ~ Transporte_Especial_de_Pacientes + JURIDICO +  nivel_1 + nivel_2 +
             Superficie + Poblacion + Tasa_servicio_nivel_2_xc100milhab + 
             Tasa_servicio_nivel_3_xc100milhab, data = Datos)

m_04

summary(m_04)

#----8.1.1.5. Paso 5 ----

m_05 <- lm(Nacimientos_2022 ~ Transporte_Especial_de_Pacientes + JURIDICO + nivel_2 +
             Superficie + Poblacion + Tasa_servicio_nivel_2_xc100milhab, data = Datos)

m_05

summary(m_05) 

#----8.1.1.6. Paso 6 ----

m_06 <- lm(Nacimientos_2022 ~ Transporte_Especial_de_Pacientes + JURIDICO + nivel_2 +
             Superficie + Poblacion, data = Datos)

m_06

summary(m_06) # Se quita la variable "Tasa_servicio_nivel_2_xc100milhab" toda vez porque la base original no incluye información
              # de Guachené, Cauca en la base Municipios, haciendo que se reduzca el vector de residuos en n-1.

#----8.1.1.6. Modelo provisional proyectado - versión 1----

Datos$Pronostico <- m_06$coefficients[1] + (Datos$Transporte_Especial_de_Pacientes * m_06$coefficients[2]) + 
  (Datos$JURIDICO * m_06$coefficients[3]) + (Datos$nivel_2 * m_06$coefficients[4]) +
  (Datos$Superficie * m_06$coefficients[5]) + (Datos$Poblacion * m_06$coefficients[6])

plot01 <- Datos %>% 
  ggplot(aes(x = depa_nombre)) + 
  geom_point(aes(y = Pronostico, colour = "Prónostico")) + 
  geom_point(aes(y = Nacimientos_2022, colour = "Nacimientos 2022")) +
  coord_flip() + 
  ylab("Valor") 
    
plot01

plot01 <- Datos %>% 
  group_by(depa_nombre) %>% 
  summarise(Nacimientos_2022 = mean(Nacimientos_2022, na.rm = TRUE),
            Pronostico = mean(Pronostico, na.rm = TRUE)) %>% 
  ggplot(aes(x = depa_nombre)) + 
  geom_bar(aes(y = reorder(Pronostico), colour = "Prónostico"), stat = "identity") +
  coord_flip()
  
#----8.1.1.6.1. Cumplimiento de supuestos del modelo de regresión modelo provisional proyectado ----

#----8.1.1.6.2.1. Linealidad ----

Grafico01 <- ggplot(data = Datos, aes(Transporte_Especial_de_Pacientes, m_06$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()

Grafico02 <- ggplot(data = Datos, aes(JURIDICO, m_06$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()

Grafico03 <- ggplot(data = Datos, aes(nivel_2, m_06$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()

Grafico04 <- ggplot(data = Datos, aes(Superficie, m_06$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()

Grafico05 <- ggplot(data = Datos, aes(Poblacion, m_06$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()

Grafico <- grid.arrange(Grafico01, Grafico02, Grafico03, Grafico04, Grafico05)

Grafico

plot(m_06$fitted.values, resid(m_06))
abline(h = 0, col = "red")

mean(m_06$residuals) # Si la media de los residuos es cero hay linealidad.

linearHypothesis(m_06, c("Transporte_Especial_de_Pacientes=0", "JURIDICO=0", "nivel_2=0", "Superficie=0", "Poblacion=0"))

# La prueba gráfica evidencia que en los diversos regresores no expresan ninguna tendencia. Adicionalmente con la prueba de 
# Lack of fit muestra que los coeficientes de los regresores son distintos de cero sumado a que la media de los residuos es cero.

# Hay linealidad para todas las variables independientes.

#----8.1.1.6.2.2. Normalidad: Normalidad de los residuos ----

qqnorm(m_06$residuals)
qqline(m_06$residuals)

# Hay normalidad de los residuos.

#----8.1.1.6.2.3. Homocedasticidad: Variabilidad de los residuos ----

plot(m_06$residuals ~ m_06$fitted)
plot(abs(m_06$residuals) ~ m_06$fitted)

# Hay variabilidad de los residuos

#----8.1.1.6.2.4. Independencia: Independencia de los residuos ----

plot(m_06$residuals)

# Hay independencia de los residuos

#----8.1.1.6.2.5. Multicolineadlidad ----

Dependientes <- Datos %>% 
  select(Transporte_Especial_de_Pacientes, JURIDICO, nivel_2, Superficie, Poblacion)

cor(Dependientes)

car::vif(m_06) # Se aplica test de Factor de Inflaciòn de la Varianza.

# Puesto que el VIF para al menos una las variables independientes es mayor a 5, se puede afirmar que hay alg{un nivel de
# multicolinealidad.

multic_01 <- lm(Transporte_Especial_de_Pacientes ~ JURIDICO + nivel_2 + Superficie + Poblacion, data = Datos)

summary(multic_01)

multic_02 <- lm(JURIDICO ~ nivel_2 + Superficie + Poblacion + Transporte_Especial_de_Pacientes, data = Datos)

summary(multic_02)

multic_03 <- lm(nivel_2 ~ Superficie + Poblacion + Transporte_Especial_de_Pacientes + JURIDICO, data = Datos)

summary(multic_03)

multic_04 <- lm(Superficie ~ Poblacion + Transporte_Especial_de_Pacientes + JURIDICO + nivel_2, data = Datos)

summary(multic_04)

multic_05 <- lm(Poblacion ~ Transporte_Especial_de_Pacientes + JURIDICO + nivel_2 + Superficie, data = Datos)

summary(multic_05)

# Al aplicar regresiones auxiliares de Klein se observa que el R^2 ajustado de la regresiòn original
# m_04 (0.9506) es mayor al R^2 en las regresiones auxiliares Transporte_Especial_de_Pacientes (0.6478),
# JURIDICO (0.9258), nivel_2 (0.2287), Superficie (0.004014) y Poblacion (0.8989) , por lo tanto 
# al modelo m_06 tiene multicolinealidad con impacto en el modelo, lo cual implica que 
# si incide en su capacidad de predicción.

#----8.1.1.7. Paso 7 ----

m_07_01 <- lm(Nacimientos_2022 ~ JURIDICO + nivel_2 +
             Superficie + Poblacion, data = Datos)

m_07_01

summary(m_07_01)

m_07_02 <- lm(Nacimientos_2022 ~ Transporte_Especial_de_Pacientes + nivel_2 +
                Superficie + Poblacion, data = Datos)

m_07_02

summary(m_07_02)

m_07_03 <- lm(Nacimientos_2022 ~ Transporte_Especial_de_Pacientes + JURIDICO + 
                Superficie + Poblacion, data = Datos)

m_07_03

summary(m_07_03)

m_07_04 <- lm(Nacimientos_2022 ~ Transporte_Especial_de_Pacientes + JURIDICO + nivel_2 +
                Poblacion, data = Datos)

m_07_04

summary(m_07_04)

m_07_05 <- lm(Nacimientos_2022 ~ Transporte_Especial_de_Pacientes + JURIDICO + nivel_2 +
             Superficie, data = Datos)

m_07_05

summary(m_07_05)

m_07 <- m_07_04

# Dentro de los cinco modelos, el modelo asociado m_07_04 tiene con menos regresores el R^2 más alto (0.9503). No obstante,
# el modelo m_07_01 con R^2 del 0.9489 se comporta bastante bien y podría ser una opción bastaten buena si el inicial no pasa las pruebas.

#----8.1.1.7.1. Modelo provisional proyectado - versión 2----

Datos$Pronostico <- m_07$coefficients[1] + (Datos$Transporte_Especial_de_Pacientes * m_07$coefficients[2]) + 
  (Datos$JURIDICO * m_07$coefficients[3]) + (Datos$nivel_2 * m_07$coefficients[4]) + (Datos$Poblacion * m_07$coefficients[5])

plot01 <- Datos %>% 
  ggplot(aes(x = depa_nombre)) + 
  geom_point(aes(y = Pronostico, colour = "Prónostico")) + 
  geom_point(aes(y = Nacimientos_2022, colour = "Nacimientos 2022")) +
  coord_flip() + 
  ylab("Valor") 

plot01

#----8.1.1.7.2. Cumplimiento de supuestos del modelo de regresión modelo provisional proyectado ----

#----8.1.1.7.2.1. Linealidad ----

Grafico01 <- ggplot(data = Datos, aes(Transporte_Especial_de_Pacientes, m_07$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()

Grafico02 <- ggplot(data = Datos, aes(JURIDICO, m_07$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()

Grafico03 <- ggplot(data = Datos, aes(nivel_2, m_07$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()

Grafico04 <- ggplot(data = Datos, aes(Poblacion, m_07$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()

Grafico <- grid.arrange(Grafico01, Grafico02, Grafico03, Grafico04)

Grafico

plot(m_07$fitted.values, resid(m_06))
abline(h = 0, col = "red")

mean(m_07$residuals) # Si la media de los residuos es cero hay linealidad.

linearHypothesis(m_06, c("Transporte_Especial_de_Pacientes=0", "JURIDICO=0", "nivel_2=0", "Superficie=0", "Poblacion=0"))

# La prueba gráfica evidencia que en los diversos regresores no expresan ninguna tendencia. Adicionalmente con la prueba de 
# Lack of fit muestra que los coeficientes de los regresores son distintos de cero sumado a que la media de los residuos es cero.

# Hay linealidad para todas las variables independientes.

#----8.1.1.7.2.2. Normalidad: Normalidad de los residuos ----

qqnorm(m_07$residuals)
qqline(m_07$residuals)

# Hay normalidad de los residuos.

#----8.1.1.7.2.3. Homocedasticidad: Variabilidad de los residuos ----

plot(m_07$residuals ~ m_07$fitted)
plot(abs(m_07$residuals) ~ m_07$fitted)

# Hay variabilidad de los residuos

#----8.1.1.7.2.4. Independencia: Independencia de los residuos ----

plot(m_07$residuals)

# Hay independencia de los residuos

#----8.1.1.7.2.5. Multicolineadlidad ----

Dependientes <- Datos %>% 
  select(Transporte_Especial_de_Pacientes, JURIDICO, nivel_2, Poblacion)

cor(Dependientes)

car::vif(m_06) # Se aplica test de Factor de Inflaciòn de la Varianza.

# Puesto que el VIF para al menos una las variables independientes es mayor a 5, se puede afirmar que hay alg{un nivel de
# multicolinealidad.

multic_01 <- lm(Transporte_Especial_de_Pacientes ~ JURIDICO + nivel_2 + Poblacion, data = Datos)

summary(multic_01)

multic_02 <- lm(JURIDICO ~ nivel_2 + Poblacion + Transporte_Especial_de_Pacientes, data = Datos)

summary(multic_02)

multic_03 <- lm(nivel_2 ~ Poblacion + Transporte_Especial_de_Pacientes + JURIDICO, data = Datos)

summary(multic_03)

multic_04 <- lm(Poblacion ~ Transporte_Especial_de_Pacientes + JURIDICO + nivel_2, data = Datos)

summary(multic_04)

# Al aplicar regresiones auxiliares de Klein se observa que el R^2 ajustado de la regresiòn original
# m_04 (0.9506) es mayor al R^2 en las regresiones auxiliares Transporte_Especial_de_Pacientes (0.6478),
# JURIDICO (0.9258), nivel_2 (0.2287), Superficie (0.004014) y Poblacion (0.8989) , por lo tanto 
# al modelo m_06 tiene multicolinealidad con impacto en el modelo, lo cual implica que 
# si incide en su capacidad de predicción.

m_07 <- m_07_01

# Dentro de los cinco modelos, el modelo asociado m_07_04 tiene con menos regresores el R^2 más alto (0.9503). No obstante,
# el modelo m_07_01 con R^2 del 0.9489 se comporta bastante bien y podría ser una opción bastaten buena si el inicial no pasa las pruebas.

#----8.1.1.8. Paso 8 ----

m_08_01 <- lm(Nacimientos_2022 ~ nivel_2 +
                Superficie + Poblacion, data = Datos)

m_08_01

summary(m_08_01)

m_08_02 <- lm(Nacimientos_2022 ~ JURIDICO + Superficie + Poblacion, data = Datos)

m_08_02

summary(m_08_02)

m_08_03 <- lm(Nacimientos_2022 ~ JURIDICO + nivel_2 + Poblacion, data = Datos)

m_08_03

summary(m_08_03)

m_08_04 <- lm(Nacimientos_2022 ~ JURIDICO + nivel_2 + Superficie , data = Datos)

m_08_04

summary(m_08_04)

m_08 <- m_08_03

#----8.1.1.8.1. Modelo provisional proyectado - versión 3----

Datos$Pronostico <- m_08$coefficients[1] + (Datos$JURIDICO * m_07$coefficients[2]) + 
  (Datos$nivel_2 * m_07$coefficients[3]) + (Datos$Poblacion * m_07$coefficients[4])

plot01 <- Datos %>% 
  ggplot(aes(x = depa_nombre)) + 
  geom_point(aes(y = Pronostico, colour = "Prónostico")) + 
  geom_point(aes(y = Nacimientos_2022, colour = "Nacimientos 2022")) +
  coord_flip() + 
  ylab("Valor") 

plot01

#----8.1.1.8.2. Cumplimiento de supuestos del modelo de regresión modelo provisional proyectado ----

#----8.1.1.8.2.1. Linealidad ----

Grafico01 <- ggplot(data = Datos, aes(JURIDICO, m_08$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()

Grafico02 <- ggplot(data = Datos, aes(nivel_2, m_08$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()

Grafico03 <- ggplot(data = Datos, aes(Poblacion, m_08$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()

Grafico <- grid.arrange(Grafico01, Grafico02, Grafico03)

Grafico

plot(m_08$fitted.values, resid(m_08))
abline(h = 0, col = "red")

mean(m_08$residuals) # Si la media de los residuos es cero hay linealidad.

linearHypothesis(m_08, c("JURIDICO=0", "nivel_2=0", "Poblacion=0"))

# La prueba gráfica evidencia que en los diversos regresores no expresan ninguna tendencia. Adicionalmente con la prueba de 
# Lack of fit muestra que los coeficientes de los regresores son distintos de cero sumado a que la media de los residuos es cero.

# Hay linealidad para todas las variables independientes.

#----8.1.1.8.2.2. Normalidad: Normalidad de los residuos ----

qqnorm(m_08$residuals)
qqline(m_08$residuals)

# Hay normalidad de los residuos.

#----8.1.1.8.2.3. Homocedasticidad: Variabilidad de los residuos ----

plot(m_08$residuals ~ m_08$fitted)
plot(abs(m_08$residuals) ~ m_08$fitted)

# Hay variabilidad de los residuos

#----8.1.1.8.2.4. Independencia: Independencia de los residuos ----

plot(m_08$residuals)

# Hay independencia de los residuos

#----8.1.1.8.2.5. Multicolineadlidad ----

Dependientes <- Datos %>% 
  select(JURIDICO, nivel_2, Poblacion)

cor(Dependientes)

car::vif(m_08) # Se aplica test de Factor de Inflaciòn de la Varianza.

# Puesto que el VIF para al menos una las variables independientes es mayor a 5, se puede afirmar que hay alg{un nivel de
# multicolinealidad.

multic_01 <- lm(JURIDICO ~ nivel_2 + Poblacion, data = Datos)

summary(multic_01)

multic_02 <- lm(nivel_2 ~ JURIDICO + Poblacion, data = Datos)

summary(multic_02)

multic_03 <- lm(Poblacion ~ JURIDICO + nivel_2, data = Datos)

summary(multic_03)

# Al aplicar regresiones auxiliares de Klein se observa que el R^2 ajustado de la regresiòn original
# m_04 (0.9506) es mayor al R^2 en las regresiones auxiliares Transporte_Especial_de_Pacientes (0.6478),
# JURIDICO (0.9258), nivel_2 (0.2287), Superficie (0.004014) y Poblacion (0.8989) , por lo tanto 
# al modelo m_06 tiene multicolinealidad con impacto en el modelo, lo cual implica que 
# si incide en su capacidad de predicción.

#----8.1.1.9. Paso 9 ----

m_09_01 <- lm(Nacimientos_2022 ~ nivel_2 + Poblacion, data = Datos)

m_09_01

summary(m_09_01)

m_09_02 <- lm(Nacimientos_2022 ~ JURIDICO + Poblacion, data = Datos)

m_09_02

summary(m_09_02)

m_09_03 <- lm(Nacimientos_2022 ~ JURIDICO + nivel_2, data = Datos)

m_09_03

summary(m_09_03)

m_09 <- m_09_01

#----8.1.1.9.1. Modelo provisional proyectado - versión 4----

Datos$Pronostico <- m_09$coefficients[1] + (Datos$nivel_2 * m_09$coefficients[2]) + 
  (Datos$Poblacion * m_09$coefficients[3])

plot01 <- Datos %>% 
  group_by(depa_nombre) %>% 
  summarise(Pronostico = mean(Pronostico),
            Nacimientos_2022 = mean(Nacimientos_2022)) %>% 
  ggplot(aes(x = reorder(depa_nombre, Nacimientos_2022))) + 
  geom_point(aes(y = Pronostico, colour = "Prónostico")) + 
  geom_point(aes(y = Nacimientos_2022, colour = "Nacimientos 2022")) +
  coord_flip() + 
  labs(title = "Comparación entre los datos observados y pronosticados del modelo para nacimientos en el año 2022", 
       x = "Departamento", y = "Valor")    # Etiquetas de los ejes y título
  
plot01

ggsave(plot01, file="ComparacionDatosObservadosVSModeloNacimientos2022.bmp", width = 13.66, height = 7.68, dpi = 150)

#----8.1.1.9.2. Cumplimiento de supuestos del modelo de regresión modelo provisional proyectado ----

#----8.1.1.9.2.1. Linealidad ----

Grafico01 <- ggplot(data = Datos, aes(nivel_2, m_09$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()

Grafico02 <- ggplot(data = Datos, aes(Poblacion, m_09$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()

Grafico <- grid.arrange(Grafico01, Grafico02)

Grafico

ggsave(Grafico, file="CumplimientoLinealidad.bmp", width = 13.66, height = 7.68, dpi = 150)

bmp(file="ValoresAjustadosResiduosModeloNacimientos2022.bmp",width=1366, height=768, res=100)
plot(m_09$fitted.values, resid(m_09))
abline(h = 0, col = "red")
dev.off()


mean(m_09$residuals) # Si la media de los residuos es cero hay linealidad.

linearHypothesis(m_09, c("nivel_2=0", "Poblacion=0"))

# La prueba gráfica evidencia que en los diversos regresores no expresan ninguna tendencia. Adicionalmente con la prueba de 
# Lack of fit muestra que los coeficientes de los regresores son distintos de cero sumado a que la media de los residuos es cero.

# Hay linealidad para todas las variables independientes.

#----8.1.1.9.2.2. Normalidad: Normalidad de los residuos ----

bmp(file="NormalidadResiduosModeloNacimientos2022.bmp",width=1366, height=768, res=100)
qqnorm(m_09$residuals)
qqline(m_09$residuals)
dev.off()

# Hay normalidad de los residuos.

#----8.1.1.9.2.3. Homocedasticidad: Variabilidad de los residuos ----

bmp(file="HomocedasticidadResiduosModeloNacimientos2022.bmp",width=1366, height=768, res=100)
plot(m_09$residuals ~ m_09$fitted)
plot(abs(m_09$residuals) ~ m_09$fitted)
dev.off()

# Hay variabilidad de los residuos

#----8.1.1.9.2.4. Independencia: Independencia de los residuos ----

bmp(file="IndependenciaResiduosModeloNacimientos2022.bmp",width=1366, height=768, res=100)
plot(m_09$residuals)
dev.off()

# Hay independencia de los residuos

#----8.1.1.9.2.5. Multicolineadlidad ----

Dependientes <- Datos %>% 
  select(nivel_2, Poblacion)

cor(Dependientes)

car::vif(m_09) # Se aplica test de Factor de Inflación de la Varianza.

# Puesto que el VIF para cada una de las variables independientes es menor a 5, se puede afirmar 
# que no hay multicolinealidad.

#----8.2. Proyecciones ------

datos_proyecciones <- Datos %>% 
  select(municipio_prestador, depa_nombre, muni_nombre, nivel_2, Poblacion, Nacimientos_2022, Pronostico)

# ++++++++++++++++++++++++ #
#----9. Exportar salidas----
# ++++++++++++++++++++++++ #

FIV <- as.data.table(car::vif(m_09),keep.rownames=TRUE)
names(FIV) <- c("VariableDependiente","FactorInflacionVarianza")

Listado <- list("Tabla pronóstico" = datos_proyecciones,
                "Matriz correlación" = cor(Dependientes),
                "FactorInflaciónVarianza" = FIV)
write.xlsx(Listado,"TablaPronóstico_Modelo_Nacimientos2022.xlsx", overwrite = TRUE)
