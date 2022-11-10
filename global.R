# global

# Librería

library(shiny) # libreria base
library(shinydashboard) # apariencia del shiny
library(tidyverse) # operar objetos
library(ggplot2)
library(ggthemes) # temas para ggplot
library(viridis)
library(shinyWidgets)
library(plotly)
library(sf)
library(leaflet)
library(dashboardthemes)
library(plotly)
library(tmap)
library(haven)
library(viridis)
library(readxl)
library(RColorBrewer)
library(haven)
library(zoo)
library(png)


####SALARIO ENOE
enoe_sal_ent_trim <- read.csv("www/enoe_sal_ent_trim.csv")
enoe_sal_ent_trim$date <- paste(enoe_sal_ent_trim$year,"-", enoe_sal_ent_trim$mes,"-","01",sep="")
enoe_sal_ent_trim$date <- as.yearqtr(enoe_sal_ent_trim$date, format = "%Y-%m-%d")
enoe_sal_gen_trim <- enoe_sal_ent_trim %>%  group_by(date, sex) %>% summarise(saltrim=mean(saltrim)) 
enoe_sal_gen_trim <- spread(enoe_sal_gen_trim, key="sex", value ="saltrim")
colnames(enoe_sal_gen_trim) <- c("date","Male","Female")
enoe_sal_gen_trim$Gap <- enoe_sal_gen_trim$Male - enoe_sal_gen_trim$Female
enoe_sal_gen_trim <- gather(enoe_sal_gen_trim, key="sex", value="saltrim", 2:4)
  
# Bases de datos para glosarios
glosario_pob<-read.csv("www/glosario_pob.csv", encoding = "UTF-8")
glosario_nat<-read.csv("www/glosario_nat.csv",encoding = "UTF-8")
glosario_mor<-read.csv("www/glosario_mor.csv",encoding = "UTF-8")
glosario_mat<-read.csv("www/glosario_mat.csv",encoding = "UTF-8")
glosario_div<-read.csv("www/glosario_div.csv",encoding = "UTF-8")
glosario_intern<-read.csv("www/glosario_internacional.csv",encoding = "UTF-8")
glosario_intern_eco<-read.csv("www/glosario_internacional_econ.csv",encoding = "UTF-8")

colnames(glosario_pob) <- c("concepto", "def")
colnames(glosario_nat) <- c("concepto", "def")
colnames(glosario_mor) <- c("concepto", "def")
colnames(glosario_mat) <- c("concepto", "def")
colnames(glosario_div) <- c("concepto", "def")
colnames(glosario_intern) <- c("concepto", "def")
colnames(glosario_intern_eco) <- c("concepto", "def")

####  Base IMSS
 base1 <- read.csv(file = 'www/salario.csv', encoding = "UTF-8")
 base1$Salario<- as.numeric(base1$Salario)
 base1mean <- base1 %>% group_by(year,Desc.Sexo) %>%  summarise(salario=mean(Salario,na.rm=TRUE), sd=sd(Salario, na.rm = TRUE))
 
 ###salariomedio página wage
salmed <- read.csv(file = 'www/dist/salmed.csv')
salmed$sex <- as.character(salmed$sex)
salmed$educ <- as.character(salmed$educ)
 
####color blind
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
 
###BASE MAPA muni



base  <- read.csv("www/base_muni.csv")
shape_file <- read_sf("www/municd/municd.shp")
shape_file$CVE_MUN <- as.numeric(shape_file$CVE_MUN)
shape_file$CVE_ENT <- as.numeric(shape_file$CVE_ENT)
mex_map <- shape_file %>% left_join(base, by = c( "CVE_ENT"="clave","CVE_MUN"="mun"))
mex_map$ent <- as.character(mex_map$ent)
mex_map<- mex_map %>% filter(!is.na(year), !is.na(ent))


###BASE MAPA est

baseest <- read.csv("www/gg_est.csv")

shape_file <- read_sf("www/est/destdv250k_2gw/destdv250k_2gw.shp")
shape_file$NUM_EDO <- as.numeric(shape_file$NUM_EDO)
mex_map2 <- shape_file %>% left_join(baseest, by = c("NUM_EDO"="clave"))
mex_map2$ent <- as.character(mex_map2$ent)
mex_map2$var <- as.character(mex_map2$var)
mex_map2 <- mex_map2 %>% filter(!is.na(year), !is.na(ent))

##working tab

employ  <- read.csv("www/part.csv")
employ <- employ %>% filter(var!="Hours Worked")
employ$var <- as.character(employ$var)

### hours worked 
hours <- read.csv("www/horas.csv")
hours$var <- as.character(hours$var)
hours$categoria <- as.character(hours$categoria)


###Econometrics 

qreg <- read.csv("www/qreg.csv")



###Scatter

scat <- read.csv("www/covid_ent.csv")
scat$var <- as.character(scat$var)

### table econometircs

qregtab <- as.data.frame(matrix(c("p25", "p50","p75","p99","6.86","1.75","1.67","-7.24","-18.24","-5.78","-5.17","-38.66","12.72","8.91","7.66","83.05"),ncol=4, nrow = 4))
colnames(qregtab)<- c("Quantile","2019","2020", "2021")
