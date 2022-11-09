


c<- readxl::read_xlsx("D:/ray/shiny/empleo/www/imss/Nacionalpt.xlsx")

library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(tidyverse)
library(tidyr)
library(spdep)
library(rgdal)
library(rgeos)



names(c)[names(c) == 'id'] <- 'CVE_ENT'

c$CVE_ENT<- as.character(c$CVE_ENT)
c$CVE_ENT[c$CVE_ENT == "1"] <- "01"
c$CVE_ENT[c$CVE_ENT == "2"] <- "02"
c$CVE_ENT[c$CVE_ENT == "3"] <- "03"
c$CVE_ENT[c$CVE_ENT == "4"] <- "04"
c$CVE_ENT[c$CVE_ENT == "5"] <- "05"
c$CVE_ENT[c$CVE_ENT == "6"] <- "06"
c$CVE_ENT[c$CVE_ENT == "7"] <- "07"
c$CVE_ENT[c$CVE_ENT == "8"] <- "08"
c$CVE_ENT[c$CVE_ENT == "9"] <- "09"

a <- read_sf("C:/Users/rault/OneDrive/Desktop/suiza1/suiza/sh/est.shp")

g <- merge(a,c)


m <- g %>% filter(año==2001, mes==1)

ggplot() +  geom_sf(data = m, mapping = aes(fill = puestos))  +scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  annotation_scale(location = "bl", width_hint = 0.5)+
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  coord_sf()


