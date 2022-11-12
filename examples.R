library(plotly)

enoe_sal_ent_trim$date <- paste(enoe_sal_ent_trim$year,"-", enoe_sal_ent_trim$mes,"-","01",sep="")

enoe_sal_ent_trim$date <- as.yearqtr(enoe_sal_ent_trim$date, format = "%Y-%m-%d")

enoe_sal_gen_trim <- enoe_sal_ent_trim %>%  group_by(date, sex) %>% summarise(saltrim=mean(saltrim))



n1 <- ggplot(enoe_sal_gen_trim, aes(x = date, y=saltrim, group=sex, color=sex)) + geom_line() +
  geom_point() +  theme_bw() +xlab("Date") +
  ylab("Quarterly Wage, 2022 Constant prices") 
  

n1 <- ggplotly(n1)

n1


a <- read.csv("D:/ray/shiny/empleo/www/sal_por_educ.csv")




library(ggridges)
library(dplyr)
library(ggplot2)
library(haven)
library(zoo)
library(tidyverse)


df<- haven::read_dta("D:/empleo_enoe.dta")

df$mes[df$trim==1] <- "03"
df$mes[df$trim==2] <- "06"
df$mes[df$trim==3] <- "09"
df$mes[df$trim==4] <- "12"

df$date <- paste(df$year,"-", df$mes,"-","01",sep="")
df$date <- as.yearqtr(df$date, format = "%Y-%m-%d")
df$cs_p13_1 <- as.factor(df$cs_p13_1)



b<-df %>% filter(sex==2, cs_p13_1!=99,!is.na(cs_p13_1))
x<-df %>% filter(sex==1, cs_p13_1!=99,!is.na(cs_p13_1))

a2019m <-ggplot(b, aes(x =lnw_hr, y = cs_p13_1, fill = stat(x))) +
  geom_density_ridges_gradient() +
  scale_fill_viridis_c(name = "Depth", option = "C") + xlim(0, 7.5) + theme_bw() + labs(x ="Log Hourly Wage", y = "Schooling") +
  scale_y_discrete(labels = c('None','Preschool','Primary',"Middle school","High school", 
                              "Normal School","Technical career", "Bachelor", "Master's degree", "PhD"))+
  theme(legend.position="none")

ggsave("D:/ray/shiny/empleo/www/dist/a2019m.png")
  

a2019h <-ggplot(x, aes(x =lnw_hr, y = cs_p13_1, fill = stat(x))) +
  geom_density_ridges_gradient() +
  scale_fill_viridis_c(name = "Depth", option = "C") + xlim(0, 7.5) + theme_bw() + labs(x ="Log Hourly Wage", y = "Schooling") +
  scale_y_discrete(labels = c('None','Preschool','Primary',"Middle school","High school", 
                              "Normal School","Technical career", "Bachelor", "Master's degree", "PhD"))+
  theme(legend.position="none")
ggsave("D:/ray/shiny/empleo/www/dist/a2019h.png")

####Revisamos sí hay diferencia de medias por año para hombres y mujeres

med <- df %>%filter(cs_p13_1!=99,!is.na(cs_p13_1)) %>% group_by(sex, year, cs_p13_1) %>% summarize(media =mean(sal_trim_const, na.rm=T))

med <- spread(med, key="cs_p13_1", value="media")
med <- gather(med, key="educ", value="salmed", 3:12)

write.csv(med,"D:/ray/shiny/empleo/www/dist/salmed.csv", row.names = FALSE)

hom <- med %>% filter(sex==1, year %in% 2018:2022)
muj <- med %>% filter(sex==2, year %in% 2018:2022)

ggplot(hom, aes(x=year, y=salmed, group=educ, color=educ)) + geom_line()

ggplot(muj, aes(x=year, y=salmed, group=educ, color=educ)) + geom_line()


enoe_sal_gen_trim <- spread(enoe_sal_gen_trim, key="sex", value ="saltrim")
colnames(enoe_sal_gen_trim) <- c("date","Male","Female")
enoe_sal_gen_trim$Gap <- enoe_sal_gen_trim$Male - enoe_sal_gen_trim$Female
enoe_sal_gen_trim <- gather(enoe_sal_gen_trim, key="sex", value="saltrim", 2:4)

salmed$year <- as.factor(salmed$year)

ggplot(salmed, aes(x=educ,y=salmed, fill=as.factor(year))) + 
  geom_bar(position="dodge" ,stat="identity") 

salmed$educ<- as.character(salmed$educ)

a <- read.csv("D:/ray/shiny/empleo/www/dist/salmed.csv")

b <- a %>% filter(sex=="Female") %>% 
  group_by(educ) %>% 
  arrange(year, .by_group = TRUE) %>%
  mutate(pct_change = (salmed/lag(salmed) - 1) * 100)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

library(sf)


base  <- read.csv("D:/ray/shiny/empleo/www/base_muni.csv")
shape_file <- read_sf("D:/ray/shiny/empleo/www/municd/municd.shp")
shape_file$CVE_MUN <- as.numeric(shape_file$CVE_MUN)
shape_file$CVE_ENT <- as.numeric(shape_file$CVE_ENT)
mex_map <- shape_file %>% left_join(base, by = c( "CVE_ENT"="clave","CVE_MUN"="mun"))
##Filtro para la prueba
mex_map <- mex_map %>% filter(year==2015, ent == "Sinaloa" )


ggplot(data=mex_map,aes(fill = nivel)) +
  geom_sf(colour = "white",  size = 0) +
  scale_fill_viridis_d() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.key.size = unit(0.4, 'cm'),
        legend.text = element_text(size=12),
        legend.title = element_text(size = 12),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.grid.major.y = element_line(size=0.05, color='black'),
        panel.background = element_rect(fill='white'),
        plot.title = element_text(size=16))


### EMPLOYMENT

employ  <- read.csv("D:/part.csv")


employ <- employ %>% filter(var!="Hours Worked") 
a <- employ %>% filter(var=="PEA") 

ggplot(a, aes(x=var,y=value, fill= as.factor(year))) + 
  geom_bar(position="dodge",stat="identity") +ylim(min(employ$value[employ$var=="PEA"]),max(employ$value[employ$var=="PEA"]) )+ theme_bw() +xlab("Variable") +
  ylab("Percentage") + scale_fill_manual(values=cbPalette)



 ### hours worked employment 


hours <- read.csv("D:/horas.csv")


ggplot(hours, aes(x=year,y=hrs, group=categoria)) + 
  geom_line(position="dodge",stat="identity") + theme_bw() +xlab("Variable") +
  ylab("Percentage") #+ scale_fill_manual(values=cbPalette)





###Apartado econometrico 


qreg <- read.csv("D:/qreg.csv")
qreg <- qreg %>% filter(year==2020)

qreg_graph <- ggplot(qreg)+
  geom_line(aes(x=n,y=genero,color="Qreg coefficient"),size=.5)+
  geom_line(aes(x=n,y=genero_mco,color="OLS coefficient"),size=.75)+
  #geom_line(data=qreg,aes(x=n,y=ll_g,color="Qreg CI"),linetype="dashed",size=.5)+
  #geom_line(data=qreg,aes(x=n,y=ul_g,color="Qreg CI"),linetype="dashed",size=.5)+ 
  geom_line(aes(x=n,y=ll_g_mco,color="OLS CI"),linetype="dashed",size=.5)+
  geom_line(aes(x=n,y=ul_g_mco,color="OLS CI"),linetype="dashed",size=.5)+
  scale_color_manual(values = c(
    "Qreg coefficient" = "blue",
    "OLS coefficient" = "black",
    #"Qreg CI" = "grey",
    "OLS CI" = "grey")) +
  labs(y="Percent wage gap", x="Percentile")+
  theme_bw()
qreg_graph

q1 <- ggplotly(qreg_graph)

q1


### filtro bases covid

covid <- read.csv("C:/Users/rault/Downloads/covid2.csv")

a <- covid %>% group_by(poblacion, year,nombre,cve_ent,mes) %>% summarise(muertos=sum(muertos)) 

a$trim[a$mes<=3] <- 1
a$trim[a$mes>3 & a$mes<=6] <- 2
a$trim[a$mes>6 & a$mes<=9] <- 3
a$trim[a$mes>9 & a$mes<=12] <- 4

a <- a %>% group_by(poblacion,year,nombre,cve_ent,trim) %>% summarise(muertos=sum(muertos)) 

a$tasa <- (a$muertos/a$poblacion)*100000

write.csv(a,"C:/Users/rault/Downloads/covid3.csv", row.names = FALSE)


x <- "a1~!@#$%^&*(){}_+:\"<>?,./;'[]-=" #or whatever
Data <- as.data.frame(gsub("''","" , mex_map$NOM_MUN ,ignore.case = TRUE))

