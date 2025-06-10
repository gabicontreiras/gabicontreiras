rm(list=ls())
library(tidyverse)
library(gridExtra)
library(RColorBrewer)
elite <- read_csv("elite.csv")
elite$partido <- ifelse(elite$partido == "PDC", "DC", elite$partido)
table(elite$partido)
povo <- read_csv("povo.csv")
summary(elite) 
summary(povo) 

table(povo$partido)

povo_desconfia <- tapply(povo$desconfia, povo$partido,
                         FUN=function (x) mean(x, na.rm = TRUE))
elite_desconfia <- tapply(elite$desconfia, elite$partido,
                          FUN= function (x) mean(x, na.rm = TRUE))
povo_desconfia
elite_desconfia
elite_desconfia <- as.numeric(elite_desconfia)
povo_desconfia <- as.numeric(povo_desconfia)


Base <- c(rep("Elite", 5), rep("Público", 5))
Partido <-c("PS","DC", "UDI", "PPD" , "RN",
            "PS","DC", "UDI", "PPD" , "RN")
Partido <- factor(Partido, levels = c("PS","DC", "UDI", "PPD" , "RN"))
Values <- c(povo_desconfia, elite_desconfia)
prop.table(table(elite$partido))*100
prop.table(table(povo$partido))*100
hh1 <- c(prop.table(table(elite$partido))*100,
         prop.table(table(povo$partido))*100)

graf1 <- data.frame(Base,Partido,Values,hh1)

f1 <- graf1 %>%
  dplyr::filter(Partido %in% c("PS","DC", "UDI", "PPD" , "RN", "OUTROS")) %>%
  ggplot(aes(x=Values, y=Base, size = hh1, color = Partido)) +
  geom_point(alpha=1) + 
  theme_classic(base_size = 16)+
  scale_size(range = c(2,10), name="%") +
  scale_y_discrete() +
  scale_x_continuous(name="Desconfiança", limits=c()) +
  scale_color_manual(values = c("red", "blue", "yellow", "orange", "green", "gray34")) +
  ggtitle("Elites e Públicos Desconfiança, médias")

f1

f1 + labs(subtitle = "Chile 2018", 
          caption = "Fonte: PELA 2014 e WVS 2012")


