rm(list=ls())
library(tidyverse)
library(gridExtra)
library(RColorBrewer)
elite <- read_csv("elite.csv")
elite$partido <- ifelse(elite$partido == "PDC", "DC", elite$partido)
table(elite$partido)


povo <- read_csv("povo.csv")
povo$proMERCADO *-1 -> povo$proESTADO
povo$proESTADO <- scales::rescale(povo$proESTADO, to = c(0, 1))
elite$proMERCADO *-1 -> elite$proESTADO
elite$proESTADO <- scales::rescale(elite$proESTADO,to=c(0,1))

summary(elite) 
summary(povo) 
table(elite$partido)
table(povo$partido)


elite$partido <- factor(elite$partido,
                        levels = c("PS", "DC", "UDI", "PPD", "RN", "OUTROS"))
povo$partido <- factor(povo$partido,
                       levels = c("PS", "DC", "UDI", "PPD", "RN"))
table(elite$partido)
table(povo$partido)

povo_proESTADO <- tapply(povo$proESTADO, povo$partido,
                         FUN = function(x) mean (x, na.rm = TRUE))

elite_proESTADO <- tapply(elite$proESTADO, elite$partido,
                          FUN = function(x) mean (x, na.rm = TRUE))

povo_proESTADO
elite_proESTADO

povo_proESTADO <- as.numeric(povo_proESTADO)
elite_proESTADO <- as.numeric(elite_proESTADO)
Base <- c(rep("Elite", 6), rep("Público", 5))
Partido <- c("PS", "DC", "UDI", "PPD", "RN", "OUTROS" , 
             "PS", "DC", "UDI", "PPD", "RN")
Partido <- factor(Partido, levels = c("PS", "DC", "UDI", "PPD", "RN", "OUTROS"))
Values <- c(povo_proESTADO, elite_proESTADO)
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
  scale_x_continuous(name="Visão Estatista", limits=c()) +
  scale_color_manual(values = c("red", "blue", "yellow", "orange", "green", "gray34")) +
  ggtitle("Elites e Públicos Valores Econômicos, médias")

f1

f1 + labs(subtitle = "Chile 2018", 
          caption = "Fonte: PELA 2014 e WVS 2012")

