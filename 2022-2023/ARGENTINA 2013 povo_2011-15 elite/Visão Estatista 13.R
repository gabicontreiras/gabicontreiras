rm(list=ls())
library(tidyverse)
library(gridExtra)
library(RColorBrewer)
elite <- read_csv("elite.csv")
povo <- read_csv("povo.csv")
summary(elite) 
summary(povo) 
table(elite$partido)
table(povo$partido)

elite$partido <- factor(elite$partido,
                        levels = c("KIRCHENERISTA","SOCIALISTA", "PJ", "UCR", "OUTROS"))
povo$partido <- factor(povo$partido,
                       levels = c("KIRCHENERISTA","SOCIALISTA", "PJ", "UCR", "OUTROS", "NENHUM"))
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

Base <- c(rep("Elite", 5), rep("Público", 6))
Partido <- c("KIRCHENERISTA","SOCIALISTA", "PJ", "UCR", "OUTROS", 
             "KIRCHENERISTA","SOCIALISTA", "PJ", "UCR", "OUTROS", "NENHUM")
Partido <- factor(Partido, levels = c("KIRCHENERISTA","SOCIALISTA", "PJ", "UCR", "OUTROS", "NENHUM"))
Values <- c(povo_proESTADO, elite_proESTADO)
prop.table(table(elite$partido))*100
prop.table(table(povo$partido))*100
hh1 <- c(prop.table(table(elite$partido))*100,
         prop.table(table(povo$partido))*100)

graf1 <- data.frame(Base,Partido,Values,hh1)

f1 <- graf1 %>%
  dplyr::filter(Partido %in% c("KIRCHENERISTA","SOCIALISTA", "PJ", "UCR", "OUTROS", "NENHUM")) %>%
  ggplot(aes(x=Values, y=Base, size = hh1, color = Partido)) +
  geom_point(alpha=1) + 
  theme_classic(base_size = 16)+
  scale_size(range = c(2,10), name="%") +
  scale_y_discrete() +
  scale_x_continuous(name="Visão Estatista", limits=c()) +
  scale_color_manual(values = c("blue", "red", "green" , "yellow",  "gray45", "#bc6c25")) +
  ggtitle("Elites e Públicos Valores Econômicos, médias")

f1

f1 + labs(subtitle = "Argentina 2013",
          caption = "Fonte: PELA 2013 e WVS 2013")
