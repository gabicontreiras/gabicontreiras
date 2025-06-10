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

povo_desconfia <- tapply(povo$desconfia, povo$partido,
                         FUN=function (x) mean(x, na.rm = TRUE))
elite_desconfia <- tapply(elite$desconfia, elite$partido,
                          FUN= function (x) mean(x, na.rm = TRUE))
povo_desconfia
elite_desconfia
elite_desconfia <- as.numeric(elite_desconfia)
povo_desconfia <- as.numeric(povo_desconfia)


Base <- c(rep("Elite", 6), rep("Público", 7))
Partido <- c("MDB", "PT", "PSDB", "PDT", "PFL", "NENHUM", "OUTROS",
             "MDB", "PT", "PSDB", "PDT", "PFL", "OUTROS")
Partido <- factor(Partido, levels = c("MDB", "PT", "PSDB", "PDT", "PFL", "NENHUM", "OUTROS"))
Values <- c(povo_desconfia, elite_desconfia)
prop.table(table(elite$partido))*100
prop.table(table(povo$partido))*100
hh1 <- c(prop.table(table(elite$partido))*100,
         prop.table(table(povo$partido))*100)

graf1 <- data.frame(Base,Partido,Values,hh1)

f1 <- graf1 %>%
  dplyr::filter(Partido %in% c("MDB", "PT", "PSDB", "PDT", "PFL", "NENHUM", "OUTROS")) %>%
  ggplot(aes(x=Values, y=Base, size = hh1, color = Partido)) +
  geom_point(alpha=1) + 
  theme_classic(base_size = 16)+
  scale_size(range = c(2,10), name="%") +
  scale_y_discrete() +
  scale_x_continuous(name="Desconfiança", limits=c()) +
  scale_color_manual(values = c("orange", "red", "blue", "darkred", "green", "gray45", "#bc6c25")) +
  ggtitle("Elites e Públicos Desconfiança, médias")

f1


f1 + labs(subtitle = "Brasil 2006",
          caption = "Fonte: PELA 2005 e WVS 2006")

