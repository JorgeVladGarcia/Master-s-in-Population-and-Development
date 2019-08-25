# Tasas de fecundidad por edad segun encuestas de demografia y salud (por mil)

library(tidyverse)
library(reshape2)

Age_group<- as.character(c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))
EDSA_2016<- c(71, 140, 131, 120, 82, 25, 6)
ENDSA_2008<- c(88, 174, 173, 128, 95, 43, 8)
ENDSA_2003<- c(84, 183, 179, 145, 114, 51, 12)
ENDSA_1998<- c(84, 207, 201, 165, 117, 57, 15)

DATA<- data.frame(Age_group, EDSA_2016, ENDSA_2008, ENDSA_2003, ENDSA_1998)

# get data into the right format or graph
DATA_graph<- melt(DATA, id="Age_group")

## Graph
attach(DATA_graph)
CUADRO_1<- ggplot(data = DATA_graph) +
  geom_point(mapping = aes(x=Age_group, y=value, color = variable, group=variable))+
  geom_line(mapping = aes(x=Age_group, y=value, color = variable, group=variable)) +
  labs(x="EDADES",
       y="TASA F(x)",
       title = "GRÁFICO No. 1",
       subtitle = "BOLIVIA.TASA DE FECUNIDAD POR ENCUESTAS SEGÚN GRUPOS DE EDAD. AÑOS 1988, 2003, 2008, 2016.",
       color = "Ref:",
       caption = "Bolivia's age pattern of fertility suggests that women are bearing children at young ages, peaking at age 20-24")+
      theme(plot.title = element_text(hjust = .5),
        axis.ticks = element_blank())+
    theme_minimal()

# Por porcentajes
DATA$PER_EDSA_2016<- EDSA_2016/sum(EDSA_2016) * 100
DATA$PER_ENDSA_2008<- ENDSA_2008/sum(ENDSA_2008)*100
DATA$PER_ENDSA_2003<- ENDSA_2008/sum(ENDSA_2003)*100
DATA$PER_ENDSA_1998<- ENDSA_2008/sum(ENDSA_1998)*100

# Getting data to the right format
DATA_graph_per<- select(DATA, c(Age_group, PER_EDSA_2016, PER_ENDSA_2008, PER_ENDSA_2003, PER_ENDSA_1998))
DATA_graph_per<- melt(DATA_graph_per, id="Age_group")

# Graph
attach(DATA_graph_per)
CUADRO_2<- ggplot(data = DATA_graph_per) +
  geom_point(mapping = aes(x=Age_group, y=value, color = variable, group=variable))+
  geom_line(mapping = aes(x=Age_group, y=value, color = variable, group=variable)) +
  labs(x="EDADES",
       y="TASA F(x)",
       title = "GRÁFICO No. 2",
       subtitle = "BOLIVIA.TASA DE FECUNIDAD POR ENCUESTAS PORCENTUALIZADAS SEGÚN GRUPOS DE EDAD. AÑOS 1988, 2003, 2008, 2016.",
       color = "Ref:",
       caption = "")+
  theme(plot.title = element_text(hjust = .5),
        axis.ticks = element_blank())+
  theme_minimal()

# GEtting values to calculate TGF (total global fertility)
DATA$EDSA_2016<- DATA$EDSA_2016/1000
DATA$ENDSA_2008<- DATA$ENDSA_2008/1000
DATA$ENDSA_2003<- DATA$ENDSA_2003/1000
DATA$ENDSA_1998<- DATA$ENDSA_1998/1000

DATA$sum_f_x_2008<- sum(DATA$ENDSA_2008)
DATA$sum_f_x_2016<- sum(DATA$EDSA_2016)
DATA$sum_f_x_2003<- sum(DATA$ENDSA_2003)
DATA$sum_f_x_1998<- sum(DATA$ENDSA_1998)

# Calculating TGF
DATA$TGF_2016<- 5 * DATA$sum_f_x_2016
DATA$TGF_2008<- 5 * DATA$sum_f_x_2008
DATA$TGF_2003<- 5 * DATA$sum_f_x_2003
DATA$TGF_1998<- 5 * DATA$sum_f_x_1998

# GEtting data to calculate TBR (net rate of reproduction or tasa bruta de reproduccion)
DATA$K<- 100/205

# Calculate TBR
DATA$TBR_2016<-  5 * DATA$K * DATA$sum_f_x_2016
DATA$TBR_2008<-  5 * DATA$K * DATA$sum_f_x_2008
DATA$TBR_2003<-  5 * DATA$K * DATA$sum_f_x_2003
DATA$TBR_1998<-  5 * DATA$K * DATA$sum_f_x_1998
