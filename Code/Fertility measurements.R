library(ggplot2)

#####################################################################
#     Tasas de fecundida por grupos quinquenales                   ##
#-------------------------------------------------------------------#

## Building data set
Age_group<- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")
MEF<- c(570.820, 541.429, 474.959, 415.863, 365.770, 325.770, 265.288)
B<- c(41.042, 86.791, 70.476, 48.490, 28.896, 11.230, 2.010)

Data_1<- cbind(Age_group, MEF, B)
Data_1<- as.data.frame(Data_1)
attach(Data_1)

## Calcuating rates
# Calculating age-specific birth rate (tasa de fecundidad por edad)
Data_1$F_x_5<- B/MEF
# Reduce to 4 significant figures
Data_1$F_x_5<- signif(Data_1$F_x_5, digits = 4)
## signif(x, digits = 6)

# Calculating fertility rate by 1000
Data_1$F_x_5_1000<- Data_1$F_x_5*1000

## Creating graph
attach(Data_1)
Fertility_graph<- ggplot(data = Data_1) +
  geom_point(mapping = aes(x=Age_group, y=F_x_5_1000))+
  labs(x="Age Groups",
       y="By 1000 people",
       title = "Bolivia. Age-specific Birth Rate (2015)",
       caption = "Bolivia's age pattern of fertility suggests that women are bearing children at young ages, peaking at age 20-24")+
  theme(plot.title = element_text(hjust = .5),
        axis.ticks = element_blank()) +
  theme_minimal()
# Fecundidad de cuspide temprana porque su maximo valor esta entre 20 y 25 anhos de edad. Tasa de

# Calculating TGF
Data_1$Sum_f_x_5<- sum(Data_1$F_x_5)
Data_1$TGF<- 5*Data_1$Sum_f_x_5 # Hijos e hijas por mujer

# Calculating TBR
Data_1$K<- 100/205
Data_1$TBR<- 5 * Data_1$K * Data_1$Sum_f_x_5 # Hijas por mujer, considerando que las mujeres no mueren

# Calculating TNR
# Considera muertes antes de llegar a la edad reproductiva
# Inclduing survivorship probability
Data_1$P_x<- c(0.9321, 0.9247, 0.9168, 0.9077, 0.8952, 0.8784, 0.8563)
Data_1$F_x_5_P_x<- Data_1$F_x_5 * Data_1$P_x
Data_1$Sum_F_x_5_P_x<- sum(Data_1$F_x_5_P_x)
Data_1$TNR<- 5 * Data_1$K * sum(Data_1$F_x_5_P_x) # Hijas mujeres por cada mujer en edad fertil
