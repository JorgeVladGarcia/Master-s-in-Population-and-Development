library(ggplot2)
library(ggthemes)

# Create data set 
Departamento<- c("Chuquisaca", "La Paz", "Cochabamba")
"2001"<- c(531522, 2350466, 1455711)
"2012"<- c(581347, 2719344, 1762762)

censo_Bol<- data.frame(Departamento,`2001`, `2012`)

# Calculate total growth
censo_Bol$crec_total<- `2012`- `2001`

# Calculate population mean
censo_Bol$pob_prom<- (`2012`+`2001`)/2

# Calculate growth rate
censo_Bol$tasa_crec<- censo_Bol$crec_total/censo_Bol$pob_prom

# Calculate growth rate (percentage)
censo_Bol$tasa_crec_per<- censo_Bol$crec_total/censo_Bol$pob_prom*100

# Calculate growth per 1000 habs.
censo_Bol$tasa_Crec_hab<- censo_Bol$tasa_crec*1000

# Graph bar Economist style
ggplot(data = censo_Bol, aes(x=Departamento, y=tasa_crec_per, fill=Departamento)) +
  geom_bar(stat = "identity", color="black") + # color adds black borders to charts 
  scale_fill_manual(values = c("#d8b365", "#f5f5f5", "#5ab4ac")) +
  xlab("Departamento") + ylab("Tasa de crecimiento") +
  theme_economist() +
  scale_color_economist()+
  ggtitle("Tasa de Crecimiento Poblacional 2001-2012") +
  theme(legend.position = "none")   # Eliminates legend to the right
ggsave("C:\\Users\\Vladimir\\Documents\\R\\Maestría\\crec_pob.png")  

# Graph bar WSJ style
ggplot(data = censo_Bol, aes(x=Departamento, y=tasa_crec_per, fill=Departamento)) +
  geom_bar(stat = "identity", color="black") + # color adds black borders to charts 
  scale_fill_manual(values = c("#d8b365", "#f5f5f5", "#5ab4ac")) +
  xlab("Departamento") + ylab("Tasa de crecimiento") +
  theme_wsj() +
  scale_color_wsj("colors6")+
  ggtitle("Tasa de Crecimiento Poblacional 2001-2012") +
  theme(legend.position = "none")   # Eliminates legend to the right



ggplot(data = censo_Bol, aes(x=Departamento, y=tasa_crec_per, fill=Departamento)) +
  geom_bar(stat = "identity", color="black") + # color adds black borders to charts 
  scale_fill_manual(values = c("#d8b365", "#f5f5f5", "#5ab4ac")) +
  xlab("Departamento") + ylab("Tasa de crecimiento") +
  ggtitle("Tasa de Crecimiento Poblacional 2001-2012") +
  theme(legend.position = "none") +  # Eliminates legend to the right

# To-do 
# 1. Add dark borders to charts
# 2. Eliminate the stuff to the right as it is too obvious
# 3. Make the background nicer 
