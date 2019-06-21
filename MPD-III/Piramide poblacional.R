# Population pyramids in R

# Packages
library(XML)
library(extrafont)
# Getting data ready

censo2001<- read_excel("C:\\Users\\Vladimir\\Documents\\R\\Maestría\\Data\\reporte.xls")
# censo2012<- read_excel("C:\\Users\\Vladimir\\Documents\\R\\Maestría\\Data\\reporte(1).xls")

#----- CLEANING DATA
## to eliminate unnecessary rows and columns
censo2001<- censo2001[-c(1:10, 32:35), -1]
# to change names
names(censo2001)<- as.matrix(censo2001[1,]) # 
names(censo2001)[1]<- "AGEGRP"
censo2001$YEAR<- 2001
# eliminate first row
censo2001<- censo2001[-1,]

## setting data to the right format
# to remove characters     
censo2001[, 2:4] %>% clean_names()
# convert from character to numeric
censo2001$HOMBRE<- as.numeric(as.character(censo2001$HOMBRE))
censo2001$MUJER<- as.numeric(as.character(censo2001$MUJER))
censo2001$Total<- as.numeric(as.character(censo2001$Total))
# get total population
censo2001$TOTAL_AGE<- censo2001$HOMBRE+censo2001$MUJER
censo2001$TOTAL_POP<-sum(censo2001$TOTAL_AGE)
# drop total column with NA values
censo2001<- censo2001[,-4]

## create data set for males
# selecting variables for males
censo2001_homb<- censo2001[c(1,2,4,6)]
censo2001_homb$GROUP<- "Males"
# percentage per age group
censo2001_homb$PERCENT<- censo2001_homb$HOMBRE/censo2001_homb$TOTAL_POP *100
# change variable name
names(censo2001_homb)[2]<- "COUNT"

## create data set for females
# selecting variables for males
censo2001_mujer<- censo2001[c(1,3,4,6)]
censo2001_mujer$GROUP<- "Females"
# percentage per age group
censo2001_mujer$PERCENT<- censo2001_mujer$MUJER/censo2001_mujer$TOTAL_POP *100
# change variable name
names(censo2001_mujer)[2]<- "COUNT"

# stack data
CENSO2001<- rbind(censo2001_homb, censo2001_mujer)
# take all the rows coded as males and flip their percent to negative
CENSO2001$PERCENT[CENSO2001$GROUP=="Males"] <- -CENSO2001$PERCENT[CENSO2001$GROUP=="Males"] 
# change values in agegroup column
CENSO2001$AGEGRP[censo2001$AGEGRP=="De 5 a 9 años"]<- "De 05 a 9 años"

#----- CREATING PLOT
attach(CENSO2001)

ggplot(CENSO2001, aes(x=AGEGRP, y=PERCENT, fill=GROUP)) +
  geom_bar(stat = "identity", width = .85) +   # draw the bars
  scale_y_continuous(breaks = seq(-10,10,length.out = 5),labels = c('10%','5%', '0','5%','10%')) +
  coord_flip() +  # Flip axes
  labs(title="Cambios en la estructura poblacional de Bolivia desde 2001") +
  theme(plot.title = element_text(hjust = .5),
        axis.ticks = element_blank()) +   # Centre plot title
  scale_fill_manual(values=c("#899DA4", "#C93312")) +
  #theme_tufte(base_size = 12, base_family="Avenir") +
  facet_grid(. ~ YEAR)

