# Population pyramids in R

# Packages
#:::::::::
library(readxl)
library(ggplot2)
library(gganimate)
library(devtools)
library(tm)
library(magick)

# Getting data ready
#::::::::::::::::::::
censo2001<- read_excel("C:\\Users\\Vladimir\\Documents\\R\\Maestría\\Data\\reporte.xls")
censo2012<- read_excel("C:\\Users\\Vladimir\\Documents\\R\\Maestría\\Data\\reporte (1).xls")


          #---- CLEANING DATA 2001 -----#
          #::::::::::::::::::::::::::::::
# to eliminate unnecessary rows and columns
censo2001<- censo2001[-c(1:10, 32:35), -1]
# to change names
names(censo2001)<- as.matrix(censo2001[1,]) # 
names(censo2001)[1]<- "AGEGRP"
censo2001$YEAR<- 2001
# eliminate first row
censo2001<- censo2001[-1,]

# setting data to the right format
#:::::::::::::::::::::::::::::::::
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
#:::::::::::::::::::::::::::
# selecting variables for males
censo2001_homb<- censo2001[c(1,2,4,6)]
censo2001_homb$GROUP<- "Males"
# percentage per age group
censo2001_homb$PERCENT<- censo2001_homb$HOMBRE/censo2001_homb$TOTAL_POP *100
# change variable name
names(censo2001_homb)[2]<- "COUNT"

## create data set for females
#:::::::::::::::::::::::::::::
# selecting variables for males
censo2001_mujer<- censo2001[c(1,3,4,6)]
censo2001_mujer$GROUP<- "Females"
# percentage per age group
censo2001_mujer$PERCENT<- censo2001_mujer$MUJER/censo2001_mujer$TOTAL_POP *100
# change variable name
names(censo2001_mujer)[2]<- "COUNT"

          # Cleaning Data for 2012 #
          #::::::::::::::::::::::::#

## to eliminate unnecessary rows and columns
censo2012<- censo2012[-c(1:10, 32:35), -1]
# to change names
names(censo2012)<- as.matrix(censo2012[1,]) # 
names(censo2012)[1]<- "AGEGRP"
censo2012$YEAR<- 2012
# eliminate first row
censo2012<- censo2012[-1,]

## setting data to the right format 
#::::::::::::::::::::::::::::::::::
# to remove characters     
censo2012[, 2:4] %>% clean_names()
# convert from character to numeric
censo2012$Hombre<- as.numeric(as.character(censo2012$Hombre))
censo2012$Mujer<- as.numeric(as.character(censo2012$Mujer))
censo2012$Total<- as.numeric(as.character(censo2012$Total))
# get total population
censo2012$TOTAL_AGE<- censo2012$Mujer+censo2012$Hombre
censo2012$TOTAL_POP<-sum(censo2012$TOTAL_AGE)
# drop total column with NA values 
censo2012<- censo2012[,-4]

## create data set for males
#:::::::::::::::::::::::::::
# selecting variables for males
censo2012_homb<- censo2012[c(1,3,4,6)]
censo2012_homb$GROUP<- "Males"
# percentage per age group
censo2012_homb$PERCENT<- censo2012_homb$Hombre/censo2012_homb$TOTAL_POP *100
# change variable name
names(censo2012_homb)[2]<- "COUNT"

## create data set for females
#:::::::::::::::::::::::::::::
# selecting variables for males
censo2012_mujer<- censo2012[c(1,2,4,6)]
censo2012_mujer$GROUP<- "Females"
# percentage per age group
censo2012_mujer$PERCENT<- censo2012_mujer$Mujer/censo2001_mujer$TOTAL_POP *100
# change variable name
names(censo2012_mujer)[2]<- "COUNT"

            #---- GETTING DATA READY FOR PLOT -----#
            #::::::::::::::::::::::::::::::::::::::#
# stack data to get final data set 
CENSO<- rbind(censo2001_homb, censo2001_mujer, censo2012_homb, censo2012_mujer)

# take all the rows coded as males and flip their percent to negative
CENSO$PERCENT[CENSO$GROUP=="Males"] <- -CENSO$PERCENT[CENSO$GROUP=="Males"] 
# edit AGE group column
CENSO$AGEGRP<- removeWords(CENSO$AGEGRP,c("De", "de", "Edad"))
# remove white spaces at the front and end
CENSO$AGEGRP<- trimws(CENSO$AGEGRP)
# edit names
CENSO$AGEGRP[CENSO$AGEGRP=="5 a 9 años"]<- "05 a 9 años"
CENSO$AGEGRP[CENSO$AGEGRP=="95 y mas años"]<- "95 años y más"

            #---- CREATING PLOT -----#
            #::::::::::::::::::::::::#
attach(CENSO)
# Plot for 2001
#::::::::::::::
A<- ggplot(data = subset(CENSO, YEAR==2001), aes(x=AGEGRP, y=PERCENT, fill=GROUP)) +
  geom_bar(stat = "identity", width = .85) +   # draw the bars
  scale_y_continuous(breaks = seq(-10,10,length.out = 5),labels = c('10%','5%', '0','5%','10%')) +
  coord_flip() +  # Flip axes
  labs(x="Edad", y="Porcentaje de la población",
       title="Cambios en la estructura poblacional de Bolivia",
       caption="Jorge Vladimir Garcia | twitter: @vladiroufakis | github: VladGarcia",
       fill = "") +
  theme(plot.title = element_text(hjust = .5),
        axis.ticks = element_blank()) +   # Centre plot title
  scale_fill_manual(values=c("#899DA4", "#C93312")) +
  #theme_tufte(base_size = 12, base_family="Avenir") +
  facet_grid(. ~ YEAR)+
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = rel(0.8), face = "bold"),
        plot.subtitle = element_text(size = rel(0.8)),
        plot.caption = element_text(size = rel(0.8)),
        axis.text.y = element_text(size = rel(0.9)),
        axis.text.x = element_text(size = rel(0.9)))

# reorder and rename legend labels 
pop_pyr_2001 <-  A + scale_fill_discrete(guide=guide_legend(reverse = TRUE), name = "", labels=c("Mujeres", "Hombres"))

# Plot for 2012
#::::::::::::::
B<- ggplot(data = subset(CENSO, YEAR==2012), aes(x=AGEGRP, y=PERCENT, fill=GROUP)) +
  geom_bar(stat = "identity", width = .85) +   # draw the bars
  scale_y_continuous(breaks = seq(-10,10,length.out = 5),labels = c('10%','5%', '0','5%','10%')) +
  coord_flip() +  # Flip axes
  labs(x="Edad", y="Porcentaje de la población",
       title="Cambios en la estructura poblacional de Bolivia",
       caption="Jorge Vladimir Garcia | twitter: @vladiroufakis | github: VladGarcia",
       fill = "") +
  theme(plot.title = element_text(hjust = .5),
        axis.ticks = element_blank()) +   # Centre plot title
  scale_fill_manual(values=c("#899DA4", "#C93312")) +
  #theme_tufte(base_size = 12, base_family="Avenir") +
  facet_grid(. ~ YEAR)+
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = rel(0.8), face = "bold"),
        plot.subtitle = element_text(size = rel(0.8)),
        plot.caption = element_text(size = rel(0.8)),
        axis.text.y = element_text(size = rel(0.9)),
        axis.text.x = element_text(size = rel(0.9)))
# reorder and rename legend lables 
pop_pyr_2012<-  B + scale_fill_discrete(guide=guide_legend(reverse = TRUE), name = "", labels=c("Mujeres", "Hombres"))

# Animated plot
#:::::::::::::::
C<- ggplot(CENSO, aes(x=AGEGRP, y=PERCENT, fill=GROUP)) +
  geom_bar(stat = "identity", width = .85) +   # draw the bars
  scale_y_continuous(breaks = seq(-10,10,length.out = 5),labels = c('10%','5%', '0','5%','10%')) +
  coord_flip() +  # Flip axes
  theme(plot.title = element_text(hjust = .5),
        axis.ticks = element_blank()) +   # Centre plot title
  scale_fill_manual(values=c("#899DA4", "#C93312")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = rel(0.8), face = "bold"),
        plot.subtitle = element_text(size = rel(0.8)),
        plot.caption = element_text(size = rel(0.8)),
        axis.text.y = element_text(size = rel(0.9)),
        axis.text.x = element_text(size = rel(0.9)))
 # reorder and rename legend lables 
pop_pyr_01_12<-  C + scale_fill_discrete(guide=guide_legend(reverse = TRUE), name = "", labels=c("Mujeres", "Hombres"))

# actual animation 
#:::::::::::::::::::::::::
# conver years to integers 
CENSO$YEAR<- as.integer(CENSO$YEAR)
# actual animation 
pop_pyr_anim_2<- pop_pyr_01_12 + 
  labs(x="Edad", y="Porcentaje de la población",
       title='Cambios en la estructura poblacional de Bolivia {frame_along}',
       caption="Jorge Vladimir Garcia | twitter: @vladiroufakis | github: VladGarcia | Fuente: INE",
       fill = "") +
  transition_reveal(YEAR)+
  ease_aes('linear')
# save gif
pop_pyr<- animate(pop_pyr_anim_2)
anim_save("C:\\Users\\Vladimir\\Documents\\R\\Maestría\\Figures\\censo-2001-2012.gif", pop_pyr)