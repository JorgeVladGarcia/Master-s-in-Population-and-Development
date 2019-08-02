library(readxl)
library(ggplot2)
library(gganimate)
library(devtools)
library(tm)
library(magick)
library(reshape2)

# Getting data ready
#::::::::::::::::::::
censo_dept_2001_2012<- read_excel("C:\\Users\\Vladimir\\Documents\\R\\Maestría\\Data\\pob_depts.xlsx")

# eliminate unnesary rows and columns
censo_dept_2001_2012<- censo_dept_2001_2012[-c(1:3,5), -c(8,9)]
# change variable names
names(censo_dept_2001_2012)<- as.matrix(censo_dept_2001_2012[1,])
names(censo_dept_2001_2012)[1]<- "AGEGRP"
# remove unnecesary rows
censo_dept_2001_2012<- censo_dept_2001_2012[-1,]

# get data only for chuquisaca
censo_CHUQ_2001_2012<- censo_dept_2001_2012[-c(1:63,127:632),]

censo_CHUQ_2001<- censo_CHUQ_2001_2012[,c(1,3,4)]
censo_CHUQ_2001$YEAR<- 2001

censo_CHUQ_2012<- censo_CHUQ_2001_2012[,c(1,6,7)]
censo_CHUQ_2012$YEAR<- 2012

#---------- Chuquisaca 2012 ----------#
censo_CHUQ_2012_total<-censo_CHUQ_2012[c(1:21),]
censo_CHUQ_2012_total<- censo_CHUQ_2012_total[-1,]
censo_CHUQ_2012_total<- melt(censo_CHUQ_2012_total, id=c("AGEGRP", "YEAR"))
censo_CHUQ_2012_total$value<- as.numeric(as.character(censo_CHUQ_2012_total$value))
censo_CHUQ_2012_total$TOTAL_POP<- sum(censo_CHUQ_2012_total$value)
censo_CHUQ_2012_total$PERCENT<- censo_CHUQ_2012_total$value/censo_CHUQ_2012_total$TOTAL_POP *100
censo_CHUQ_2012_total$LOCATION<- "Total"


censo_CHUQ_2012_urbana<- censo_CHUQ_2012[c(22:42),]
censo_CHUQ_2012_urbana<- censo_CHUQ_2012_urbana[-1,]
censo_CHUQ_2012_urbana<- melt(censo_CHUQ_2012_urbana, id=c("AGEGRP", "YEAR"))
censo_CHUQ_2012_urbana$value<- as.numeric(as.character(censo_CHUQ_2012_urbana$value))
censo_CHUQ_2012_urbana$TOTAL_POP<- sum(censo_CHUQ_2012_urbana$value)
censo_CHUQ_2012_urbana$PERCENT<- censo_CHUQ_2012_urbana$value/censo_CHUQ_2012_urbana$TOTAL_POP*100
censo_CHUQ_2012_urbana$LOCATION<- "Urbana"

censo_CHUQ_2012_rural<- censo_CHUQ_2012[c(43:63),]
censo_CHUQ_2012_rural<- censo_CHUQ_2012_rural[-1,]
censo_CHUQ_2012_rural<- melt(censo_CHUQ_2012_rural, id=c("AGEGRP", "YEAR"))
censo_CHUQ_2012_rural$value<- as.numeric(as.character(censo_CHUQ_2012_rural$value))
censo_CHUQ_2012_rural$TOTAL_POP<- sum(censo_CHUQ_2012_urbana$value)
censo_CHUQ_2012_rural$PERCENT<- censo_CHUQ_2012_rural$value/censo_CHUQ_2012_rural$TOTAL_POP*100
censo_CHUQ_2012_rural$LOCATION<- "Rural"

censo_CHUQ_2012<- rbind(censo_CHUQ_2012_total, censo_CHUQ_2012_urbana, censo_CHUQ_2012_rural)
colnames(censo_CHUQ_2012)[colnames(censo_CHUQ_2012)=="value"] <- "COUNT"
colnames(censo_CHUQ_2012)[colnames(censo_CHUQ_2012)=="variable"] <- "GROUP"
censo_CHUQ_2012<- as.data.frame(censo_CHUQ_2012)

# Getting data ready for plot

## remove white spaces at the front and end
censo_CHUQ_2012$GROUP<- trimws(censo_CHUQ_2012$GROUP)
censo_CHUQ_2012$AGEGRP<- trimws(censo_CHUQ_2012$AGEGRP)
## so group 5-9 can go in the correct line
censo_CHUQ_2012$AGEGRP[censo_CHUQ_2012$AGEGRP=="5 - 9"]<- "05 - 9"
## take all the rows coded as males and flip their percent to negative
censo_CHUQ_2012$PERCENT[censo_CHUQ_2012$GROUP=="Hombres"] <- -censo_CHUQ_2012$PERCENT[censo_CHUQ_2012$GROUP=="Hombres"]


# ------------- Chuquisaca 2001 -----------------#
censo_CHUQ_2001_total<-censo_CHUQ_2001[c(1:21),]
censo_CHUQ_2001_total<- censo_CHUQ_2001_total[-1,]
censo_CHUQ_2001_total<- melt(censo_CHUQ_2001_total, id=c("AGEGRP", "YEAR"))
censo_CHUQ_2001_total$value<- as.numeric(as.character(censo_CHUQ_2001_total$value))
censo_CHUQ_2001_total$TOTAL_POP<- sum(censo_CHUQ_2001_total$value)
censo_CHUQ_2001_total$PERCENT<- censo_CHUQ_2001_total$value/censo_CHUQ_2001_total$TOTAL_POP *100
censo_CHUQ_2001_total$LOCATION<- "Total"


censo_CHUQ_2001_urbana<- censo_CHUQ_2001[c(22:42),]
censo_CHUQ_2001_urbana<- censo_CHUQ_2001_urbana[-1,]
censo_CHUQ_2001_urbana<- melt(censo_CHUQ_2001_urbana, id=c("AGEGRP", "YEAR"))
censo_CHUQ_2001_urbana$value<- as.numeric(as.character(censo_CHUQ_2001_urbana$value))
censo_CHUQ_2001_urbana$TOTAL_POP<- sum(censo_CHUQ_2001_urbana$value)
censo_CHUQ_2001_urbana$PERCENT<- censo_CHUQ_2001_urbana$value/censo_CHUQ_2001_urbana$TOTAL_POP*100
censo_CHUQ_2001_urbana$LOCATION<- "Urbana"

censo_CHUQ_2001_rural<- censo_CHUQ_2001[c(43:63),]
censo_CHUQ_2001_rural<- censo_CHUQ_2001_rural[-1,]
censo_CHUQ_2001_rural<- melt(censo_CHUQ_2001_rural, id=c("AGEGRP", "YEAR"))
censo_CHUQ_2001_rural$value<- as.numeric(as.character(censo_CHUQ_2001_rural$value))
censo_CHUQ_2001_rural$TOTAL_POP<- sum(censo_CHUQ_2001_urbana$value)
censo_CHUQ_2001_rural$PERCENT<- censo_CHUQ_2001_rural$value/censo_CHUQ_2001_rural$TOTAL_POP*100
censo_CHUQ_2001_rural$LOCATION<- "Rural"

censo_CHUQ_2001<- rbind(censo_CHUQ_2001_total, censo_CHUQ_2001_urbana, censo_CHUQ_2001_rural)
colnames(censo_CHUQ_2001)[colnames(censo_CHUQ_2001)=="value"] <- "COUNT"
colnames(censo_CHUQ_2001)[colnames(censo_CHUQ_2001)=="variable"] <- "GROUP"
censo_CHUQ_2001<- as.data.frame(censo_CHUQ_2001)

# Getting data ready for plot
## remove white spaces at the front and end
censo_CHUQ_2001$GROUP<- trimws(censo_CHUQ_2001$GROUP)
censo_CHUQ_2001$AGEGRP<- trimws(censo_CHUQ_2001$AGEGRP)
## so group 5-9 can go in the correct line
censo_CHUQ_2001$AGEGRP[censo_CHUQ_2001$AGEGRP=="5 - 9"]<- "05 - 9"
## take all the rows coded as males and flip their percent to negative
censo_CHUQ_2001$PERCENT[censo_CHUQ_2001$GROUP=="Hombres"] <- -censo_CHUQ_2001$PERCENT[censo_CHUQ_2001$GROUP=="Hombres"]

# Combining all data sets
#censo_CHUQ_2001_2012<- rbind(censo_CHUQ_2001, censo_CHUQ_2012)

###################################
#----- CREATING ACTUAL PLOT ------#
###################################
attach(censo_CHUQ_2001)

CHUQ_2001<- ggplot(data = censo_CHUQ_2001, aes(x=AGEGRP, y=PERCENT, fill=GROUP))+
  geom_bar(stat = "identity", width = .85)+
  scale_y_continuous(breaks = seq(-10,10,length.out = 5),labels = c('10%','5%', '0','5%','10%')) +
  coord_flip() +  # Flip axes
  labs(x="Edad", y="Porcentaje de la población",
       title="Estructura poblacional de Bolivia 2001",
       caption="Jorge Vladimir Garcia | twitter: @vladiroufakis | github: VladGarcia",
       fill = "") +
  theme(plot.title = element_text(hjust = .5),
        axis.ticks = element_blank()) +   # Centre plot title
  scale_fill_manual(values=c("#899DA4", "#C93312")) +
  #theme_tufte(base_size = 12, base_family="Avenir") +
  facet_grid(. ~ LOCATION)+
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = rel(0.8), face = "bold"),
        plot.subtitle = element_text(size = rel(0.8)),
        plot.caption = element_text(size = rel(0.8)),
        axis.text.y = element_text(size = rel(0.9)),
        axis.text.x = element_text(size = rel(0.9)))

attach(censo_CHUQ_2012)

CHUQ_2012<- ggplot(data = censo_CHUQ_2012, aes(x=AGEGRP, y=PERCENT, fill=GROUP))+
  geom_bar(stat = "identity", width = .85)+
  scale_y_continuous(breaks = seq(-10,10,length.out = 5),labels = c('10%','5%', '0','5%','10%')) +
  coord_flip() +  # Flip axes
  labs(x="Edad", y="Porcentaje de la población",
       title="Estructura poblacional de Bolivia 2012",
       caption="Jorge Vladimir Garcia | twitter: @vladiroufakis | github: VladGarcia",
       fill = "") +
  theme(plot.title = element_text(hjust = .5),
        axis.ticks = element_blank()) +   # Centre plot title
  scale_fill_manual(values=c("#899DA4", "#C93312")) +
  #theme_tufte(base_size = 12, base_family="Avenir") +
  facet_grid(. ~ LOCATION)+
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = rel(0.8), face = "bold"),
        plot.subtitle = element_text(size = rel(0.8)),
        plot.caption = element_text(size = rel(0.8)),
        axis.text.y = element_text(size = rel(0.9)),
        axis.text.x = element_text(size = rel(0.9)))

attach(censo_CHUQ_2001_2012) # review this one it doesnt do what i wanted to

CHUQ_2001_2012<- ggplot(data = censo_CHUQ_2001_2012, aes(x=AGEGRP, y=PERCENT, fill=GROUP))+
  geom_bar(stat = "identity", width = .85)+
  scale_y_continuous(breaks = seq(-10,10,length.out = 5),labels = c('10%','5%', '0','5%','10%')) +
  coord_flip() +  # Flip axes
  labs(x="Edad", y="Porcentaje de la población",
       title="Estructura poblacional de Bolivia 2012",
       caption="Jorge Vladimir Garcia | twitter: @vladiroufakis | github: VladGarcia",
       fill = "") +
  theme(plot.title = element_text(hjust = .5),
        axis.ticks = element_blank()) +   # Centre plot title
  scale_fill_manual(values=c("#899DA4", "#C93312")) +
  #theme_tufte(base_size = 12, base_family="Avenir") +
  facet_wrap(YEAR ~ LOCATION)+
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = rel(0.8), face = "bold"),
        plot.subtitle = element_text(size = rel(0.8)),
        plot.caption = element_text(size = rel(0.8)),
        axis.text.y = element_text(size = rel(0.9)),
        axis.text.x = element_text(size = rel(0.9)))
