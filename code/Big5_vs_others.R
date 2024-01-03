library(viridis)
library(ggpubr)

d  = merge.data.frame(df1, df2, all.x=TRUE)
d$Durchschnitt_Heimvorteil_2020 = format(round(as.numeric(d$Durchschnitt_Heimvorteil_2020),digits=3), nsmall = 3)
d$Durchschnitt_Heimvorteil_2018 = format(round(as.numeric(d$Durchschnitt_Heimvorteil_2018),digits=3), nsmall = 3)


#selecting big 5 clubs
c = data.frame('Deutschland','England','Frankreich','Italien','Spanien')
d1 = d %>% filter( d$club %in% c)
d1 = mutate(d1, die_groﬂen_5 = as.numeric(Durchschnitt_Heimvorteil_2020) - as.numeric(Durchschnitt_Heimvorteil_2018))


#selecting other clubs
c2 = c('÷sterreich','Kroatien','D‰nemark','Polen','Slowakei')
d2 = d %>% filter( d$club %in% c2)
d2 = mutate(d2, Andere = as.numeric(Durchschnitt_Heimvorteil_2020) - as.numeric(Durchschnitt_Heimvorteil_2018))


die_groﬂen_5 <- melt(d2, id.vars='club')
Andere <- melt(d1, id.vars='club') 

compare = rbind(Andere,die_groﬂen_5)

die_groﬂen_5 = filter(compare, variable   == 'die_groﬂen_5')
Andere = filter(compare, variable   == 'Andere')

x= rbind(die_groﬂen_5,Andere)
x = mutate(x, Mannschaften=variable )


#generating bar plots representing change in home advantage values for big 5 and
#other clubs for years 2018 and 2020

ggplot(data=x, aes(x=reorder(club,-as.numeric(value)), y=as.numeric(value), fill=Mannschaften)) +ylab('Differenz der Heimvorteile')+
  geom_bar(stat="identity",position = 'dodge')+ xlab('Land') + ggtitle('Differenz der Heimvorteile (2020-2018)')+
  geom_text(aes(y=as.numeric(value), label=''), vjust=1.6, 
            color="white", size=3.5)+scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+  theme(axis.text.x=element_text(color = "black", size=11, angle=45, vjust=.8, hjust=0.8))



#-------------------------------------------------------------------------------
#Creating fudge axis

CalcFudgeAxis = function( y1, y2=y1) {
  Cast2To1 = function(x) ((ylim1[2]-ylim1[1])/(ylim2[2]-ylim2[1])*x) # x gets mapped to range of ylim2
  ylim1 <- c(min(y1),max(y1))
  ylim2 <- c(min(y2),max(y2))    
  yf <- Cast2To1(y2)
  labelsyf <- pretty(y2)  
  return(list(
    yf=yf,
    labels=labelsyf,
    breaks=Cast2To1(labelsyf)
  ))
}

FudgeAxis <- CalcFudgeAxis(as.numeric(d2$Durchschnitt_Heimvorteil_2018), as.numeric(d2$Durchschnitt_Heimvorteil_2020 ))

#generating fudge axis represntatipn for big5 and other clubs

#OTHERS
ggplot(data=d2, aes(label=club)) + xlab('Jahr')+ ylab('Heimvorteilswert')+
  theme_bw() + 
  geom_segment(aes(x='2018', xend='2020', y=as.numeric(d2$Durchschnitt_Heimvorteil_2018), yend=FudgeAxis$yf), color=c('red','darkgreen','blue','brown','orange'),size = 0.5)+
  geom_text(aes(x='2018', y=as.numeric(d2$Durchschnitt_Heimvorteil_2018)), color=c('red','darkgreen','blue','brown','orange'),size = 4) +
  geom_text(aes(x='2020', y=as.numeric(FudgeAxis$yf)), color=c('red','darkgreen','blue','brown','orange'),size = 4) +
  theme(legend.position='none', panel.grid=element_blank())



#BIG_5

FudgeAxis <- CalcFudgeAxis(as.numeric(d1$Durchschnitt_Heimvorteil_2018), as.numeric(d1$Durchschnitt_Heimvorteil_2020 ))

ggplot(data=d1, aes(label=club)) + xlab('Jahr')+ ylab('Heimvorteilswert')+
  theme_bw() + 
  geom_segment(aes(x='2018', xend='2020', y=as.numeric(d1$Durchschnitt_Heimvorteil_2018), yend=FudgeAxis$yf), color=c('red','darkgreen','blue','brown','orange'),size = 0.5)+
  geom_text(aes(x='2018', y=as.numeric(d1$Durchschnitt_Heimvorteil_2018)), color=c('red','darkgreen','blue','brown','orange'),size = 4) +
  geom_text(aes(x='2020', y=as.numeric(FudgeAxis$yf)), color=c('red','darkgreen','blue','brown','orange'),size = 4) +
  theme(legend.position='none', panel.grid=element_blank())

#--------------------------------------------------------------------------------
#Table

tbody.style = tbody_style(color = "black",
                          fill = c("#e8f3de", "#d3e8bb"), hjust=1, x=0.9)

Land = c("÷sterreich","Bulgarien","Kroatien","Zypern",
         "D‰nemark","Estland","Finnland","Frankreich",
         "Deutschland","Griechenland","Ungarn","Irland","Italien",
         "Malta","Niederlande","Polen",
         "Portugal","Slowakei","Spanien",
         "Schweden","England")




df_2020_ = as.data.frame(cbind(Land,df_2020$Durchschnitt_Heimvorteil_2020))
names(df_2020_)[2]= "Durchschnitt_Heimvorteil_2020"
df_2020_$Durchschnitt_Heimvorteil_2020 = round(as.numeric(df_2020_$Durchschnitt_Heimvorteil_2020),3)

#showing country names and home advantage in table representation for year 2020
tab = ggtexttable(df_2020_, rows = NULL,
                  theme = ttheme(
                    colnames.style = colnames_style(color = "white", fill = "#8cc257"),
                    tbody.style = tbody.style
                  )
)
tab

#2018--------------------------------------------------------------


#showing country names and home advantage in table representation for year 2018

#
df_2018_ = as.data.frame(cbind(Land,df_2018$Durchschnitt_Heimvorteil_2018))
names(df_2018_)[2]= "Durchschnitt_Heimvorteil_2018"
df_2018_$Durchschnitt_Heimvorteil_2018 = round(as.numeric(df_2018_$Durchschnitt_Heimvorteil_2018),3)
tab1 = ggtexttable(df_2018_, rows = NULL,
                  theme = ttheme(
                    colnames.style = colnames_style(color = "white", fill = "#8cc257"),
                    tbody.style = tbody.style
                  )
)
tab1


