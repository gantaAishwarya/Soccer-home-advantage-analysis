#loading required libraries

library(ggplot2)
library(tidyr)
library(rvest)
library('rstatix')
library(rworldmap)
library(mapproj)
library(dplyr)
library(reshape)

#Getting entire world map
worldMap <- getMap()

# Member States of the European Union
#Selecting european countries whose data is available for both years 2018 and 2020

europeanUnion <- c("Austria","Bulgaria","Croatia","Cyprus",
                   "Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy",
                   "Malta","Netherlands","Poland",
                   "Portugal","Slovakia","Spain",
                   "Sweden","England")

df = data.frame("")
df_club = data.frame("")

for(i in europeanUnion){
  if (i=='Estonia' || i =='Finland' || i =='Ireland' || i == 'Sweden'){
    link <- paste("https://www.soccerstats.com/homeaway.asp?league=",i,'_2020',sep = "")
  }
  else{
  link <- paste("https://www.soccerstats.com/homeaway.asp?league=",i,'_2021',sep = "")
  }
  #Getting entire table data
  tab <- 
    read_html(link) %>% 
    html_nodes("table") %>%
    html_table()
  
  #selecting only table required table data 
  data  = data.frame(tab[14])

  if (i== 'Austria' ||i== 'Cyprus' || i=='Bulgaria' || i == 'Croatia' || i =='Denmark' || i =='France' || i=='Greece' || i=='England' || i=='Slovakia' || i =='Hungary'||i=='Malta' || i=='Poland' || i == 'Estonia' || i == "Finland" || i =="Germany" || i =="Italy" || i =="Netherlands" || i =="Portugal" || i == "Spain"){
      data = filter(data,data$Var.2!="")
      assign(paste('club_',i,sep=""), as.numeric(data$Home.advantage.1))
      assign(paste('club_name_',i,sep=""), data$Var.2)
      HA = mean(as.numeric(data$Home.advantage.1))
  }
  else{
  data = filter(data,data$X2!="")
  assign(paste('club_',i,sep=""), as.numeric(data$X9))
  assign(paste('club_name_',i,sep=""), data$X2)
  HA = mean(as.numeric(data$X9))
  }

  #data = data %>% mutate(data,average_HA = mean(HA))
  #appending calculated mean home advantage to the dataframe
  df = rbind(df,HA)
}

value = as.numeric(df[-1,])

df_2020 = value
df_2020 = data.frame(cbind(round(as.numeric(df_2020),3),europeanUnion))
df_2020 = mutate(df_2020,Durchschnitt_Heimvorteil_2020 = V1, Country = europeanUnion)
df_2020 = select(df_2020,Country,Durchschnitt_Heimvorteil_2020)


# Select only the index of states member of the E.U.
indEU <- which(worldMap$NAME%in%europeanUnion)

# Extract longitude and latitude border's coordinates of members states of E.U. 
europeCoords <- lapply(indEU, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})

europeCoords <- do.call("rbind", europeCoords)

europeanUnionTable <- data.frame(country = europeanUnion, value = value)


europeCoords$value <- europeanUnionTable$value[match(europeCoords$region,europeanUnionTable$country)]

P <- ggplot() + ggtitle("Heimvorteil während Corona")+geom_polygon(data = europeCoords, aes(x = long, y = lat, group = region, fill = value),
                                                                   colour = "black", size = 0.1) +
  coord_map(xlim = c(-13, 35),  ylim = c(32, 71))

P <- P + scale_fill_gradient(name = "Heimvorteilwert", high = "#FF0000FF", low = "#FFFF00FF", 
                             na.value = "grey50",limits=c(-0.8,0.8))

P2 <- P + theme(#panel.grid.minor = element_line(colour = NA), panel.grid.minor = element_line(colour = NA),
  #panel.background = element_rect(fill = NA, colour = NA),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(), axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(), axis.title = element_blank(),
  #rect = element_blank(),
  plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))

#showing world map with calculated home advantage values
P2
#-------------------------------------------------------------------------------


#2018 HOME ADVANTAGE
#repeating same for year 2018

df = data.frame("")
for(i in europeanUnion){
    if (i=='Estonia' || i =='Finland' || i =='Ireland' || i == 'Sweden'){
      link <- paste("https://www.soccerstats.com/homeaway.asp?league=",i,'_2018',sep = "")
    }
    else{
      link <- paste("https://www.soccerstats.com/homeaway.asp?league=",i,'_2019',sep = "")
    }
  tab <- 
    read_html(link) %>% 
    html_nodes("table") %>%
    html_table()
  data  = data.frame(tab[14])
  if(i == "Germany"|| i== "Netherlands" || i == "Portugal" || i =="Spain"){
    data = filter(data,data$Var.2!="")
    HA = mean(as.numeric(data$Home.advantage.1))
    assign(paste('club_2018_',i,sep=""), as.numeric(data$Home.advantage.1))
    assign(paste('club_2018_name_',i,sep=""), data$Var.2)
  }
  else{
    data = filter(data,data$X2!="")
    HA = mean(as.numeric(data$X9))
    assign(paste('club_2018_',i,sep=""), as.numeric(data$X9))
    assign(paste('club_2018_name_',i,sep=""), data$X2)
    
  }
  
  #data = data %>% mutate(data,average_HA = mean(HA))
  df = rbind(df,HA)
}

value = as.numeric(df[-1,])


# Select only the index of states member of the E.U.
indEU <- which(worldMap$NAME%in%europeanUnion)

# Extract longitude and latitude border's coordinates of members states of E.U. 
europeCoords <- lapply(indEU, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})

europeCoords <- do.call("rbind", europeCoords)

europeanUnionTable <- data.frame(country = europeanUnion, value = value)
europeCoords$value <- europeanUnionTable$value[match(europeCoords$region,europeanUnionTable$country)]


P <- ggplot()+ ggtitle("Heimvorteil vor Corona") + geom_polygon(data = europeCoords, aes(x = long, y = lat, group = region, fill = value),
                                                                      colour = "black", size = 0.1) +
  coord_map(xlim = c(-13, 35),  ylim = c(32, 71))

P1 <- P + scale_fill_gradient(name = "Heimvorteilwert", high = "#FF0000FF",
                        low = "#FFFF00FF", na.value = "grey50",limits=c(-0.8,0.8)) +xlab("")+ylab("")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(), axis.title = element_blank(),
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) 

P1

df_2018 = value
df_2018 = data.frame(cbind(round(as.numeric(df_2018),3),europeanUnion))
df_2018 = mutate(df_2018,Durchschnitt_Heimvorteil_2018 = V1, Country = europeanUnion)
df_2018 = select(df_2018,Country,Durchschnitt_Heimvorteil_2018)


plot_grid(P1, P2, labels = c('Jahr 2018', 'Jahr 2020'))

#-------------------------------------------------------------------------------
#Barplots comparison

club = c("Österreich","Bulgarien","Kroatien","Zypern",
         "Dänemark","Estland","Finnland","Frankreich",
         "Deutschland","Griechenland","Ungarn","Irland","Italien",
         "Malta","Niederlande","Polen",
         "Portugal","Slowakei","Spanien",
         "Schweden","England")

common <- intersect(df_2020$Country, df_2018$Country) 

df1 = df_2020 %>% filter(df_2020$Country %in% common)
df2 = df_2018 %>% filter(df_2018$Country %in% common)

df1 = cbind(df1,club)
df2 = cbind(df2,club)

df1 = df1[-1]
df2 = df2[-1]

data_1 <- melt(df1, id.vars='club')
data_2 <- melt(df2, id.vars='club') 

df_ = rbind(data_1,data_2)

df_  =  mutate(df_, Jahr = variable)

#generating dodge bar plots for home advantage values for 21 european soccer clubs
ggplot(df_, aes(x=club, y= as.numeric(value), fill= Jahr, alpha= 1.5, label ="",width=.5)) + ggtitle("Durchschnittlicher Heimvorteil 2020 vs. 2018")+ geom_bar(stat="identity",position = "dodge",alpha=1.9)+
  theme(legend.position="bottom")+ ylab('Wert des Heimvorteils')+xlab('Land') + theme(axis.text.x = element_text(angle = 90)) + scale_fill_brewer(palette="Paired") 







