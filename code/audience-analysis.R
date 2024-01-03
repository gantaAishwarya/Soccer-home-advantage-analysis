library(ggrepel)
library(stringr)
library(dplyr)
library(reshape)

#Link of audience percentage data 
link <- 'https://www.transfermarkt.com/premier-league/besucherzahlen/wettbewerb/GB1/saison_id/2020/plus/1'

#loading all tables of the link
tab <- 
  read_html(link) %>% 
  html_nodes("table") %>%
  html_table()

#selecting required table
data = as.data.frame(tab[4])
data = select(data,Average ,Var.11)


#data cleaning
data = filter(data,Var.11!="")
data$Var.11 = str_replace_all(data$Var.11,"%"," ")
data$Var.11[c(19,20)] = 0
data$Var.11 =  as.numeric(data$Var.11)



#changing club names to maintain uniformity

club_2020_name_England = str_replace_all(club_name_England,'Utd','United')
data$Average = str_replace_all(data$Average,'FC','')
data = arrange(data,data$Average)

HA_england = as.data.frame(cbind(club_England,club_name_England))
club_2020_name_England = str_replace_all(club_2020_name_England,'Brighton','Brighton & Hove Albion')
club_2020_name_England = str_replace_all(club_2020_name_England,'Tottenham','Tottenham Hotspur')
club_2020_name_England = str_replace_all(club_2020_name_England,'West Brom','West Bromwich Albion')
club_2020_name_England = str_replace_all(club_2020_name_England,'Wolverhampton','Wolverhampton Wanderers')
HA_england$club_name_England = str_replace_all(HA_england$club_name_England,' ','')

HA_england  = arrange(as.data.frame(cbind(as.numeric(club_England),club_2020_name_England)),club_2020_name_England)

HA_england_2020 = cbind(HA_england,data$Var.11)
HA_england_2020$club_2020_name_England = str_replace_all(HA_england$club_2020_name_England,' ','')

HA_england_2020 = subset(HA_england_2020,club_2020_name_England != 'Fulham')

#genrating scatter plot which show how home advantage vary with audience percentage
ggplot(HA_england_2020, aes(`data$Var.11`,V1, label = club_2020_name_England)) + geom_point(col = 'red') + geom_text_repel()+
  xlab("Auslastung") + ylab("Heimvorteil")

--------------------------------------------------------------------------------
#repeating same for 2018 year
  
link <- 'https://www.transfermarkt.com/premier-league/besucherzahlen/wettbewerb/GB1/plus/1?saison_id=2018'

tab <- 
  read_html(link) %>% 
  html_nodes("table") %>%
  html_table()

data = as.data.frame(tab[4])
data = select(data,Average ,Var.11)
data = filter(data,Var.11!="")
data$Var.11 = str_replace_all(data$Var.11,"%"," ")
#data$Var.11[c(19,20)] = 0
data$Var.11 =  as.numeric(data$Var.11)

data$Average = str_replace_all(data$Average,'FC','')
data$Average = str_replace_all(data$Average,'A Bournemouth','Bournemouth')
data$Average = str_replace_all(data$Average,'Huddersfield Town','Huddersfield')
data$Average = str_replace_all(data$Average,' ','')
club_2018_name_England = str_replace_all(club_2018_name_England,'Brighton','Brighton & Hove Albion')
club_2018_name_England = str_replace_all(club_2018_name_England,'Tottenham','Tottenham Hotspur')

club_2018_name_England = str_replace_all(club_2018_name_England,' ','')
club_2018_name_England = str_replace_all(club_2018_name_England,'Utd','United')

data = filter(data,data$Average %in% club_2018_name_England)

data = arrange(data,data$Average)


HA_england  = arrange(as.data.frame(cbind(as.numeric(club_2018_England),club_2018_name_England)),club_2018_name_England)

HA_england = filter(HA_england, HA_england$club_2018_name_England %in% data$Average)
HA_england_2018 = cbind(HA_england,data$Var.11)
HA_england_2018$club_2018_name_England = str_replace_all(HA_england_2018$club_2018_name_England,' ','')

HA_england_2018 = subset(HA_england_2018,club_2018_name_England != 'Fulham')



ggplot(HA_england_2018, aes(HA_england_2018$`data$Var.11`,HA_england_2018$V1, label = HA_england_2018$club_2018_name_England)) + geom_point(col = 'red') + geom_text_repel()+
  xlab("Auslastung") + ylab("Heimvorteil")

--------------------------------------------------------------------------------
#selecting clubs thata re common for 2018 and 2020

common_data1 = filter(HA_england_2018, HA_england_2018$club_2018_name_England %in% HA_england_2020$club_2020_name_England)
common_data2  = filter(HA_england_2020, HA_england_2020$club_2020_name_England %in% HA_england_2018$club_2018_name_England)

common_data2 < cbind(common_data2,'2020')
common_data <- cbind(common_data1,common_data2$V1,common_data2$`data$Var.11`)

colnames(common_data1)[2] <- "club"
colnames(common_data2)[2] <- "club"

common_data2['Jahr']='2020'
common_data1['Jahr']='2018'

common_data = rbind(common_data1,common_data2)

common_data$V1 = as.numeric(common_data$V1)
common_data$`data$Var.11` = as.numeric(common_data$`data$Var.11`)


#Plotting both years in same scatter plot

ggplot(common_data, aes(common_data$`data$Var.11`,common_data$V1, label = club, colour = Jahr)) + geom_point()+theme(legend.position = "bottom") + geom_text()+
  xlab("Anwesenheitsprozentsatz") + ylab("Heimvorteil") + scale_y_continuous(breaks= seq(-0.80,1.63,.3)) 



