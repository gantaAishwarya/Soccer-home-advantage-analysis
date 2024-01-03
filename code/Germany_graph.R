
link <- "https://www.soccerstats.com/homeaway.asp?league=germany_2021"

# read table
tab <- 
  read_html(link) %>% 
  html_nodes("table#btable") %>%
  html_table()

data_germany <- data.frame(tab[3])
data_germany <- select(data_germany,Var.2,Points.Per.Game,Points.Per.Game.1,Home.advantage.1)
data_germany =  data_germany %>% mutate(data_germany,Club = Var.2,"2020/21" = Home.advantage.1)

data_germany <- filter(data_germany,Club!="")
data_germany$`2020/21` = as.numeric(data_germany$`2020/21`)



###############################################################################
#germany 2018- home vs away

link <- "https://www.soccerstats.com/homeaway.asp?league=germany_2019"

# read table
tab <- 
  read_html(link) %>% 
  html_nodes("table") %>%
  html_table()

data_germany_2018 <- data.frame(tab[14])

data_germany_2018 <- select(data_germany_2018,Var.2,Points.Per.Game,Points.Per.Game.1,Home.advantage.1)
data_germany_2018 =  data_germany_2018 %>% mutate(data_germany_2018,Club = Var.2,"2018/19" = Home.advantage.1)


data_germany_2018 <- filter(data_germany_2018,Club!="")

data_germany_2018$`2018/19` = as.numeric(data_germany_2018$`2018/19`)


###############################################################################


home_away_data_germany = select(data_germany, Club,`2020/21` )
home_away_data_2018_germany = select(data_germany_2018, Club,`2018/19`)

home_away_data_germany = filter(home_away_data_germany ,home_away_data_germany$Club %in% home_away_data_2018_germany$Club )
home_away_data_2018_germany =filter(home_away_data_2018_germany ,home_away_data_2018_germany$Club %in% home_away_data_germany$Club)


home_away_data_2018_germany$HomeAdvantage_2018 = as.numeric(home_away_data_2018_germany$HomeAdvantage_2018)
home_away_data_2018_germany 


data.m <- melt(home_away_data_2018_germany, id.vars='Club')
data <- melt(home_away_data_germany, id.vars='Club') 

df = rbind(data.m,data)

df  =  mutate(df,Jahr = variable)


#generating dodge bar plots with home advantage values vs clubs for all german 
#league clubs for years 2018 and 2020

ggplot(df, aes(x=Club, y= value, fill= Jahr, label ="", width = 0.6)) +
  ggtitle("Heimvorteil - Vor und während Corona")  + geom_bar(stat="identity",position = "dodge", color="#e9ecef",alpha=0.6)+scale_fill_manual(values=c("#69b3a2", "#404080"))+
  theme(legend.position="bottom")+
  theme(axis.text.x = element_text(angle = 90))+
  scale_y_continuous(breaks = seq(-0.7, 1, 0.2)) + geom_text(size = 4, position = position_dodge2(width = 0.3), vjust=-0.25) + ylab("Heimvorteil")+xlab('Verein') +
  geom_hline(yintercept = mean(home_away_data_2018_germany$`2018/19`), color="#69b3a2",linetype='dashed',size=0.8)+
  geom_hline(yintercept = 0,color= "black")+geom_hline(yintercept = mean(home_away_data_germany$`2020/21`),color= "#404080",size=0.8,linetype='dashed')+
  geom_text(aes(6,0.90,label = 0.89, hjust = 0.5))+geom_text(aes(6,-0.41,label = -0.41, hjust = 0.5))+
geom_text(aes(13,0.94,label = 0.94, hjust = 0.5))+geom_text(aes(13,-0.17,label = -0.17, hjust = 0.5))

ggplot(df, aes(x=value, y= Club, fill= Jahr, label ="", width = 0.6)) +
  ggtitle("Heimvorteil - Vor und während Corona")  + geom_bar(stat="identity",position = "dodge", color="#e9ecef",alpha=0.6)+scale_fill_manual(values=c("#69b3a2", "#404080"))+
  theme(legend.position="bottom")+
  scale_x_continuous(breaks = seq(-0.5, 1, 0.2)) + geom_text(size = 4, position = position_dodge2(width = 0.3), vjust=-0.25) + xlab("Heimvorteil")+ylab('Verein') +
  geom_vline(xintercept = mean(home_away_data_2018_germany$`2018/19`), color="#69b3a2",linetype='dashed',size=0.8)+
  geom_vline(xintercept = 0,color= "black")+geom_vline(xintercept = mean(home_away_data_germany$`2020/21`),color= "#404080",size=0.8,linetype='dashed')+
   geom_text(aes(0.90,6,label = 0.89, vjust = 0.5))+geom_text(aes(-0.41,6,label = -0.41, vjust = 0.5))+
  geom_text(aes(0.94,13,label = 0.94, vjust = 0.5))+geom_text(aes(-0.17,13,label = -0.17, vjust = 0.5))



