####################################################################
#Checking how away ponits per game vary for home points per game for the
#premier league


link <- "https://www.soccerstats.com/homeaway.asp?league=england_2021"

# read table
tab <- 
  read_html(link) %>% 
  html_nodes("table#btable") %>%
  html_table()

data_england <- data.frame(tab[3])
data_england <- select(data_england,Var.2,Points.Per.Game,Points.Per.Game.1,Home.advantage.1)
data_england =  data_england %>% mutate(data_england,Club = Var.2,"2020/21" = Home.advantage.1)

data_england <- filter(data_england,Club!="")
data_england$`2020/21` = as.numeric(data_england$`2020/21`)



home_points_2020 = select(data_england,Var.2,Points.Per.Game)
home_points_2020 = cbind(home_points_2020,'PPS'='Heim')
away_points_2020 = select(data_england,Var.2,Points.Per.Game.1)
away_points_2020 = cbind(away_points_2020,'PPS' = 'Auswärts')

colnames(away_points_2020)[2] <- "Points.Per.Game"

home_away = rbind(home_points_2020,away_points_2020)


ggplot(home_away, aes(fill=PPS, x=Var.2, y=as.numeric(Points.Per.Game))) + 
  geom_bar(position="stack", stat="identity") + scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_fill_viridis(discrete = T) +
  ggtitle("Heim vs. Auswärts PPS - 2020") + xlab('Vereine')+ ylab('Punkte pro Spiel')


###############################################################################
#England 2018- home vs away

link <- "https://www.soccerstats.com/homeaway.asp?league=england_2019"

# read table
tab <- 
  read_html(link) %>% 
  html_nodes("table") %>%
  html_table()

data_england_2018 <- data.frame(tab[14])
data_england_2018 <- select(data_england_2018,X2,X6,X7,X9)
data_england_2018 =  data_england_2018 %>% mutate( Club = X2,"2018/19" = X9)

data_england_2018 <- filter(data_england_2018,Club!="")

data_england_2018$`2018/19` = as.numeric(data_england_2018$`2018/19`)


home_points_2018 = select(data_england_2018,X2,X6)
home_points_2018 = cbind(home_points_2018,'PPS'='Heim ')
away_points_2018 = select(data_england_2018,X2,X7)
away_points_2018 = cbind(away_points_2018,'PPS'='Auswärts')

colnames(away_points_2018)[2] <- "Points.Per.Game"
colnames(home_points_2018)[2] <- "Points.Per.Game"

home_away_2018 = rbind(home_points_2018,away_points_2018)


write.csv(data_england_2018,"C:\\Users\\Aishwarya\\Documents\\dortmun-digital-classes\\Semetser 3\\SportsDataVisualization\\Code\\England_2018.csv", row.names = FALSE)

#Generating stacked bar graphs showing how home vs away PPS vary
ggplot(home_away_2018, aes(fill=PPS, x=X2, y=as.numeric(Points.Per.Game))) + 
  geom_bar(position="stack", stat="identity") + scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_fill_viridis(discrete = T) +
  ggtitle("Heim vs. Auswärts PPS - 2018") + xlab('Vereine')+ ylab('Punkte pro Spiel')


###############################################################################


home_away_data_england = select(data_england, Club,`2020/21` )
home_away_data_2018_england = select(data_england_2018, Club,`2018/19`)

home_away_data_england = filter(home_away_data_england ,home_away_data_england$Club %in% home_away_data_2018_england$Club )
home_away_data_2018_england =filter(home_away_data_2018_england ,home_away_data_2018_england$Club %in% home_away_data_england$Club)


home_away_data_2018_england$HomeAdvantage_2018 = as.numeric(home_away_data_2018_england$HomeAdvantage_2018)
home_away_data_2018_england 


data.m <- melt(home_away_data_2018_england, id.vars='Club')
data <- melt(home_away_data_england, id.vars='Club') 

df = rbind(data.m,data)

df  =  mutate(df,Jahr = variable)


#generating dodge bar plots with home advantage values vs clubs for all premier 
#league clubs for years 2018 and 2020

ggplot(df, aes(x=Club, y= value, fill= Jahr, label ="")) +
  ggtitle("Heimvorteil - Vor und während Corona")  + geom_bar(stat="identity",position = "dodge", color="#e9ecef",alpha=0.6)+scale_fill_manual(values=c("#69b3a2", "#404080"))+
  theme(legend.position="bottom")+
  theme(axis.text.x = element_text(angle = 90))+
  scale_y_continuous(breaks = seq(-0.7, 1, 0.2)) + geom_text(size = 4, position = position_dodge2(width = 0.3), vjust=-0.25) + ylab("Heimvorteil")+xlab('Verein') +
  geom_hline(yintercept = mean(home_away_data_2018_england$`2018/19`), color="#69b3a2",linetype='dashed',size=0.8)+
  geom_hline(yintercept = 0,color= "black")+geom_hline(yintercept = mean(home_away_data_england$`2020/21`),color= "#404080",size=0.8,linetype='dashed')+
  geom_text(aes(7,0.85,label = 0.85, hjust = 0.5))+geom_text(aes(7,-0.42,label = -0.42, hjust = 0.5))+
  geom_text(aes(11,0.31,label = 0.31, hjust = 0.5))+geom_text(aes(11,-0.63,label = -0.63, hjust = 0.5))


###############################################################################
