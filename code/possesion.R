library("readxl")
library(RColorBrewer)
library(ggplot2)
library(dplyr)

#loading collected possession data
data <- as.data.frame(read_excel('C:\\Users\\Aishwarya\\Downloads\\posession_buli.xlsx'))

away_18 <- select(data,`18 away`,Team)
away_18 <- cbind(away_18, Jahr = '2018')
away_20 <- select(data,`21 away`,Team)
away_20 <- cbind(away_20, Jahr = '2020')

average_18 <- select(data,`18 average`,Team)
average_18 <- cbind(average_18, Jahr = '2018')
average_20 <- select(data,`21 average`,Team)
average_20 <- cbind(average_20, Jahr = '2020')

colnames(average_18)[1] = 'average'
colnames(average_20)[1] = 'average'

#calculating difference  for both years

diff_20 <- away_20$`21 away` - average_20$average
away_20 <- cbind(away_20, difference = diff_20)

diff_18 <- away_18$`18 away` - average_18$average
away_18 <- cbind(away_18, difference = diff_18)

average <- rbind(average_18,average_20)

colnames(away_18)[1] = 'away'
colnames(away_20)[1] = 'away'

away <- rbind(away_18,away_20)


#plotting barplots with away possesision values 
ggplot(away, aes(y= away, x = Team, fill = Jahr)) + geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+ ylab('ein Weg') + xlab('Team')+
  theme_minimal()+theme(axis.text.x = element_text(angle = 90))

ggplot(away, aes(y= away, x = Team, fill = Jahr)) + geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+ ylab('ein Weg') + xlab('Team')+
  theme_minimal()+theme(axis.text.x = element_text(angle = 90))


#plotting barplots with calculated difference values
ggplot(away_18, aes(y= difference, x = Team)) + geom_bar(stat="identity", position=position_dodge(),fill = '#00AFBB')+
 ylab('Unterschied (Ballbesitz Differenz (auswärts-gesamtdurchschnitt))') + xlab('Team')+ ggtitle('Jahr 2018')+
  theme_minimal()+theme(axis.text.x = element_text(angle = 90)) 

ggplot(away_20, aes(y= difference, x = Team)) + geom_bar(stat="identity", position=position_dodge(),fill = '#00AFBB')+
  ylab('Ballbesitz Differenz (auswärts-gesamtdurchschnitt)') + xlab('Team')+ggtitle('Jahr 2020')+
  theme_minimal()+theme(axis.text.x = element_text(angle = 90)) 


#plotting both the years (difference values) in same graph
ggplot(away, aes(y= difference, x = Team, fill =Jahr )) + geom_bar(stat="identity", position=position_dodge())+
  scale_fill_manual(values=c("#00AFBB", "#E7B800")) + ylab('Ballbesitz Differenz (auswärts-gesamtdurchschnitt)') + xlab('Team')+
  theme_minimal()+theme(axis.text.x = element_text(angle = 90)) 
