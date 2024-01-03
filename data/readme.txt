#README FILE

##########################################################################################################################################################################
Incase the R code doesn't work due to change in website link or change in table names. The data folder can be used to load the data. 

1.HA_2018 and HA_2020: These files have 2 columns country and Durchschnitt_Heimvorteil. Country represents the respective European league and Durchschnitt_Heimvorteil 
represents the respective calculated mean home advantage value. This data is used to create heat map and barplots mentioned in heatmap_barplot_HA.R file. The same data is used
to create bar plots, fudge axis graphs and tables that are discussed in Big5_vs_others.R file.

2. England_2018 and England_2020:

These data files correspond to the home advantage of different England clubs for years 2018 and 2020 respectively. 
club is the club name, home_PPS is the home points per game, away_PPS are points per game away, home_adv is the home advantage value. This data is used in file England_graphs.R


3. Germany_2018 and Germany_2020:

These data files correspond to the home advantage of different Germany clubs for years 2018 and 2020 respectively. 
club is the club name, home_PPS is the home points per game, away_PPS are points per game away, home_adv is the home advantage value. This data is used in file Germany_graphs.R

4.pValues

This data file contains calculated pvalues for respective League. The column league corresponds to respective country league and pvalue contains the calculated pvalue.

5.possesion_buli

This file contains possession data which was collected manually from kicker. The column team corresponds to the respective german club, 18 average and 18 away corresponds to the possesion average 
and away for year 2018, 20 average and 20 away corresponds to the possesion average and away for year 2020.

