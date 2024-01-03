#README FILE

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
It is mandatory that the code is run in a sequence as the previous data is dependent on current calculated. 

Excecution order for files:
heatmap_barplot_HA.R -> Big5_vs_others.R _> England_graphs.R -> Germany_graph.R -> p-values.R -> audience-analysis.R

possesion.R is independent file

Incase the R code doesn't work due to change in website link or change in table names. The data folder can be used to load the data. 
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
1. heatmap_barplot_HA.R

The heatmap_barplot_HA.R file extracts the home advantage data from https://www.soccerstats.com/homeaway.asp?league='RESPECTIVE_LEAGUE_NAME' for all the 21 European countries for years
2018 and 2020.
The graphs corresponding to the heat map representation for both years 2018 and 2020 is generated by this file. The 'dodge' bar plots for home advantage (respective european league)
for both years is generated using this file.

2. Big5_vs_others.R

The bar chart that compares difference in home advantage values for big 5 and other small leagues is generated by this file. The fudge axis graph corresponding to big 5 and others
is also generated using this file. The tabular representation of mean home advantage value for 21 European leagues for 2018 and 2020 is also generated by this file.

3. England_graphs.R

The stacked home-away bar graphs for years 2018 and 2020 for England league is generated by this file. The dodge bar graph representation for england is also generated by this file.

4.Germany_graph.R

The dodge bar graph representation for Germany (Bundesliga) is generated by this file.

5. p-values.R

The p-values for 21 European leagues is calculated by this file. We represent the calculated p values in tabular and graphs (lolipop) in this file.

6.audience-analysis.R

The effect of audience analysis on home advantage of England clubs is generated in this file (2018 and 2020 years separately and both together in same scatter plot).

7.possesion.R

The possesion bar graphs are generated by this file.
