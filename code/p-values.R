
list_2018 = c(club_2018_Austria,club_2018_Bulgaria,club_2018_Croatia,club_2018_Cyprus,club_2018_Denmark,club_2018_Estonia,
              club_2018_Finland,club_2018_France,club_2018_Germany,club_2018_Greece,club_2018_Hungary,club_2018_Ireland,club_2018_Italy,
            club_2018_Malta,club_2018_Netherlands,club_2018_Poland,club_2018_Portugal,club_2018_Slovakia,club_2018_Spain,
              club_2018_Sweden,club_2018_England)

list_2020 = c(club_Austria,club_Bulgaria,club_Croatia,club_Cyprus,club_Denmark,club_Estonia,club_Finland,club_France,club_Germany,club_Greece,club_Hungary,club_Ireland,club_Italy,
                       club_Malta,club_Netherlands,club_Poland,club_Portugal,club_Slovakia,club_Spain,
                       club_Sweden,club_England)


c = intersect(club_2018_name_Austria,club_name_Austria)

austria = data.frame(cbind(club_2018_name_Austria,as.numeric(club_2018_Austria)))
austria_2020 = data.frame(cbind(club_name_Austria,as.numeric(club_Austria)))

data_2020 = austria_2020 %>% filter(club_name_Austria %in% c)
data_2018 = austria %>% filter(club_2018_name_Austria %in% c)

data_2020_sort <- with(data_2020,  data_2020[order(club_name_Austria) , ])
data_2018_sort <- with(data_2018,  data_2018[order(club_2018_name_Austria) , ])

paired_test  = t.test(as.numeric(data_2020_sort$V2),as.numeric(data_2018_sort$V2),
                      alternative = "less", var.equal = FALSE,paired = TRUE)

austria_val = paired_test$p.value

c = intersect(club_2018_name_Bulgaria,club_name_Bulgaria)

Bulgaria = data.frame(cbind(club_2018_name_Bulgaria,as.numeric(club_2018_Bulgaria)))
Bulgaria_2020 = data.frame(cbind(club_name_Bulgaria,as.numeric(club_Bulgaria)))

data_2020 = Bulgaria_2020 %>% filter(club_name_Bulgaria %in% c)
data_2018 = Bulgaria %>% filter(club_2018_name_Bulgaria %in% c)

data_2020_sort <- with(data_2020,  data_2020[order(club_name_Bulgaria) , ])
data_2018_sort <- with(data_2018,  data_2018[order(club_2018_name_Bulgaria) , ])

paired_test  = t.test(as.numeric(data_2020_sort$V2),as.numeric(data_2018_sort$V2),
                      alternative = "less", var.equal = FALSE,paired = TRUE)

Bulgaria_val = paired_test$p.value

c = intersect(club_2018_name_Croatia,club_name_Croatia)

Croatia = data.frame(cbind(club_2018_name_Croatia,as.numeric(club_2018_Croatia)))
Croatia_2020 = data.frame(cbind(club_name_Croatia,as.numeric(club_Croatia)))

data_2020 = Croatia_2020 %>% filter(club_name_Croatia %in% c)
data_2018 = Croatia %>% filter(club_2018_name_Croatia %in% c)

data_2020_sort <- with(data_2020,  data_2020[order(club_name_Croatia) , ])
data_2018_sort <- with(data_2018,  data_2018[order(club_2018_name_Croatia) , ])

paired_test  = t.test(as.numeric(data_2020_sort$V2),as.numeric(data_2018_sort$V2),
                      alternative = "less", var.equal = FALSE,paired = TRUE)

Croatia_val = paired_test$p.value

c = intersect(club_2018_name_Cyprus,club_name_Cyprus)

Cyprus = data.frame(cbind(club_2018_name_Cyprus,as.numeric(club_2018_Cyprus)))
Cyprus_2020 = data.frame(cbind(club_name_Cyprus,as.numeric(club_Cyprus)))

data_2020 = Cyprus_2020 %>% filter(club_name_Cyprus %in% c)
data_2018 = Cyprus %>% filter(club_2018_name_Cyprus %in% c)

data_2020_sort <- with(data_2020,  data_2020[order(club_name_Cyprus) , ])
data_2018_sort <- with(data_2018,  data_2018[order(club_2018_name_Cyprus) , ])

paired_test  = t.test(as.numeric(data_2020_sort$V2),as.numeric(data_2018_sort$V2),
                      alternative = 'less', var.equal = FALSE,paired = TRUE)

Cyprus_val = paired_test$p.value

c = intersect(club_2018_name_Denmark,club_name_Denmark)

Denmark = data.frame(cbind(club_2018_name_Denmark,as.numeric(club_2018_Denmark)))
Denmark_2020 = data.frame(cbind(club_name_Denmark,as.numeric(club_Denmark)))

data_2020 = Denmark_2020 %>% filter(club_name_Denmark %in% c)
data_2018 = Denmark %>% filter(club_2018_name_Denmark %in% c)

data_2020_sort <- with(data_2020,  data_2020[order(club_name_Denmark) , ])
data_2018_sort <- with(data_2018,  data_2018[order(club_2018_name_Denmark) , ])

paired_test  = t.test(as.numeric(data_2020_sort$V2),as.numeric(data_2018_sort$V2),
                      alternative = "less", var.equal = FALSE,paired = TRUE)

Denmark_val = paired_test$p.value

c = intersect(club_2018_name_Estonia,club_name_Estonia)

Estonia = data.frame(cbind(club_2018_name_Estonia,as.numeric(club_2018_Estonia)))
Estonia_2020 = data.frame(cbind(club_name_Estonia,as.numeric(club_Estonia)))

data_2020 = Estonia_2020 %>% filter(club_name_Estonia %in% c)
data_2018 = Estonia %>% filter(club_2018_name_Estonia %in% c)

data_2020_sort <- with(data_2020,  data_2020[order(club_name_Estonia) , ])
data_2018_sort <- with(data_2018,  data_2018[order(club_2018_name_Estonia) , ])

paired_test  = t.test(as.numeric(data_2020_sort$V2),as.numeric(data_2018_sort$V2),
                      alternative = "less", var.equal = FALSE,paired = TRUE)

Estonia_val = paired_test$p.value

c = intersect(club_2018_name_Finland,club_name_Finland)

Finland = data.frame(cbind(club_2018_name_Finland,as.numeric(club_2018_Finland)))
Finland_2020 = data.frame(cbind(club_name_Finland,as.numeric(club_Finland)))


data_2020 = Finland_2020 %>% filter(club_name_Finland %in% c)
data_2018 = Finland %>% filter(club_2018_name_Finland %in% c)

data_2020_sort <- with(data_2020,  data_2020[order(club_name_Finland) , ])
data_2018_sort <- with(data_2018,  data_2018[order(club_2018_name_Finland) , ])

paired_test  = t.test(as.numeric(data_2020_sort$V2),as.numeric(data_2018_sort$V2),
                      alternative = "less", var.equal = FALSE,paired = TRUE)

Finland_val = paired_test$p.value

c = intersect(club_2018_name_France,club_name_France)

France = data.frame(cbind(club_2018_name_France,as.numeric(club_2018_France)))
France_2020 = data.frame(cbind(club_name_France,as.numeric(club_France)))

data_2020 = France_2020 %>% filter(club_name_France %in% c)
data_2018 = France %>% filter(club_2018_name_France %in% c)

data_2020_sort <- with(data_2020,  data_2020[order(club_name_France) , ])
data_2018_sort <- with(data_2018,  data_2018[order(club_2018_name_France) , ])

paired_test  = t.test(as.numeric(data_2020_sort$V2),as.numeric(data_2018_sort$V2),
                      alternative = "less", var.equal = FALSE,paired = TRUE)

France_val = paired_test$p.value

c = intersect(club_2018_name_Germany,club_name_Germany)

Germany = data.frame(cbind(club_2018_name_Germany,as.numeric(club_2018_Germany)))
Germany_2020 = data.frame(cbind(club_name_Germany,as.numeric(club_Germany)))

data_2020 = Germany_2020 %>% filter(club_name_Germany %in% c)
data_2018 = Germany %>% filter(club_2018_name_Germany %in% c)

data_2020_sort <- with(data_2020,  data_2020[order(club_name_Germany) , ])
data_2018_sort <- with(data_2018,  data_2018[order(club_2018_name_Germany) , ])

paired_test  = t.test(as.numeric(data_2020_sort$V2),as.numeric(data_2018_sort$V2),
                      alternative = "less", var.equal = FALSE,paired = TRUE)

Germany_val = paired_test$p.value


c = intersect(club_2018_name_Greece,club_name_Greece)

Greece = data.frame(cbind(club_2018_name_Greece,as.numeric(club_2018_Greece)))
Greece_2020 = data.frame(cbind(club_name_Greece,as.numeric(club_Greece)))

data_2020 = Greece_2020 %>% filter(club_name_Greece %in% c)
data_2018 = Greece %>% filter(club_2018_name_Greece %in% c)

data_2020_sort <- with(data_2020,  data_2020[order(club_name_Greece) , ])
data_2018_sort <- with(data_2018,  data_2018[order(club_2018_name_Greece) , ])

paired_test  = t.test(as.numeric(data_2020_sort$V2),as.numeric(data_2018_sort$V2),
                      alternative = "less", var.equal = FALSE,paired = TRUE)

Greece_val = paired_test$p.value


c = intersect(club_2018_name_Hungary,club_name_Hungary)

Hungary = data.frame(cbind(club_2018_name_Hungary,as.numeric(club_2018_Hungary)))
Hungary_2020 = data.frame(cbind(club_name_Hungary,as.numeric(club_Hungary)))

data_2020 = Hungary_2020 %>% filter(club_name_Hungary %in% c)
data_2018 = Hungary %>% filter(club_2018_name_Hungary %in% c)

data_2020_sort <- with(data_2020,  data_2020[order(club_name_Hungary) , ])
data_2018_sort <- with(data_2018,  data_2018[order(club_2018_name_Hungary) , ])

paired_test  = t.test(as.numeric(data_2020_sort$V2),as.numeric(data_2018_sort$V2),
                      alternative = "less", var.equal = FALSE,paired = TRUE)

Hungary_val = paired_test$p.value


c = intersect(club_2018_name_Ireland,club_name_Ireland)

Ireland = data.frame(cbind(club_2018_name_Ireland,as.numeric(club_2018_Ireland)))
Ireland_2020 = data.frame(cbind(club_name_Ireland,as.numeric(club_Ireland)))

data_2020 = Ireland_2020 %>% filter(club_name_Ireland %in% c)
data_2018 = Ireland %>% filter(club_2018_name_Ireland %in% c)

data_2020_sort <- with(data_2020,  data_2020[order(club_name_Ireland) , ])
data_2018_sort <- with(data_2018,  data_2018[order(club_2018_name_Ireland) , ])

paired_test  = t.test(as.numeric(data_2020_sort$V2),as.numeric(data_2018_sort$V2),
                      alternative = "less", var.equal = FALSE,paired = TRUE)

Ireland_val = paired_test$p.value


c = intersect(club_2018_name_Italy,club_name_Italy)

Italy = data.frame(cbind(club_2018_name_Italy,as.numeric(club_2018_Italy)))
Italy_2020 = data.frame(cbind(club_name_Italy,as.numeric(club_Italy)))

data_2020 = Italy_2020 %>% filter(club_name_Italy %in% c)
data_2018 = Italy %>% filter(club_2018_name_Italy %in% c)

data_2020_sort <- with(data_2020,  data_2020[order(club_name_Italy) , ])
data_2018_sort <- with(data_2018,  data_2018[order(club_2018_name_Italy) , ])

paired_test  = t.test(as.numeric(data_2020_sort$V2),as.numeric(data_2018_sort$V2),
                      alternative = "less", var.equal = FALSE,paired = TRUE)

Italy_val = paired_test$p.value


c = intersect(club_2018_name_Malta,club_name_Malta)

Malta = data.frame(cbind(club_2018_name_Malta,as.numeric(club_2018_Malta)))
Malta_2020 = data.frame(cbind(club_name_Malta,as.numeric(club_Malta)))

data_2020 = Malta_2020 %>% filter(club_name_Malta %in% c)
data_2018 = Malta %>% filter(club_2018_name_Malta %in% c)

data_2020_sort <- with(data_2020,  data_2020[order(club_name_Malta) , ])
data_2018_sort <- with(data_2018,  data_2018[order(club_2018_name_Malta) , ])

paired_test  = t.test(as.numeric(data_2020_sort$V2),as.numeric(data_2018_sort$V2),
                      alternative = "less", var.equal = FALSE,paired = TRUE)

Malta_val = paired_test$p.value


c = intersect(club_2018_name_Netherlands,club_name_Netherlands)

Netherlands = data.frame(cbind(club_2018_name_Netherlands,as.numeric(club_2018_Netherlands)))
Netherlands_2020 = data.frame(cbind(club_name_Netherlands,as.numeric(club_Netherlands)))


data_2020 = Netherlands_2020 %>% filter(club_name_Netherlands %in% c)
data_2018 = Netherlands %>% filter(club_2018_name_Netherlands %in% c)

data_2020_sort <- with(data_2020,  data_2020[order(club_name_Netherlands) , ])
data_2018_sort <- with(data_2018,  data_2018[order(club_2018_name_Netherlands) , ])

paired_test  = t.test(as.numeric(data_2020_sort$V2),as.numeric(data_2018_sort$V2),
                      alternative = "less", var.equal = FALSE,paired = TRUE)

Netherlands_val = paired_test$p.value


c = intersect(club_2018_name_Poland,club_name_Poland)

Poland = data.frame(cbind(club_2018_name_Poland,as.numeric(club_2018_Poland)))
Poland_2020 = data.frame(cbind(club_name_Poland,as.numeric(club_Poland)))

data_2020 = Poland_2020 %>% filter(club_name_Poland %in% c)
data_2018 = Poland %>% filter(club_2018_name_Poland %in% c)

data_2020_sort <- with(data_2020,  data_2020[order(club_name_Poland) , ])
data_2018_sort <- with(data_2018,  data_2018[order(club_2018_name_Poland) , ])

paired_test  = t.test(as.numeric(data_2020_sort$V2),as.numeric(data_2018_sort$V2),
                      alternative = "less", var.equal = FALSE,paired = TRUE)

Poland_val = paired_test$p.value


c = intersect(club_2018_name_Portugal,club_name_Portugal)

Portugal = data.frame(cbind(club_2018_name_Portugal,as.numeric(club_2018_Portugal)))
Portugal_2020 = data.frame(cbind(club_name_Portugal,as.numeric(club_Portugal)))

data_2020 = Portugal_2020 %>% filter(club_name_Portugal %in% c)
data_2018 = Portugal %>% filter(club_2018_name_Portugal %in% c)

data_2020_sort <- with(data_2020,  data_2020[order(club_name_Portugal) , ])
data_2018_sort <- with(data_2018,  data_2018[order(club_2018_name_Portugal) , ])

paired_test  = t.test(as.numeric(data_2020_sort$V2),as.numeric(data_2018_sort$V2),
                      alternative = "less", var.equal = FALSE,paired = TRUE)

Portugal_val = paired_test$p.value


c = intersect(club_2018_name_Slovakia,club_name_Slovakia)

Slovakia = data.frame(cbind(club_2018_name_Slovakia,as.numeric(club_2018_Slovakia)))
Slovakia_2020 = data.frame(cbind(club_name_Slovakia,as.numeric(club_Slovakia)))


data_2020 = Slovakia_2020 %>% filter(club_name_Slovakia %in% c)
data_2018 = Slovakia %>% filter(club_2018_name_Slovakia %in% c)

data_2020_sort <- with(data_2020,  data_2020[order(club_name_Slovakia) , ])
data_2018_sort <- with(data_2018,  data_2018[order(club_2018_name_Slovakia) , ])

paired_test  = t.test(as.numeric(data_2020_sort$V2),as.numeric(data_2018_sort$V2),
                      alternative = "less", var.equal = FALSE,paired = TRUE)

Slovakia_val = paired_test$p.value


c = intersect(club_2018_name_Spain,club_name_Spain)

Spain = data.frame(cbind(club_2018_name_Spain,as.numeric(club_2018_Spain)))
Spain_2020 = data.frame(cbind(club_name_Spain,as.numeric(club_Spain)))

data_2020 = Spain_2020 %>% filter(club_name_Spain %in% c)
data_2018 = Spain %>% filter(club_2018_name_Spain %in% c)

data_2020_sort <- with(data_2020,  data_2020[order(club_name_Spain) , ])
data_2018_sort <- with(data_2018,  data_2018[order(club_2018_name_Spain) , ])

paired_test  = t.test(as.numeric(data_2020_sort$V2),as.numeric(data_2018_sort$V2),
                      alternative = "less", var.equal = FALSE,paired = TRUE)

Spain_val = paired_test$p.value


c = intersect(club_2018_name_Sweden,club_name_Sweden)

Sweden = data.frame(cbind(club_2018_name_Sweden,as.numeric(club_2018_Sweden)))
Sweden_2020 = data.frame(cbind(club_name_Sweden,as.numeric(club_Sweden)))

data_2020 = Sweden_2020 %>% filter(club_name_Sweden %in% c)
data_2018 = Sweden %>% filter(club_2018_name_Sweden %in% c)

data_2020_sort <- with(data_2020,  data_2020[order(club_name_Sweden) , ])
data_2018_sort <- with(data_2018,  data_2018[order(club_2018_name_Sweden) , ])

paired_test  = t.test(as.numeric(data_2020_sort$V2),as.numeric(data_2018_sort$V2),
                      alternative = "less", var.equal = FALSE,paired = TRUE)

Sweden_val = paired_test$p.value



c = intersect(club_2018_name_England,club_name_England)

England = data.frame(cbind(club_2018_name_England,as.numeric(club_2018_England)))
England_2020 = data.frame(cbind(club_name_England,as.numeric(club_England)))

data_2020 = England_2020 %>% filter(club_name_England %in% c)
data_2018 = England %>% filter(club_2018_name_England %in% c)

data_2020_sort <- with(data_2020,  data_2020[order(club_name_England) , ])
data_2018_sort <- with(data_2018,  data_2018[order(club_2018_name_England) , ])

paired_test  = t.test(as.numeric(data_2020_sort$V2),as.numeric(data_2018_sort$V2),
                      alternative = "less", var.equal = FALSE,paired = TRUE)

England_val = paired_test$p.value

--------------------------------------------------------------------------------
pvalue_list = c(England_val,Sweden_val,Spain_val,Slovakia_val,Portugal_val,Poland_val,
                Netherlands_val,Malta_val, Italy_val,Ireland_val,Hungary_val,
                Greece_val,Germany_val,France_val,Finland_val,Estonia_val,Denmark_val,Cyprus_val,
                Croatia_val,Bulgaria_val,austria_val)

land = c("England", "Schweden", "Spanien", "Slowakei", "Portugal" ,"Polen" ,"Niederlande", "Malta",
         "Italien", "Irland" ,"Ungarn", "Griechenland", "Deutschland", "Frankreich" ,"Finnland" ,"Estland",
         "Dänemark","Zypern", "Kroatien" ,"Bulgarien" ,"Österreich")

pval = data.frame(cbind(land,round(pvalue_list,3)))

pval = pval %>% mutate(pValue = as.numeric(V2), Land = land)
pval = pval %>% select(Land,pValue)

library(gridExtra)
grid.newpage()
grid.table(pval)

pval1 = pval %>% filter(as.numeric(pval$pValue) < 0.05)
tab = ggtexttable(pval, rows = NULL, theme = ttheme("mBlue")) 

tab = table_cell_bg(tab, row = 2, column = 2, linewidth = 5,fill="darkblue", color = "darkolivegreen4") %>%
  table_cell_font(row = 2, column = 2, face = "italic", color = "white")

tab

tab = table_cell_bg(tab, row = 6, column = 2, linewidth = 5,fill="darkblue", color = "darkolivegreen4") %>%
  table_cell_font(row = 6, column = 2, face = "italic", color = "white")
tab

tab = table_cell_bg(tab, row = 8, column = 2, linewidth = 5,fill="darkblue", color = "darkolivegreen4") %>%
  table_cell_font(row = 8, column = 2, face = "italic", color = "white")
tab

tab = table_cell_bg(tab, row = 10, column = 2, linewidth = 5,fill="darkblue", color = "darkolivegreen4") %>%
  table_cell_font(row = 10, column = 2, face = "italic", color = "white")
tab

tab = table_cell_bg(tab, row = 13, column = 2, linewidth = 5,fill="darkblue", color = "darkolivegreen4") %>%
  table_cell_font(row = 13, column = 2, face = "italic", color = "white")
tab

tab = table_cell_bg(tab, row = 15, column = 2, linewidth = 5,fill="darkblue", color = "darkolivegreen4") %>%
  table_cell_font(row = 15, column = 2, face = "italic", color = "white")
tab


#----------------------------------------------------------------------------------
# Plot
ggplot(pval, aes(x=land, y= as.numeric(pValue)))  + ylab('p values')+xlab('Land')+
  geom_point( color="orange", size=4) + 
  geom_segment(aes(x=land, 
                   xend=land, 
                   y=0, 
                   yend=as.numeric(pValue)),color="grey") + geom_hline(yintercept = 0.05,linetype="dashed", color = "red")+
  labs(title="P Values", 
       caption="source: mpg") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) 


