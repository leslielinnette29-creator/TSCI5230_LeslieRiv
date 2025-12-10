#' title: "TSCI 5230 "

#' Author: Leslie L Rivera Lopez 

##Copy over the init section
debug <- 0;seed <-22;#See is to generate a random number but in a different way. You will have a random number and reproducibility.

knitr::opts_chunk$set(echo=debug>-1, warning=debug>0, message=debug>0, class.output="scroll-20", attr.output='style="max-height: 150px; overflow-y: auto;"');

library(ggplot2); # visualization
library(DataExplorer) #? is use to help file available
library(GGally);
library(rio);# simple command for importing and exporting
library(pander); # format tables
#library(printr); # set limit on number of lines printed
library(broom); # allows to give clean dataset
library(dplyr);#add dplyr library
library(tidymodels);
library(ggfortify)
library(dm)
library(nycflights13)
library(DiagrammeR)
flights_dm_no_keys <- dm(airlines, airports, flights, planes, weather)
flights_dm_no_keys;
dm_enum_pk_candidates(
  dm = flights_dm_no_keys,
  table = planes
)

sapply(names(flights_dm_no_keys),function(xx){
  dm_enum_pk_candidates(
    dm = flights_dm_no_keys,
    table = !!xx
  )%>%subset(candidate) %>%select(columns) %>%mutate(table=xx)},simplify = F) %>% bind_rows()

dm_add_pk(flights_dm_no_keys,planes,tailnum) %>%  dm_add_pk(airports,faa) %>%  dm_add_pk(airlines,carrier)
flights_dm_only_pks <- dm_add_pk(flights_dm_no_keys,planes,tailnum) %>% dm_add_pk(airports,faa) %>% dm_add_pk(airlines,carrier) %>% dm_add_pk(weather,columns=c(origin,time_hour)) 
flights_dm_only_pks

flights_dm_all_keys <-
  flights_dm_only_pks %>%
  dm_add_fk(table = flights, columns = tailnum, ref_table = planes) %>%
  dm_add_fk(flights, carrier, airlines) %>%
  dm_add_fk(flights, origin, airports) %>% 
  dm_add_fk(flights,c(origin,time_hour),weather) %>% 
  #dm_add_fk(weather,origin,airports)
flights_dm_all_keys

flights_dm_all_keys %>%
  dm_draw()

dm_flatten_to_tbl(flights_dm_all_keys,flights) %>% view()
dm_enum_fk_candidates(flights_dm_all_keys,weather,airports)

#Plot
set.seed(42)

#ggplot used for plotting
dm_flatten_to_tbl(flights_dm_all_keys,flights) %>% 
  select(lon,dep_time) %>%
#  na.omit() %>% 
  ggplot(aes(x=time_hour,y=long))+  # + if yo ading layers to a plot
  geom_point(position = "jitter",alpha=.1)
