library(tidyverse)
library(rio)
library(ggplot2)
 #file location in pc
Maestrofile<-"C:\\Users\\Linne\\Documents\\output\\LabNotebook_00975151_cc3c267c-7dec-4da5-a4ce-aed5e7a11961mito72hrs(AutoRecovered).xlsx"


# 1) Load your CSV ----
startrow<-import(Maestrofile,which = 'Summary Series')[,2] %>% grep('^A1$',.)
df0<-import(Maestrofile, which = 'Summary Series',skip=startrow+1) %>% rename( Hours='...1')
df1<-pivot_longer(df0,
    cols = -Hours,               # all columns except Hours
    names_to = "Well",
    values_to = "Resistance"
  ) %>%
  mutate(
    Condition = gsub("\\.{3}\\d+$","",Well)
  ) %>% 
#take out empty rows (n/a)
na.omit() 

df_baseline<-subset(df1,Hours==23)
df_baseline<-group_by(df_baseline, Condition) %>% summarise(baseline=median(Resistance)) %>% 
  right_join(df_baseline,by = c(Condition='Condition')) %>% rename(Well_baseline="Resistance") %>% select(-Hours)
df2<-left_join(df1,df_baseline)%>% mutate(normalize=Resistance-baseline,Well_normalize=Resistance-Well_baseline) 

#plot results

ggplot(df2,aes(x=Hours,y=normalize,color=Condition))+geom_line()

df3<-group_by(df2,Condition,Hours) %>% summarise(Resistance=mean(Resistance),normalize=mean(normalize),Well_normalize=mean(Well_normalize))

ggplot(df3,aes(x=Hours,y=normalize,color=Condition))+geom_line()

ggplot(df3,aes(x=Hours,y=Well_normalize,color=Condition))+geom_line()

ggplot(df3,aes(x=normalize,y=Well_normalize,color=Condition))+geom_point()

export(list(Bywell=df2,bycondition=df3),'Maestroprocessresults.xlsx')
