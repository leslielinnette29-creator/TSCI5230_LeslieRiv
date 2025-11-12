#This is a method of randomization

library(dplyr);#add dplyr library
library(rio);
mouse_rand_file_loc<-."./output/Example Mouse Randomization.xlsx"

mouse_rand_data <-import(mouse_rand_import_file_loc,skip=1)
view(mouse_rand_data)

arrange_mouse_rand_data,desc(`photons/sec`))
View(arranged_mouse_rand_data)
mouse_groups <-c("treatment1","treatment2","control","combination")
arranged_mouse_rand_data (["mouse_groups"])<-mouse_groups
group_by(arranged_mouse_rand_data,mouse_groups) %% summarize (average_`photons/sec`= mean (`photons/sec`))



unarrange_mouse_rand_data <- mouse_rand_data
View(unarranged_mouse_rand_data)
arranged_mouse_rand_data (["mouse_groups"])<-sample [mouse_groups,n=nrow(unarrange_mouse_rand_data),replace=TRUE))
group_by(arranged_mouse_rand_data,mouse_groups) %% summarize (average_`photons/sec`= mean (`photons/sec`))

