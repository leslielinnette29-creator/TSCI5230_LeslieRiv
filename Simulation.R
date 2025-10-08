#' title: "TSCI 5230 Processing a Data Set"

#' Author:  Leslie Rivera Lopez

##Copy over the init section

debug <- 0;seed <-22;#See is to generate a random number but in a different way. You will have a random number and reproducibility.

knitr::opts_chunk$set(echo=debug>-1, warning=debug>0, message=debug>0, class.output="scroll-20", attr.output='style="max-height: 150px; overflow-y: auto;"');


library(rio);# simple command for importing and exporting

library(pander); # format tables

#library(printr); # set limit on number of lines printed

library(broom); # allows to give clean dataset

library(dplyr);#add dplyr library

library(tidymodels);

library(ggfortify);

#init----

options(max.print=500);

panderOptions('table.split.table',Inf); panderOptions('table.split.cells',Inf)

datasource <- "../output/csv/"

samplesize<-100; state<- "Texas"; city<- "";gender<-"F"; minage<-0;maxage<-120
if(state!="") {
  state <- paste0('"', state, '"')
  
  if (city != "") {
    city <- paste0('"', city, '"')
  }
} else{city<-""}

if(gender %in% c("M","F")){gender<-paste0("-g ",gender)} else{gender<-""}




runsynthea <- function(samplesize = 100,
                       state = "",
                       city = "",
                       gender = "",
                       minage = 0,
                       maxage = 120,
                       syntheaopotions = "--exporter.fhir.export=false --exporter.csv.export=true --exporter.csv.append_mode=true")
{
  if (state != "") {
    state <- paste0('"', state, '"')
    
    if (city != "") {
      city <- paste0('"', city, '"')
    }
  } else{
    city <- ""
  }
  
  if (gender %in% c("M", "F")) {
    gender <- paste0("-g ", gender)
  } else{
    gender <- ""
  }
  syntheatemplate <- "java -jar synthea-with-dependencies.jar -p %d %s %s  %s -a %d-%d %s"
  syntheacommand <- sprintf(syntheatemplate,
                            samplesize,
                            state,
                            city,
                            gender,
                            minage,
                            maxage,
                            syntheaopotions)
  browser()
}
runsynthea(gender="M",state='Texas')

