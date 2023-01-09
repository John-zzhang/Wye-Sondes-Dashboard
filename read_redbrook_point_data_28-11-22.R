# Read metedata of the Sonde dataset  (excel file)
# Loading
  library("readxl")
  library("tidyverse")
  library("hms")
  
  
# xls files
  file_path <- "C:/Users/Zhong.Zhang/Documents/Monitoring Analysis/Work/2022/Sonde/Data"

  Redbrook_point <- read.csv(paste0(file_path,"/Redbrook_points.csv"), stringsAsFactors = FALSE)
 

  Redbrook_point <- Redbrook_point %>% 
    mutate(Date_Time=paste0(Date," ",Time), .before = "pH")
  
  Redbrook_point$Date<- as.Date(Redbrook_point$Date,
                                         format = "%d/%m/%Y")
 
  Redbrook_point$Date_Time <- strptime(Redbrook_point$Date_Time,
                                               format = "%d/%m/%Y %H:%M:%S")

  
  
 
  
  
  