# Read metedata of the Sonde dataset  (excel file)
# Loading
  library("readxl")
  library("tidyverse")
  library("hms")
  
  
# xls files
  file_path <- "C:/Users/Zhong.Zhang/Documents/Monitoring Analysis/Work/2022/Sonde/Data"
  metadata_Sonde_Wye <- read_excel(paste0(file_path,"/Metadata_Sonde_Wye_23-08-2022.xlsx"))
 
  Redbrook <- read.csv(paste0(file_path,"/Wye_RedBrook.csv"),skip = 8, skipNul = TRUE, stringsAsFactors = FALSE)
  Glasbury <- read.csv(paste0(file_path,"/Wye_Glasbury.csv"),skip = 8, skipNul = TRUE, stringsAsFactors = FALSE)
  Ddol_Farm <- read.csv(paste0(file_path,"/Wye_Ddol_Farm.csv"),skip = 8, skipNul = TRUE, stringsAsFactors = FALSE)
  Irfon <- read.csv(paste0(file_path,"/Wye_Irfon_at_Cilmery.csv"),skip = 8, skipNul = TRUE, stringsAsFactors = FALSE)
  Ithon <- read.csv(paste0(file_path,"/Wye_Ithon_at_Disserth.csv"),skip = 8, skipNul = TRUE, stringsAsFactors = FALSE)
  Llynfi <- read.csv(paste0(file_path,"/Wye_Llynfi_at_Three_Cocks.csv"),skip = 8, skipNul = TRUE, stringsAsFactors = FALSE)

# Convert columns of characters to numeric for Irfon
  # Irfon$Time..Fract..Sec. <- as.numeric(Irfon$Time..Fract..Sec.)
  # Irfon[,5:7] <- as.numeric(Irfon[,5:7])
# add df_Name as column in metadate dataframe
  
  # metadata_Sonde$df_Name <- c("camrose_knock","pelcomb")

# add Id and sample name column in the dataframe
  
  Redbrook <- mutate(Redbrook, ID = 55023, Name = "Redbrook",.before = "Date..MM.DD.YYYY.")
  Glasbury <- mutate(Glasbury, ID = 55025, Name = "Glasbury", .before ="Date..MM.DD.YYYY.")
  Ddol_Farm <- mutate(Ddol_Farm, ID = 55026, Name = "Ddol Farm at Rhyader",.before = "Date..MM.DD.YYYY.")
  Irfon <- mutate(Irfon, ID = 55012, Name = "Irfon at Cilmery", .before ="Date..MM.DD.YYYY.")
  Ithon <- mutate(Ithon, ID = 55016, Name = "Ithon at Disserth",.before = "Date..MM.DD.YYYY.")
  Llynfi <- mutate(Llynfi, ID = 50098, Name = "Llynfi at Three Cocks", .before ="Date..MM.DD.YYYY.")

# rename column "Time (Fract. Sec)" as Time for camrose_knock
 
# join the sample data from the two sites. 
  
  join_redbrook_glasbury <- bind_rows(Redbrook,Glasbury,Ddol_Farm,Irfon,Ithon,Llynfi) %>% 
                              mutate(Date_Time=paste0(Date..MM.DD.YYYY.," ",Time..HH.mm.ss.), .before = "Time..Fract..Sec.")

  join_redbrook_glasbury$Date<- as.Date(join_redbrook_glasbury$Date..MM.DD.YYYY.,
                                         format = "%d/%m/%Y")
 
   join_redbrook_glasbury$Datetime <- strptime(join_redbrook_glasbury$Date_Time,
                                               format = "%d/%m/%Y %H:%M:%S")

  join_redbrook_glasbury1 <- join_redbrook_glasbury[,c(1:2,31,32,7:30)]
  
    # remove some unimportant varibles
  join_redbrook_glasbury <- join_redbrook_glasbury1[,c(-5,-6,-8,-9,-12,-15,-19,
                                                       -20,-22,-24:-26,-28)]
  
    # rename column names
  join_redbrook_glasbury <- join_redbrook_glasbury %>% 
                  rename("Chlorophyll (ug/l)" = "Chlorophyll.ug.L",
                         "Dissolved Oxygen saturation (%)" = "ODO...sat",
                         "Dissolved Oxygen (mg/l)" = "ODO.mg.L",
                         "Salinity (psu)" = "Sal.psu",
                         "Specific Conductivity (µS/cm)" = "SpCond.µS.cm",
                         "Total Algae-Phycocyanin (ug/l)" = "TAL.PC.ug.L",
                         "Total Dissolved Solids (mg/l)" =  "TDS.mg.L",
                         "Turbidity (FNU)" = "Turbidity.FNU",
                         "Nitrate (mg/l)" = "NitraLED.mg.L",
                         "Temperature (°C)" = "Temp..C")
  
  # names(join_redbrook_glasbury)[names(join_redbrook_glasbury) == "Temp..C"] <- 
  #            paste0("Temperature (°C")
  
    ## delete some of the object
  rm(Redbrook, Glasbury, Ddol_Farm, Irfon,Ithon, Llynfi, join_redbrook_glasbury1)
 
  
  
  