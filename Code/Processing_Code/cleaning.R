## ---- loaddata --------
data_location <- "../../Data/Raw_data/Raw_Oreophryne_Character_Measurements.csv"
data_path <- "../../Data/Raw_data/"

rawdata <- read.csv(data_location, check.names=FALSE)

## ---- convertpixels --------
#Making a new dataframe for converted measurements (mm not pixels) for each specimen
