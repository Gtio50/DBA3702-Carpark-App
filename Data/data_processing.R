# Library -----------------------------------------------------------------
library(vroom)

# Reading Data ------------------------------------------------------------
df <- read.csv("/Users/Xinyu/Desktop/Study Materials/Year 4/Sem 1/DBA3702/Project/Data Files/carpark_data_resultsfivepercent.csv",
               nrows = 10)
carpark <- vroom("carpark_data_resultsfivepercent.csv", col_types = "_cTncn")

