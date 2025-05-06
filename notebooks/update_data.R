library(DBI)
library(RMySQL)
library(readr)

# Connect to AWS RDS
con <- dbConnect(RMySQL::MySQL(),
                host = "stroke-db.cpcsqk8849iz.ap-southeast-1.rds.amazonaws.com",
                port = 3306,
                user = "admin",
                password = "Qwe12345#",
                dbname = "stroke_project")

# Read CSV
stroke_df <- read_csv("stroke_data.csv")

# Upload to MySQL (RDS)
dbWriteTable(con, name = "stroke_data", value = stroke_df, row.names = FALSE)

# Check
dbGetQuery(con, "SELECT COUNT(*) FROM stroke_data")

