library(googlesheets4)
googlesheets4::gs4_deauth()
#googlesheets4::gs4_auth()

#download table
x <- read_sheet('1vTqeTSV1K4SfeH7KWtSDPHKJMhNsRcB3W3rYNRdxQKc', sheet = 1)
saveRDS(x,"www/table.RDS")

#download key
x <- read_sheet('1vTqeTSV1K4SfeH7KWtSDPHKJMhNsRcB3W3rYNRdxQKc', sheet = 2)
saveRDS(x,"www/key.RDS")
