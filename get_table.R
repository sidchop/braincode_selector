library(googlesheets4)
#download table
x <- read_sheet('1vTqeTSV1K4SfeH7KWtSDPHKJMhNsRcB3W3rYNRdxQKc', sheet = 1)
saveRDS(x,"www/table.RDS")

#download key
x <- read_sheet('1vTqeTSV1K4SfeH7KWtSDPHKJMhNsRcB3W3rYNRdxQKc', sheet = 2)
saveRDS(x,"www/key.RDS")
