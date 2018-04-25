# ===================================================
# Script to get list of subscribers
# ===================================================


library(xlsx)

subs <- read.xlsx(file = "H:/TECH/Lisa/R/'Subscriber List 6-15-17'",
                  sheetname = "Sortable CURRENT",
                  startRow = 2,
                  colIndex = c(1, 3))

# getting "Java errors" with rJava package - try reinstalling java next time
