all_data_eng <- read.csv("/Users/viktoriazajceva/Desktop/R/20 Проект/FINAL/shiny indicators/All_data_eng.csv", fileEncoding = "UTF-8")

all_data_eng <- all_data_eng[,-1]

indicators <- colnames(all_data_eng)[5:16]
indicators <- indicators[-c(7,10)]  # For options

regions <- c("Russia", unique(all_data_eng$Region))  # For options

# Titles for indicators
indicatorTitles <- c(transactions = "Online Transactions",
                     activity = "Economic Activity",
                     tourism = "Tourism (difference from the last year)",
                     average.wage = "Average Wage",
                     unemployment.rate = "Unemployment Rate",
                     internet.users = "Internet Users",
                     online.loan.applications.share = "Online loan applications",
                     offline.loan.applications.share = "Offline loan applications",
                     new.build.share = "Mortgage for new build",
                     already.owned.share = "Mortgage for already owned")
