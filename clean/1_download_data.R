library(rio)
avg_data <- rio::import("https://www.bls.gov/lau/laucnty20.xlsx")
monthly_data <- rio::import("https://www.bls.gov/web/metro/laucntycur14.zip")
