#Date: Sunday, November 22nd, 2026 
#Author: Subramaniam S. 
#----------------------------------

#Setting up randomized data files

#Generating the hiring plan document
set.seed(2026)
n <- 60 
hiring_plan <- data.frame(
  position_id = sprintf("P%03d", 1:n), 
  department = sample(
    c("Finance", "Marketing", "Digital",
      "Operations", "People & Culture"), 
    n, replace = TRUE 
  ), 
  plan_status = sample(
    c("Approved", "On Hold", "Cancelled"), 
    n, replace = TRUE, 
    prob = c(0.85, 0.10, 0.05)
  )
)

#Setting a fixed reporting date 
report_date <- as.Date("2026-11-23")

#Generating target hiring dates
hiring_plan$target_hire_date <- report_date +
  sample(-30:60, n, replace = TRUE)

#Preview
head(hiring_plan) 

#----------------------------------------
#Generating recruitment activity data 
#---------------------------------------- 

recruitment <- data.frame(
  position_id = hiring_plan$position_id, 
  recruitment_status = sample(
    c("Not Started", "Recruiting", "Interviewing", "Offer", "Filled"), 
    n, replace = TRUE, 
    prob = c(0.10, 0.30, 0.25, 0.15, 0.20) 
  )
) 

#Aligning recruitment statuses with the hiring plan 

recruitment$recruitment_status[ 
  hiring_plan$plan_status == "Cancelled"
  ] <- "Cancelled"

recruitment$recruitment_status[
  hiring_plan$plan_status =="On Hold"
] <- "On Hold"

#Generating recruitment start dates 

recruitment$recruitment_start_date <- report_date - 
  sample(30:90, n, replace = TRUE)

#Removing dates for inactive positions 

recruitment$recruitment_start_date[
  recruitment$recruitment_status %in% 
    c("Not Started", "Cancelled", "On Hold")
] <- NA 

#Creating an empty fill date column 

recruitment$fill_date <- as.Date(NA)

#Identifying filled positions 
filled <- which(
  recruitment$recruitment_status == "Filled"
)

#Generating fill dates 

recruitment$fill_date[filled] <-
  recruitment$recruitment_start_date[filled] +
  sample(14:29, length(filled), replace = TRUE)

#Generating last updated dates

recruitment$last_updated <- report_date -
  sample(0:7, n, replace = TRUE)

head(recruitment)

#-------------------------------------------
#Introducing intentional data quality isses
#-------------------------------------------

#Error 1: Missing recruitment record

hiring_plan$plan_status[
  hiring_plan$position_id == "P008"
] <- "Approved"

recruitment <- recruitment[
  recruitment$position_id != "P008",
]

#Error 2: Missing required fill date
hiring_plan$plan_status[
  hiring_plan$position_id == "P014"
] <- "Approved"

recruitment$recruitment_status[
  recruitment$position_id == "P014"
] <- "Filled"

recruitment$recruitment_start_date[
  recruitment$position_id == "P014"
] <- report_date - 45

recruitment$fill_date[
  recruitment$position_id == "P014"
] <- NA

#Error 3: Inconssitent statuses across datasets 

hiring_plan$plan_status[
  hiring_plan$position_id == "P025"
] <- "Cancelled"

recruitment$recruitment_status[
  recruitment$position_id == "P025"
] <- "Recruiting"

recruitment$recruitment_start_date[
  recruitment$position_id == "P025"
] <- report_date - 40

recruitment$fill_date[
  recruitment$position_id == "P025"
] <- NA

#Error 4: Duplicate recruitment record 

duplicate_row <- recruitment[
  recruitment$position_id == "P034",
]

recruitment <- rbind(recruitment, duplicate_row)

#Error 5: Invalid recruitment status 

hiring_plan$plan_status[
  hiring_plan$position_id == "P041"
] <- "Approved"

recruitment$recruitment_status[
  recruitment$position_id == "P041"
] <- "In Progess"

recruitment$recruitment_start_date[
  recruitment$position_id == "P041"
] <- report_date - 40

recruitment$fill_date[
  recruitment$position_id == "P041"
] <- NA

#Error 6: Outdated recruitment record

hiring_plan$plan_status[
  hiring_plan$position_id == "P052"
] <- "Approved"

recruitment$recruitment_status[
  recruitment$position_id == "P052"
] <- "Recruiting"

recruitment$recruitment_start_date[
  recruitment$position_id == "P052"
] <- report_date - 60

recruitment$fill_date[
  recruitment$position_id == "P052"
] <- NA

recruitment$last_updated[
  recruitment$position_id == "P052"
] <- report_date - 2

#-------------------------------------------
#Saving the files 
#-------------------------------------------

dir.create("data", showWarnings = FALSE)

write.csv(
  hiring_plan,
  "data/hiring_plan.csv",
  row.names = FALSE
)

write.csv(
  recruitment,
  "data/recruitment_activity.csv",
  row.names = FALSE
)

dim(hiring_plan)
dim(recruitment)
