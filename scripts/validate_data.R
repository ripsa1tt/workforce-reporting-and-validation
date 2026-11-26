#Date: Sunday, November 22nd, 2026 
#Author: Subramaniam S. 
#----------------------------------

#Importing datasets

hiring_plan <- read.csv("data/hiring_plan.csv")

recruitment <- read.csv("data/recruitment_activity.csv")

#Inspecting datasets

head(hiring_plan)
head(recruitment)

str(hiring_plan)
str(recruitment)

#Converting date columns

hiring_plan$target_hire_date <- as.Date(
  hiring_plan$target_hire_date
)

recruitment$recruitment_start_date <- as.Date(
  recruitment$recruitment_start_date
)

recruitment$fill_date <- as.Date(
  recruitment$fill_date
)

recruitment$last_updated <- as.Date(
  recruitment$last_updated
)

#Setting reporting date

report_date <- as.Date("2026-11-23")

#---------------------------------------

#Check 1: Duplicate recruitment records

duplicate_records <- recruitment[
  duplicated(recruitment$position_id),
]

print(duplicate_records)

#Check 2: Missing recruitment records

missing_records <- hiring_plan[
  hiring_plan$plan_status == "Approved" &
    !(hiring_plan$position_id %in% recruitment$position_id),
]

print(missing_records)


#Check 3: Missing required fill dates

missing_fill_dates <- recruitment[
  recruitment$recruitment_status == "Filled" &
    is.na(recruitment$fill_date),
]

print(missing_fill_dates)

#Check 4: Invalid recruitment statuses

valid_statuses <- c(
  "Not Started",
  "Recruiting",
  "Interviewing",
  "Offer",
  "Filled",
  "Cancelled",
  "On Hold"
)

invalid_statuses <- recruitment[
  !(recruitment$recruitment_status %in% valid_statuses),
]

print(invalid_statuses)

#Check 5: Outdated recruitment records

stale_records <- recruitment[
  recruitment$recruitment_status %in%
    c("Recruiting", "Interviewing", "Offer") &
    as.numeric(report_date - recruitment$last_updated) > 14,
]

print(stale_records)

#Combining both datasets using position_id

combined_data <- merge(
  hiring_plan,
  recruitment,
  by = "position_id",
  all.x = TRUE
)

head(combined_data)

#Check 6: Conflicting statuses

status_conflicts <- combined_data[
  combined_data$plan_status == "Cancelled" &
    combined_data$recruitment_status %in%
    c("Recruiting", "Interviewing", "Offer"),
]

print(status_conflicts)

nrow(duplicate_records)
nrow(missing_records)
nrow(missing_fill_dates)
nrow(invalid_statuses)
nrow(stale_records)
nrow(status_conflicts)


#-------------------------------------------
#Generating an exception report
#-------------------------------------------
create_exception <- function(data, issue, severity, action) {
  
  data.frame(
    position_id = data$position_id,
    issue = rep(issue, nrow(data)),
    severity = rep(severity, nrow(data)),
    recommended_action = rep(action, nrow(data))
  )
  
}

#Consolidating validation results

exception_report <- rbind(
  
  create_exception(
    duplicate_records,
    "Duplicate recruitment record",
    "High",
    "Review and resolve duplicate records"
  ),
  
  create_exception(
    missing_records,
    "Missing recruitment record",
    "High",
    "Verify position in recruitment system"
  ),
  
  create_exception(
    missing_fill_dates,
    "Missing fill date",
    "High",
    "Confirm and record fill date"
  ),
  
  create_exception(
    invalid_statuses,
    "Invalid recruitment status",
    "Medium",
    "Review and correct status"
  ),
  
  create_exception(
    stale_records,
    "Outdated recruitment record",
    "Medium",
    "Request recruitment status update"
  ),
  
  create_exception(
    status_conflicts,
    "Conflicting recruitment status",
    "High",
    "Reconcile hiring and recruitment statuses"
  )
)

#Adding department information

exception_report$department <- hiring_plan$department[
  match(exception_report$position_id, hiring_plan$position_id)
]

#Reordering report columns

exception_report <- exception_report[
  c("position_id", "department", "issue",
    "severity", "recommended_action")
]

#Sorting by severity and position ID

exception_report <- exception_report[
  order(exception_report$severity,
        exception_report$position_id),
]

View(exception_report)

#----------------------------------------
#Creating output folder

dir.create("outputs", showWarnings = FALSE)

#Exporting exception report

write.csv(
  exception_report,
  "outputs/exception_report.csv",
  row.names = FALSE
)

#----------------------------------------
#Preparing workforce reporting data
#---------------------------------------

#Removing identical duplicate records

recruitment_unique <- unique(recruitment)


#Combining hiring and recruitment data

workforce_reporting <- merge(
  hiring_plan,
  recruitment_unique,
  by = "position_id",
  all.x = TRUE
)

#Flagging positions with outstanding issues

workforce_reporting$data_quality <- ifelse(
  workforce_reporting$position_id %in%
    exception_report$position_id,
  "Review Required",
  "Passed Checks"
)

#Calculating time-to-fill in days

workforce_reporting$time_to_fill <- as.numeric(
  workforce_reporting$fill_date -
    workforce_reporting$recruitment_start_date
)

#Exporting workforce reporting data

write.csv(
  workforce_reporting,
  "outputs/workforce_reporting.csv",
  row.names = FALSE
)

View(workforce_reporting)

dim(workforce_reporting)

table(workforce_reporting$data_quality)
