#########################################################################
## Read in enrollment data for january of each year
#########################################################################
if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}
library(readr)

if (!requireNamespace("magrittr", quietly = TRUE)) {
  install.packages("magrittr")
}

# Load the magrittr package
library(magrittr)


if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

# Load the dplyr package
library(dplyr)

if (!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
}

# Load the tidyr package
library(tidyr)

current_directory <- getwd()
print(current_directory)

for (y in 2007:2015) {
  ## Basic contract/plan information
  ma.path=paste0("C:/Users/huang/desktop/Econ470/monthly-ma-and-pdp-enrollment-by-cpsc/CPSC_Contract_Info_",y,"_01.csv")
  contract.info=read_csv(ma.path,
                         skip=1,
                         col_names = c("contractid","planid","org_type","plan_type",
                                       "partd","snp","eghp","org_name","org_marketing_name",
                                       "plan_name","parent_org","contract_date"),
                         col_types = cols(
                           contractid = col_character(),
                           planid = col_double(),
                           org_type = col_character(),
                           plan_type = col_character(),
                           partd = col_character(),
                           snp = col_character(),
                           eghp = col_character(),
                           org_name = col_character(),
                           org_marketing_name = col_character(),
                           plan_name = col_character(),
                           parent_org = col_character(),
                           contract_date = col_character()
                         ))
  
  contract.info = contract.info %>%
    group_by(contractid, planid) %>%
    mutate(id_count=row_number())
  
  contract.info = contract.info %>%
    filter(id_count==1) %>%
    select(-id_count)
  
  ## Enrollments per plan
  ma.path=paste0("C:/Users/huang/desktop/Econ470/monthly-ma-and-pdp-enrollment-by-cpsc/CPSC_Enrollment_Info_",y,"_01.csv")
  enroll.info=read_csv(ma.path,
                       skip=1,
                       col_names = c("contractid","planid","ssa","fips","state","county","enrollment"),
                       col_types = cols(
                         contractid = col_character(),
                         planid = col_double(),
                         ssa = col_double(),
                         fips = col_double(),
                         state = col_character(),
                         county = col_character(),
                         enrollment = col_double()
                       ),na="*")
  
  
  ## Merge contract info with enrollment info
  plan.data = contract.info %>%
    left_join(enroll.info, by=c("contractid", "planid")) %>%
    mutate(year=y)
  
  ## Fill in missing fips codes (by state and county)
  plan.data = plan.data %>%
    group_by(state, county) %>%
    fill(fips)
  
  ## Fill in missing plan characteristics by contract and plan id
  plan.data = plan.data %>%
    group_by(contractid, planid) %>%
    fill(plan_type, partd, snp, eghp, plan_name)
  
  ## Fill in missing contract characteristics by contractid
  plan.data = plan.data %>%
    group_by(contractid) %>%
    fill(org_type,org_name,org_marketing_name,parent_org)
  
  ## Collapse from monthly data to yearly
  plan.year = plan.data %>%
    group_by(contractid, planid, fips) %>%
    arrange(contractid, planid, fips) %>%
    rename(avg_enrollment=enrollment)
  
  write.csv(plan.year,file=paste0("C:/Users/huang/desktop/Econ470/hw1_data/ma_data_",y,".csv"), row.names = FALSE)
}

full.ma.data <- read_csv("C:/Users/huang/desktop/Econ470/hw1_data/ma_data_2007.csv")
for (y in 2008:2015) {
  full.ma.data <- rbind(full.ma.data,read_csv(paste0("C:/Users/huang/desktop/Econ470/hw1_data/ma_data_",y,".csv")))
}

write.csv(full.ma.data,file="C:/Users/huang/desktop/Econ470/hw1_data/full_ma_data.csv")
sapply(paste0("hw1_data/ma_data_", 2007:2015, ".csv"), unlink)

nrow(full.ma.data)
