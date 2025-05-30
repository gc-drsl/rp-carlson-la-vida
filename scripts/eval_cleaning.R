library(tidyverse)

fa21 <- read_csv("data/imported_data/SurveyData_GC_2021_2021-FA_Standard_Numerics_20250530104212_H1110V3Q6.csv")
sp21 <- read_csv("data/imported_data/SurveyData_GC_2022_2021-SP_Standard_Numerics_20250530104337_H1110V3Q6.csv")
su21 <- read_csv("data/imported_data/SurveyData_GC_2022_2021-SU_Standard_Numerics_20250530104447_H1110V3Q6.csv")
fa22 <- read_csv("data/imported_data/SurveyData_GC_2022_2022-FA_Standard_Numerics_20250530104457_H1110V3Q6.csv")
sp22 <- read_csv("data/imported_data/SurveyData_GC_2022_2022-FA_Standard_Numerics_20250530104457_H1110V3Q6.csv")
su22 <- read_csv("data/imported_data/SurveyData_GC_2023_2022-SU_Standard_Numerics_20250530104754_H1110V3Q6.csv")
fa23 <- read_csv("data/imported_data/SurveyData_GC_2023_2023-FA_Standard_Numerics_20250530104802_H1110V3Q6.csv")
sp23 <- read_csv("data/imported_data/SurveyData_GC_2024_2023-SP_Standard_Numerics_20250530104943_H1110V3Q6.csv")
su23 <- read_csv("data/imported_data/SurveyData_GC_2024_2023-SU_Standard_Numerics_20250530105109_H1110V3Q6.csv")
raw_evals <- rbind(fa21, sp21, su21, fa22, sp22, su22, fa23, sp23, su23)

# Purpose: compare la vida, discovery, and common core course evaluations from 
# the last three years.

# Filter for common core classes, la vida, and discovery
core_evals <- raw_evals |>
  mutate(crs_number = str_trim(str_squish(crs_number))) |>
  filter(dept == "PED" | 
           crs_number %in% c("COR 107", "COR 110", "BCM 101", "BCM 103", 
                             "NSM 202", "HIS 121", "PHI 118")) |>
  # fix unparsable strings
  mutate(question = str_replace_all(string = question,
                                    pattern = "�",
                                    replacement = "'")) 

ped_qs <- core_evals |> 
  filter(dept == "PED",
         str_detect(question, "instructor", negate = T),
         str_detect(question, "La Vida leaders", negate = T),
         str_detect(question, 
                    "Rate this activity/requirement: ", 
                    negate = T)) |>
  distinct(question)

length(pull(core_evals |>
  distinct(part_svyid),
  part_svyid))
#2183

# check for which courses don't have each of the la vida questions. 
# It's all online courses
awakening <- core_evals |>
  filter(
    str_detect(
      question, 
      "course in awakening and strengthening your understanding of the Christian faith"
    )
  )

core_evals_no_awakening <- core_evals |>
  anti_join(awakening, join_by(period, crs_number, crs_section)) 

overall_course <- core_evals |>
  filter(str_detect(question, "Overall, I rate this course as:"))

core_evals_no_overall_course <- core_evals |>
  anti_join(overall_course, join_by(period, crs_number, crs_section))

overall_exp <- core_evals |>
  filter(str_detect(question, "Overall, I rate this experience as:"))

core_evals_no_overall_exp <- core_evals |>
  anti_join(overall_exp, join_by(period, crs_number, crs_section))

core_evals_no_overall <- core_evals |>
  anti_join(overall_course, join_by(period, crs_number, crs_section)) |>
  anti_join(overall_exp, join_by(period, crs_number, crs_section))

relevancy <- core_evals |>
  filter(str_detect(question, "Rate the course relevancy"))

core_evals_no_relevancy <- core_evals |>
  anti_join(relevancy, join_by(period, crs_number, crs_section))


# Make a dataset with one row per student

core_evals |>
  filter(str_detect(question, "Overall, I rate this course as")) |>
  distinct(questionid)

data <- core_evals |>
  filter(
    questionid %in% c(
      "I4570LAT8R", # At this point, would you say La Vida continues to be a powerful experience in your life? 
      "I41F0UAVKM", # Do you recommend Discovery for Gordon students?
      "I3Q30NA4BJ", "I41C0XOODD", "I4540UXERM", # How effective is the course in awakening and strengthening your understanding of the Christian faith? / and how it connects to course content?
      "I4540Y2ABW", "I3Q30NA4BL", "I41C0XOODE", # Overall, I rate this course as: / experience as:
      "I41F0TQ71L" # Rate the course relevancy for Gordon students:
      )
  ) |>
  mutate(
    # make a la vida/discovery/core indicator column
    group = case_when(crs_number == "PED 015" ~ "Discovery",
                      crs_number == "PED 016" ~ "La Vida",
                      .default = "Core"),
    # make a question column name column
    question_short = case_when(
      questionid %in% c("I4570LAT8R") ~ "la_vida_powerful_exp",
      questionid %in% c("I41F0UAVKM") ~ "discovery_recommend",
      questionid %in% c("I3Q30NA4BJ", "I41C0XOODD", "I4540UXERM") ~ "awakening_christian_faith",
      questionid %in% c("I4540Y2ABW", "I3Q30NA4BL", "I41C0XOODE") ~ "overall_course",
      questionid %in% c("I41F0TQ71L") ~ "discovery_relevancy"
    )
  ) |>
  
  # when surveys are submitted at the same time, they are given the same 
  # part_svyid. Separate not necessarily surveys but at least one question of 
  # each kind between surveys submitted at the same time into two distinct 
  # buckets
  group_by(part_svyid, question_short) |>
  mutate(count = n(),
         counter = row_number(part_svyid)) |> 
  ungroup() |>
  mutate(part_svyid = paste0(part_svyid, counter)) |>

  select(period, part_svyid, group, question_short, response) |>
  pivot_wider(names_from = question_short,
              values_from = response) 

write_csv(data, "data/analysis_data/course_evals.csv")
