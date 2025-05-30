library(tidyverse)

lavida_disco <- read_csv("data/imported_data/lavida_disco_all_enroll.csv")

# There are many instances of students changing sections during add/drop. This
# shows up in the data as a student, in a given term, dropping one section with
# TRANSACTION_STS "D" and then completing (or not) another section with status
# "H". The STUD_SEQ_NUM seems to help distinguish these section switches. If
# a student fails the class, and then later takes it again, the STUD_SEQ_NUM
# is the same. These within-term section changes shouldn't appear as multiple
# entries in the table. Below I remove these schedule changes.

lavida_disco |>
  group_by(ID_NUM, YR_CDE, TRM_CDE) |>
  filter(STUD_SEQ_NUM == max(STUD_SEQ_NUM)) ->
lavida_disco_attempts

# I need to create a column that reports when a student completed their outdoor
# education requirement. Students can either complete La Vida or Discovery. La
# Vida is completed either during the summer before the fall of the student's
# first term, or the summer following their first year. Discovery must also be
# taken during the first year. It is offered all quads except for the 3rd.

# Looks like the registrar started recording pre-1st year La Vida as a summer
# course from the previous academic year at a certain point. The final line in
# first case_when() below accomodates this.

lavida_disco_attempts |>
  mutate(YR_CDE = as.numeric(YR_CDE)) |>
  mutate(YR_LA_VIDA_TAKEN = case_when(
    ADV_REQ_CDE == "PED016" & YR_CDE == ENTRY_YR ~ "First",
    ADV_REQ_CDE == "PED016" & YR_CDE == (ENTRY_YR + 1) ~ "Second",
    ADV_REQ_CDE == "PED016" & YR_CDE == (ENTRY_YR + 2) ~ "Third",
    ADV_REQ_CDE == "PED016" & YR_CDE == (ENTRY_YR + 3) ~ "Fourth",
    ADV_REQ_CDE == "PED016" & YR_CDE == (ENTRY_YR + 4) ~ "Fifth",
    ADV_REQ_CDE == "PED016" & YR_CDE == (ENTRY_YR - 1) ~ "First",
    .default = NA
  )) |>
  mutate(PRE_POST_FY = case_when(
    ADV_REQ_CDE == "PED016" & YR_LA_VIDA_TAKEN == "First" & TRM_CDE == "FA" ~ "Before",
    ADV_REQ_CDE == "PED016" & YR_LA_VIDA_TAKEN == "First" & TRM_CDE == "SP" ~ "After",
    ADV_REQ_CDE == "PED016" & YR_LA_VIDA_TAKEN == "First" & TRM_CDE == "SU" & YR_CDE == (ENTRY_YR - 1) ~ "Before",
    ADV_REQ_CDE == "PED016" & YR_LA_VIDA_TAKEN == "First" & TRM_CDE == "SU" & YR_CDE == ENTRY_YR ~ "After",
    .default = NA
  )) ->
lavida_disco_attempts

write_csv(lavida_disco_attempts, "data/analysis_data/lavida_disco_attempts.csv")
