library(DBI)
library(odbc)
library(keyring)
library(tidyverse)

jenzabar <- dbConnect(odbc(),
  driver = "/opt/homebrew/lib/libmsodbcsql.17.dylib",
  server = "adminprodsql.gordon.edu",
  port = 1433,
  database = "TmsEPrd",
  uid = "sam.mason",
  pwd = keyring::key_get("adminprodsql")
)

student_crs_hist <- tbl(jenzabar, "STUDENT_CRS_HIST")

# I need each students entry (matriculation) year and term

# student_master <- tbl(jenzabar, "STUDENT_MASTER")

# student_master has some very peculiar entry years and terms, including
# several that occur after students have been taking classes at Gordon for
# years. Josephine pointed me to degree_history, which has an entrance date
# column that, at first glance, looks to be more accurate.

# degree_history <- tbl(jenzabar, "DEGREE_HISTORY")

# Turns out the ENTRY_DTE column here is also not the best, just the first
# time a student took a class at Gordon. Because students can take classes well
# before matriculating (and needing to complete PED 015/016) I'll need to look
# elsewhere.

# from CTS on 5/27/25
# The most reliable data point about matriculation is STUDENT_DIV_MAST.ENTRY_DTE.
# Unfortunately, this is a date and not a year code and a term code. However,
# it is supposed to be an exact match to the first day of the term they started,
# so you could join to YEAR_TERM_TABLE.TRM_BEGIN_DTE to get the corresponding
# year code and term code.

# This message is in relation to finding the year and term that student actually
# entered Gordon as a degree-seeking student. I need to know this because it
# informs how I understand the timing of their participation in the La Vida
# wilderness expedition

student_div_mast <- tbl(jenzabar, "STUDENT_DIV_MAST")
year_term_table <- tbl(jenzabar, "YEAR_TERM_TABLE")

# Pulling the enrollment data from Jenzabar -------------------------------

student_crs_hist |>
  filter(YR_CDE >= 2010) |>
  select(
    ID_NUM, YR_CDE, TRM_CDE, SUBTERM_CDE, STUD_SEQ_NUM,
    TRANSACTION_STS, CRS_CDE, ADV_REQ_CDE, CRS_TITLE, GRADE_CDE
  ) |>
  # left_join(student_master |> select(ID_NUM, ENTRANCE_YR, ENTRANCE_TRM),
  #   by = "ID_NUM"
  # ) |>
  # left_join(
  #   degree_history |>
  #     filter(DIV_CDE == "UG") |>
  #     select(ID_NUM, ENTRY_DTE),
  #   by = "ID_NUM"
  # ) |>
  left_join(
    student_div_mast |> # these two joins to get entry year and term
      filter(DIV_CDE == "UG") |>
      select(ID_NUM, ENTRY_DTE),
    by = "ID_NUM"
  ) |>
  left_join(
    year_term_table |>
      select(TRM_BEGIN_DTE, ENTRY_YR = YR_CDE, ENTRY_TRM = TRM_CDE),
    by = join_by(ENTRY_DTE == TRM_BEGIN_DTE)
  ) |>
  collect() |>
  filter(str_detect(CRS_CDE, "^PED\\s*01[56]")) |>
  # filter(!str_detect(CRS_TITLE, regex("remote", ignore_case = T))) |>
  # filter(!str_detect(CRS_TITLE, regex("compass", ignore_case = T))) |>
  # filter(!str_detect(CRS_TITLE, regex("summit", ignore_case = T))) |>
  mutate(across(where(is.character), str_trim)) ->
lavida_disco

# Refining and addressing irregularities ----------------------------------

# Without removing any special version of PED 015/016, there are 6711 students.
# Removing remote discovery, Compass, and La Vida Summit, there are 6638.

# In the 2006 through 2008, several students are given matriculation dates that
# don't line up with start of term. Both fall and spring term entrants. For fall
# they seems to be given a possibly generic September 1st entry, and for spring
# a January 1st entry. I'll follow up with the Registrar's office to figure
# what is going on with these folks. There are also, strangely, several students
# with summer matriculation dates. It looks like many of these folks did La Vida
# and then peaced, meaning that they never actually matriculated.

# I think that it is best two remove these folks who never actually ended up
# really attending Gordon. I'll also manually fix the ENTRY_DTE column for the
# students who at least took a semester of classes here.

lavida_disco |>
  anti_join(
    student_crs_hist |>
      collect() |>
      semi_join(
        lavida_disco |>
          filter(ENTRY_TRM == "SU"),
        by = "ID_NUM"
      ) |>
      group_by(ID_NUM) |>
      summarize(CREDS = sum(HRS_EARNED)) |>
      filter(CREDS == 0),
    by = "ID_NUM"
  ) ->
lavida_disco

# 9589671 entered Fall 2011
# 9607750 entered Fall 2012
# 50009468 entered Fall 2012
# 50128114 entered Fall 2012
# 50043068 entered Fall 2015

lavida_disco |>
  mutate(ENTRY_YR = as.numeric(ENTRY_YR)) |>
  mutate(
    ENTRY_YR = case_when(
      ID_NUM == 9589671 ~ 2011,
      ID_NUM == 9607750 ~ 2012,
      ID_NUM == 50009468 ~ 2012,
      ID_NUM == 50043068 ~ 2012,
      ID_NUM == 50128114 ~ 2015,
      .default = ENTRY_YR
    ),
    ENTRY_TRM = case_when(
      ID_NUM == 9589671 ~ "FA",
      ID_NUM == 9607750 ~ "FA",
      ID_NUM == 50009468 ~ "FA",
      ID_NUM == 50043068 ~ "FA",
      ID_NUM == 50128114 ~ "FA",
      .default = ENTRY_TRM
    )
  ) ->
lavida_disco

# Now there are 6632 students.

# There remain some peculiarities here. There are 8 students who have entry
# dates well before taking PED 015/016. 9571114, for example, enters in 2006,
# takes PED 015 in 2006, which isn't included in the >= 2010 filter, but then
# takes PED 016 in their four year, 2010. I didn't investigate everyone, but
# it looks like these are simply folks who, for whatever reason, didn't take
# PED 015/016 their first year. In many cases they seem to have put it off until
# their third or fourth years. These students also lack YR and TRM codes for
# entry because they matriculated under a different SIS. Their ENTRY_DTE data
# doesn't align with term start dates, so I'll just manually fix these entries.

lavida_disco |>
  mutate(
    ENTRY_YR = case_when(
      is.na(ENTRY_YR) & month(ENTRY_DTE) == 9 ~ year(ENTRY_DTE),
      is.na(ENTRY_YR) & month(ENTRY_DTE) == 1 ~ year(ENTRY_DTE) - 1,
      .default = ENTRY_YR
    ),
    ENTRY_TRM = case_when(
      is.na(ENTRY_TRM) & month(ENTRY_DTE) == 9 ~ "FA",
      is.na(ENTRY_TRM) & month(ENTRY_DTE) == 1 ~ "SP",
      .default = ENTRY_TRM
    )
  ) ->
lavida_disco

# Writing La Vida / Discovery enrollments to file -------------------------

write_csv(lavida_disco, "data/imported_data/lavida_disco_all_enroll.csv")
