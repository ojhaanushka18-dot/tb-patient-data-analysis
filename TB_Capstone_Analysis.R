# TB Capstone Analysis
# Questions 1-3
# Load required packages
library(readxl)
library(tidyverse)
library(dplyr)
library(janitor)

# Import the original Excel dataset
tb_data <- read_excel("R Classes/Basics/data/clinical parameters_TBpatients.xlsx")

# Create a cleaned copy of the dataset
tb_clean <- tb_data %>%
  clean_names()

# Q1: Move assessment date to the first column and rename it
tb_clean <- tb_clean %>%
  relocate(date_of_assessment) %>%
  rename(start = date_of_assessment)


# Q2: Create a unique ID from the row number
tb_clean <- tb_clean %>%
  tibble::rownames_to_column(var = "id")


# Q3 Part A: Calculate assessment delay in days
tb_clean <- tb_clean %>%
  mutate(
    delay = as.numeric(
      difftime(end, start, units = "days")
    )
  )


# Q3 Part B: Replace missing age values with mean age
tb_clean <- tb_clean %>%
  mutate(
    age = ifelse(
      is.na(age),
      mean(age, na.rm = TRUE),
      age
    )
  )


# Question 4: Percentage of patients with cough

cough_summary <- tb_clean %>%
  summarise(
    n_with_cough  = sum(current_symptoms_cough == 1, na.rm = TRUE),
    n_recorded    = sum(!is.na(current_symptoms_cough)),
    percent_cough = round(n_with_cough / n_recorded * 100, 2)
  )

cough_summary



# Question 5: Stacked column chart of number of patients by district and gender

ggplot(tb_clean, aes(x = district, fill = gender)) +
  geom_bar() +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    position = position_stack(vjust = 0.5)
  ) +
  labs(
    title = "Number of TB Patients by District and Gender",
    x = "District",
    y = "Number of patients",
    fill = "Gender"
  )



# Question 6: Is age normally distributed?
# Plot 1: Histogram of age

ggplot(tb_clean, aes(x = age)) +
  geom_histogram(
    binwidth = 5,
    fill = "steelblue",
    colour = "white"
  ) +
  labs(
    title = "Distribution of Patient Age",
    x = "Age (years)",
    y = "Number of patients"
  )



# Plot 2: Normal Q-Q plot of age

ggplot(tb_clean, aes(sample = age)) +
  stat_qq() +
  stat_qq_line(colour = "red") +
  labs(
    title = "Normal Q-Q Plot of Patient Age",
    x = "Theoretical quantiles",
    y = "Observed age (years)"
  )


# Question 7: Stratify number of abnormal parameters
# Keep zero separate because the professor's rules do not define it

tb_clean <- tb_clean %>%
  mutate(
    derived_risk_category = case_when(
      no_of_abnormal_parameters == 0 ~
        "0 abnormal parameters (not classified)",
      no_of_abnormal_parameters == 1 ~
        "Low risk",
      no_of_abnormal_parameters >= 2 &
        no_of_abnormal_parameters <= 3 ~
        "Moderate risk",
      no_of_abnormal_parameters >= 4 ~
        "High risk"
    ),
    derived_risk_category = factor(
      derived_risk_category,
      levels = c(
        "0 abnormal parameters (not classified)",
        "Low risk",
        "Moderate risk",
        "High risk"
      )
    )
  )



# Question 8: Simplify the medical officer's risk rating
# Keep the original long-text column unchanged

tb_clean <- tb_clean %>%
  mutate(
    medical_officer_risk_category = case_when(
      is.na(final_risk_stratification_by_the_medical_officer) ~
        "Missing/Not recorded",
      
      str_detect(
        final_risk_stratification_by_the_medical_officer,
        regex("^Low risk", ignore_case = TRUE)
      ) ~ "Low risk",
      
      str_detect(
        final_risk_stratification_by_the_medical_officer,
        regex("^Moderate risk", ignore_case = TRUE)
      ) ~ "Moderate risk",
      
      str_detect(
        final_risk_stratification_by_the_medical_officer,
        regex("^High risk", ignore_case = TRUE)
      ) ~ "High risk"
    ),
    medical_officer_risk_category = factor(
      medical_officer_risk_category,
      levels = c(
        "Low risk",
        "Moderate risk",
        "High risk",
        "Missing/Not recorded"
      )
    )
  )


# Question 8: Cross-tabulation of medical officer risk vs derived risk
mo_vs_derived <- table(
  tb_clean$medical_officer_risk_category,
  tb_clean$derived_risk_category,
  dnn = c("Medical officer", "Derived rule")
)

# Display the table with row and column totals
addmargins(mo_vs_derived)

# Key Q8 result: medical officer says Low risk,
mo_vs_derived["Low risk", "High risk"]



# Question 9: Count patients with abnormal clinical parameters

abnormal_counts <- tb_clean %>%
  summarise(
    `Respiratory rate`  = sum(respiratory_rate == 1, na.rm = TRUE),
    `BMI`               = sum(bmi == 1, na.rm = TRUE),
    `Haemoglobin`       = sum(haemoglobin == 1, na.rm = TRUE),
    `Oxygen saturation` = sum(oxygen_saturation == 1, na.rm = TRUE)
  ) %>%
  pivot_longer(
    everything(),
    names_to = "parameter",
    values_to = "n_abnormal"
  ) %>%
  arrange(desc(n_abnormal)) %>%
  mutate(parameter = factor(parameter, levels = parameter))

abnormal_counts


# Question 9: Descending-order column chart

ggplot(abnormal_counts, aes(x = parameter, y = n_abnormal)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = n_abnormal), vjust = -0.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Number of Patients with Abnormal Parameters",
    x = "Parameter",
    y = "Number of patients with abnormal value"
  )


# Question 10: Subset of males with no blood in sputum

male_no_blood_sputum <- tb_clean %>%
  filter(
    gender == "Male",
    blood_in_sputum == 0
  )


# Question 11: The 16 indicators listed in the data dictionary
# as components of "No of Abnormal parameters"

abnormal_indicators <- c(
  "blood_in_sputum",
  "pulse_rate",
  "temperature",
  "systolic_bp",
  "diastolic_bp",
  "respiratory_rate",
  "oxygen_saturation",
  "bmi",
  "cal_muac",
  "paedal_ordema",
  "jaundice",
  "haemoglobin",
  "wbc",
  "rbs",
  "hiv",
  "chest_x_ray"
)


# Question 11: Compare recorded and independently calculated abnormal counts

q11_check <- tibble(
  id = tb_clean$id,
  recorded_count = tb_clean$no_of_abnormal_parameters,
  independent_count = rowSums(
    tb_clean[abnormal_indicators] == 1,
    na.rm = TRUE
  )
) %>%
  mutate(
    difference = independent_count - recorded_count,
    mismatch = difference != 0
  )


# Question 11: Investigate indicator patterns by count difference

q11_indicator_pattern <- tb_clean %>%
  select(id, all_of(abnormal_indicators)) %>%
  left_join(
    q11_check %>% select(id, difference),
    by = "id"
  ) %>%
  pivot_longer(
    cols = all_of(abnormal_indicators),
    names_to = "indicator",
    values_to = "flag_value"
  ) %>%
  group_by(indicator, difference) %>%
  summarise(
    n_patients = n(),
    n_flagged = sum(flag_value == 1, na.rm = TRUE),
    percent_flagged = round(
      100 * sum(flag_value == 1, na.rm = TRUE) / n(),
      1
    ),
    .groups = "drop"
  ) %>%
  arrange(indicator, difference)


# Question 11: Check whether recorded count matches a count excluding temperature

other_15 <- setdiff(abnormal_indicators, "temperature")

q11_temperature_check <- tb_clean %>%
  select(id, temperature, all_of(other_15)) %>%
  left_join(
    q11_check %>%
      select(id, recorded_count, independent_count, difference),
    by = "id"
  ) %>%
  mutate(
    count_without_temperature = rowSums(
      across(all_of(other_15))
    ),
    recorded_equals_without_temp =
      recorded_count == count_without_temperature
  ) %>%
  select(
    id,
    temperature,
    recorded_count,
    independent_count,
    difference,
    count_without_temperature,
    recorded_equals_without_temp
  )


# Question 11: Summarise the temperature diagnostic by difference group

q11_temperature_summary <- q11_temperature_check %>%
  mutate(
    difference_group = case_when(
      difference < 0  ~ "-1 (calculated < recorded)",
      difference == 0 ~ "0 (match)",
      difference > 0  ~ "+1 (calculated > recorded)"
    ),
    difference_group = factor(
      difference_group,
      levels = c(
        "-1 (calculated < recorded)",
        "0 (match)",
        "+1 (calculated > recorded)"
      )
    )
  ) %>%
  group_by(difference_group) %>%
  summarise(
    n_patients = n(),
    n_temperature_1 = sum(temperature == 1),
    n_recorded_equals_without_temp =
      sum(recorded_equals_without_temp),
    .groups = "drop"
  )

q11_temperature_summary
