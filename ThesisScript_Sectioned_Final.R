# R Script for Thesis Analysis
# Author: Bailey Bowman
# Date: April 2025
#----------------------------------------------------------------------


# Data Cleaning & Recoding
#------------------------
Merged_Data$Veteran <- as.numeric(Merged_Data$Veteran)
sum(is.na(Merged_Data$Veteran))
Merged_Data$Mechanic <- as.numeric(Merged_Data$Mechanic)
Merged_Data$Broadband <- grepl("Broadband access", Merged_Data$`Important issues`)
Merged_Data$Public_education <- grepl("Public", Merged_Data$`Important issues`)
Merged_Data$Addiction <- grepl("drug", Merged_Data$`Important issues`)
Merged_Data$Healthcare <- grepl("Healthcare", Merged_Data$`Important issues`)
Merged_Data$Rural_development <- grepl("development", Merged_Data$`Important issues`)
Merged_Data$Wealth_inequality <- grepl("inequality", Merged_Data$`Important issues`)
Merged_Data$Unions <- grepl("unionize", Merged_Data$`Important issues`)
Merged_Data$Ag_investments<- grepl("Agricultural", Merged_Data$`Important issues`)
Merged_Data$Good_job <- grepl("job avail", Merged_Data$`Important issues`)
Merged_Data$Out_of_touch <- grepl("out of touch ", Merged_Data$`Why Dems fail ranked`)
Merged_Data$Cities <- grepl("cities ", Merged_Data$`Why Dems fail ranked`)
Merged_Data$Pronouns <- grepl("pronouns ", Merged_Data$`Why Dems fail ranked`)
Merged_Data$Lack_of_effort <- grepl("effort", Merged_Data$`Why Dems fail ranked`)
t.test(Merged_data$Wealth_inequality, mu=1)
t.test(Merged_data$`Working class focus`, mu=1)
Merged_Data$`Travels to rural` <- as.numeric(Merged_Data$`Travels to rural`)
Merged_Data$`Rural priority` <- as.numeric(Merged_Data$`Rural priority`)
Merged_Data$`Rural background` <- as.numeric(Merged_Data$`Rural background`)
Merged_Data$`Working class focus` <- as.numeric(Merged_Data$`Working class focus`)
Merged_Data$`Party registration` <- factor(Merged_Data$`Party registration`)

# Variable Summaries
#------------------
str(Merged_Data$`Party registration`)
# Convert "Party Registration" to a factor, keeping full names
Merged_Data$`Party registration`<- factor(Merged_Data$`Party registration`)
str(Merged_Data$`Party registration`)
t.test(Merged_Data$Veteran, mu = .70)
t.test(Merged_Data$Teacher, mu = .75)
Merged_Data$`Education- 2 favorability` <- factor(
  Merged_Data$`Education- 2 favorability`, 
  levels = c("Strongly Disagreeable", "Disagreeable", "Neutral", "Agreeable", "Strongly Agreeable"), 
  ordered = TRUE)
Merged_Data$`Taxes- 1 favorability` <- factor(
  Merged_Data$`Taxes- 1 favorability`, 
  levels = c("Strongly Disagreeable", "Disagreeable", "Neutral", "Agreeable", "Strongly Agreeable"), 
  ordered = TRUE)
Merged_Data$`Taxes- 2 favorability` <- factor(
  Merged_Data$`Taxes- 2 favorability`, 
  levels = c("Strongly Disagreeable", "Disagreeable", "Neutral", "Agreeable", "Strongly Agreeable"), 
  ordered = TRUE)
Merged_Data$`Ag-1 favorability` <- factor(
  Merged_Data$`Ag-1 favorability`, 
  levels = c("Strongly Disagreeable", "Disagreeable", "Neutral", "Agreeable", "Strongly Agreeable"), 
  ordered = TRUE)
Merged_Data$`Ag-2 favorability` <- factor(
  Merged_Data$`Ag-2 favorability`, 
  levels = c("Strongly Disagreeable", "Disagreeable", "Neutral", "Agreeable", "Strongly Agreeable"), 
  ordered = TRUE)
levels(Merged_Data$`Ag-1 favorability`)
levels(Merged_Data$`Education- 2 favorability`)
table(Merged_Data$`Education- 2 favorability`)
summary(Merged_Data$Teacher)
summary(Merged_Data$Veteran)
summary(Merged_Data$Mechanic)
summary(Merged_Data$`Rural background`)
summary(Merged_Data$`Rural priority`)
summary(Merged_Data$Education)
summary(Merged_Data$Taxes)
summary(Merged_Data$Ag)
summary(Merged_Data$`Education- 1 favorability`)
summary(Merged_Data$`Education- 2 favorability`)
summary(Merged_Data$`Taxes- 1 favorability`)
summary(Merged_Data$`Taxes- 2 favorability`)
summary(Merged_Data$`Ag-1 favorability`)
summary(Merged_Data$`Ag-2 favorability`)   
summary(Merged_Data$`Working class focus`)
summary(Merged_Data$Wealth_inequality)
summary(Merged_Data$`Party registration`)
summary(Merged_Data$Race)
unique(Merged_Data$Race)
summary(Merged_Data$`Split ticket voting`)
unique(Merged_Data$`Split ticket voting`)
Merged_Data <- Merged_Data %>%
  mutate(Split.ticket.voting = case_when(
    Split.ticket.voting == "Yes" ~ 1,
    Split.ticket.voting == "No" ~ 0,
    TRUE ~ NA_real_  # Keeps NA values unchanged
  ))
table(Merged_Data$Split.ticket.voting)
Merged_Data <- Merged_Data %>%
  mutate(Voted.for.Dem = case_when(
    Voted.for.Dem == "Yes" ~ 1,
    Voted.for.Dem == "No" ~ 0,
    TRUE ~ NA_real_  # Keeps NA values unchanged
  ))
table(Merged_Data$Voted.for.Dem)
unique(Merged_Data$Employment)
unique(Merged_Data$sex)
Merged_Data <- Merged_Data %>%
  mutate(sex = case_when(
   sex == "Female" ~ 1,
    sex == "Male" ~ 0,
    TRUE ~ NA_real_  # Keeps NA values unchanged
  )) #### PUT IN CODING MANUAL 
table(Merged_Data$sex)
unique(Merged_Data$sex)
Merged_Data <- Merged_Data %>%
  mutate(Hispanic = case_when(
    Hispanic == "Yes" ~ 1,
    Hispanic == "None of these" ~ 0,
    TRUE ~ NA_real_  # Keeps NA values unchanged
  )) #### PUT IN CODING MANUAL 

unique(Merged_Data$Hispanic)
summary(as.factor(Merged_Data$income))
summary(as.factor(Merged_Data$Education..1.favorability)
unique(Merged_Data$Education..1.favorability)        
Merged_Data <- Merged_Data %>%
  mutate(Education..1.favorability = recode(Education..1.favorability,
                                            "Strongly Disagreeable" = 1,
                                            "Disagreeable" = 2,
                                            "Neutral" = 3,
                                            "Agreeable" = 4,
                                            "Strongly Agreeable" = 5,
                                            .default = NA_real_  # Ensures other values remain NA
  ))       
unique(Merged_Data$Education..1.favorability)
Merged_Data <- Merged_Data %>%
  mutate(Education..2.favorability = recode(Education..2.favorability,
                                            "Strongly Disagreeable" = 1,
                                            "Disagreeable" = 2,
                                            "Neutral" = 3,
                                            "Agreeable" = 4,
                                            "Strongly Agreeable" = 5,
                                            .default = NA_real_  # Ensures other values remain NA
  ))       
unique(Merged_Data$Education..2.favorability)
Merged_Data <- Merged_Data %>%
  mutate(Taxes..1.favorability = recode(Taxes..1.favorability,
                                            "Strongly Disagreeable" = 1,
                                            "Disagreeable" = 2,
                                            "Neutral" = 3,
                                            "Agreeable" = 4,
                                            "Strongly Agreeable" = 5,
                                            .default = NA_real_  # Ensures other values remain NA
  ))
unique(Merged_Data$Taxes..1.favorability)

Merged_Data <- Merged_Data %>%
  mutate(Taxes..2.favorability = recode(Taxes..2.favorability,
                                        "Strongly Disagreeable" = 1,
                                        "Disagreeable" = 2,
                                        "Neutral" = 3,
                                        "Agreeable" = 4,
                                        "Strongly Agreeable" = 5,
                                        .default = NA_real_  # Ensures other values remain NA
  ))
unique(Merged_Data$Taxes..2.favorability)
table(Merged_Data$Taxes..2.favorability)

Merged_Data <- Merged_Data %>%
  mutate(Ag.1.favorability = recode(Ag.1.favorability,
                                        "Strongly Disagreeable" = 1,
                                        "Disagreeable" = 2,
                                        "Neutral" = 3,
                                        "Agreeable" = 4,
                                        "Strongly Agreeable" = 5,
                                        .default = NA_real_  # Ensures other values remain NA
  ))
Merged_Data <- Merged_Data %>%
  mutate(Ag.2.favorability = recode(Ag.2.favorability,
                                    "Strongly Disagreeable" = 1,
                                    "Disagreeable" = 2,
                                    "Neutral" = 3,
                                    "Agreeable" = 4,
                                    "Strongly Agreeable" = 5,
                                    .default = NA_real_  # Ensures other values remain NA
  ))
unique(Merged_Data$Ag.2.favorability)
summary(Merged_Data$Ag.2.favorability)
summary(Merged_Data$Education..1.favorability)
summary(Merged_Data$Education..2.favorability)
summary(Merged_Data$Taxes..2.favorability)
unique(Merged_Data$Race)
Merged_Data <- Merged_Data %>%
  mutate(Race = recode(Race,
                                    "White" = 0,
                                    "10" = NA,
                                    "Other" = 1,
                                    "Asian" = 2,
                                    .default = NA_real_  # Ensures other values remain NA
  ))

# Load Libraries
#--------------
library(dplyr)

Merged_Data <- Merged_Data %>%
  mutate(Race = recode(Race,
                       "White" = 0,
                       "10" = NA_real_,  # Explicitly set NA as NA_real_
                       "Other" = 1,
                       "Asian" = 2,
                       .default = NA_real_  # Ensures other values remain NA
  ))
unique(Merged_Data$Race)
Merged_Data <- Merged_Data %>%
  mutate(Republican.Strength = case_when(
   Republican.Strength == "Strong" ~ 1,
    Republican.Strength == "Not very strong" ~ 0,
    TRUE ~ NA_real_  # Keeps NA values unchanged
  ))
unique(Merged_Data$Republican.Strength)
Merged_Data <- Merged_Data %>%
  mutate(Democrat.strength = case_when(
    Democrat.strength == "Strong" ~ 1,
    Democrat.strength == "Not very strong" ~ 0,
    TRUE ~ NA_real_  # Keeps NA values unchanged
  ))
unique(Merged_Data$Democrat.strength)
Merged_Data <- Merged_Data %>%
  mutate(Closer.to.Republican.or.Democrat = case_when(
    Closer.to.Republican.or.Democrat == "Republican" ~ 1,
    Closer.to.Republican.or.Democrat == "Democratic" ~ 0,
    TRUE ~ NA_real_  # Keeps NA values unchanged
  ))
unique(Merged_Data$Closer.to.Republican.or.Democrat)
Merged_Data <- Merged_Data %>%
  mutate(Broadband = case_when(
    Broadband == "TRUE" ~ 1,
    Broadband == "FALSE" ~ 0,
    TRUE ~ NA_real_  # Keeps NA values unchanged
  ))
unique(Merged_Data$Broadband)
Merged_Data <- Merged_Data %>%
  mutate(Public_education = case_when(
    Public_education == "TRUE" ~ 1,
    Public_education == "FALSE" ~ 0,
    TRUE ~ NA_real_  # Keeps NA values unchanged
  ))
unique(Merged_Data$Addiction)
Merged_Data1 <- data[rowSums(is.na(data)) <= 10, ]
# Remove rows with more than 10 NAs
Merged_Data <- Merged_Data[rowSums(is.na(Merged_Data)) <= 15, ]
Merged_Data <- Merged_Data %>%
  mutate(Addiction = case_when(
    Addiction == "TRUE" ~ 1,
   Addiction== "FALSE" ~ 0,
    TRUE ~ NA_real_  # Keeps NA values unchanged
  ))
Merged_Data <- Merged_Data %>%
  mutate(Healthcare = case_when(
    Healthcare == "TRUE" ~ 1,
    Healthcare == "FALSE" ~ 0,
    TRUE ~ NA_real_  # Keeps NA values unchanged
  ))
Merged_Data <- Merged_Data %>%
  mutate(Rural_development = case_when(
    Rural_development == "TRUE" ~ 1,
    Rural_development == "FALSE" ~ 0,
    TRUE ~ NA_real_  # Keeps NA values unchanged
  ))
Merged_Data <- Merged_Data %>%
  mutate(Wealth_inequality= case_when(
    Wealth_inequality == "TRUE" ~ 1,
    Wealth_inequality == "FALSE" ~ 0,
    TRUE ~ NA_real_  # Keeps NA values unchanged
  ))
Merged_Data <- Merged_Data %>%
  mutate(Unions = case_when(
    Unions == "TRUE" ~ 1,
    Unions == "FALSE" ~ 0,
    TRUE ~ NA_real_  # Keeps NA values unchanged
  ))
Merged_Data <- Merged_Data %>%
  mutate(Ag_investments = case_when(
    Ag_investments == "TRUE" ~ 1,
    Ag_investments == "FALSE" ~ 0,
    TRUE ~ NA_real_  # Keeps NA values unchanged
  ))
Merged_Data <- Merged_Data %>%
  mutate(Good_job = case_when(
    Good_job == "TRUE" ~ 1,
    Good_job == "FALSE" ~ 0,
    TRUE ~ NA_real_  # Keeps NA values unchanged
  ))
unique(Merged_Data$Party.registration)

# Recode Party.registration to numeric values
Merged_Data$Party.registration.numeric <- recode(Merged_Data$Party.registration,
                                                 "Republican" = 0,
                                                 "Independent" = 1,
                                                 "Democratic" = 2,
                                                 "None" = 3)

Merged_Data <- Merged_Data %>%
  mutate(Out_of_touch = case_when(
    Out_of_touch == "TRUE" ~ 1,
    Out_of_touch == "FALSE" ~ 0,
    TRUE ~ NA_real_  # Keeps NA values unchanged
  ))
Merged_Data <- Merged_Data %>%
  mutate(Cities = case_when(
    Cities == "TRUE" ~ 1,
    Cities == "FALSE" ~ 0,
    TRUE ~ NA_real_  # Keeps NA values unchanged
  ))
Merged_Data <- Merged_Data %>%
  mutate(Pronouns = case_when(
    Pronouns == "TRUE" ~ 1,
    Pronouns == "FALSE" ~ 0,
    TRUE ~ NA_real_  # Keeps NA values unchanged
  ))
Merged_Data <- Merged_Data %>%
  mutate(Lack_of_effort = case_when(
    Lack_of_effort == "TRUE" ~ 1,
    Lack_of_effort == "FALSE" ~ 0,
    TRUE ~ NA_real_  # Keeps NA values unchanged
  ))

unique(Merged_Data$Self.identified.partisanship)

# Recode Self.identified.partisanship to numeric values
Merged_Data$Self.identified.partisanship.numeric <- recode(Merged_Data$Self.identified.partisanship,
                                                           "Republican" = 0,
                                                           "Independent" = 1,
                                                           "Democrat" = 2,
                                                           "No preference" = 3,
                                                           "Other" = 4)
unique(Merged_Data$Education.Level)

# Recode education.level to numeric values (ordered)
Merged_Data$education.level.numeric <- recode(Merged_Data$Education.Level,
                                              "Less than high school degree" = 0,
                                              "High school graduate (high school diploma or equivalent including GED)" = 1,
                                              "Some college but no degree" = 2,
                                              "Associate degree in college (2-year)" = 3,
                                              "Bachelor's degree in college (4-year)" = 4,
                                              "Master's degree" = 5,
                                              "Professional degree (JD, MD)" = 6,
                                              "Doctoral degree" = 7
)
library(dplyr)

unique(Merged_Data$income)
Merged_Data$income.numeric <- recode(Merged_Data$income,
                                     "Less than $10,000" = 0,
                                     "$10,000 to $19,999" = 1,
                                     "$20,000 to $29,999" = 2,
                                     "$30,000 to $39,999" = 3,
                                     "$40,000 to $49,999" = 4,
                                     "$50,000 to $59,999" = 5,
                                     "$60,000 to $69,999" = 6,
                                     "$70,000 to $79,999" = 7,
                                     "$80,000 to $89,999" = 8,
                                     "$90,000 to $99,999" = 9,
                                     "$100,000 to $149,999" = 10,
                                     "$150,000 or more" = 11
)
unique(Merged_Data$Employment)

Merged_Data$employment.binary <- recode(Merged_Data$Employment,
                                        "Working (paid employee)" = 1,
                                        "Working (self-employed)" = 1,
                                        "Not working (retired)" = 0,
                                        "Not working (disabled)" = 0,
                                        "Not working (other)" = 0,
                                        "Prefer not to answer" = NA_real_
)
unique(Merged_Data$Year.of.Birth)

Merged_Data$age <- 2025 - Merged_Data$Year.of.Birth


# Export Data
#-----------
write.csv(Merged_Data, "Merged_Data_Cleaned.csv", row.names = FALSE)


# Load Data
#---------
file.rename("Merged_Data_Cleaned.csv", "~/Desktop/Merged_Data_Cleaned.csv")


# Create Models
#-------------
model_teacher <- glm(Teacher ~ Rural.background + age + education.level.numeric + Race + sex + income.numeric,
                     data = data,
                     family = binomial)

# Predictive Values & Interaction Terms
#-------------------------------------
summary(model_teacher)
data <- read.csv("Merged_Data_Cleaned.csv")
model_veteran <- glm(Veteran ~ Rural.background + age + education.level.numeric + 
                       Race + sex + income.numeric,
                     data = data,
                     family = binomial)

summary(model_veteran)
table(data$Veteran, data$Race)
model_veteran_norace <- glm(Veteran ~ Rural.background + age + education.level.numeric + 
                              sex + income.numeric,
                            data = data,
                            family = binomial)
summary(model_veteran_norace)

model_mechanic <- glm(Mechanic ~ Rural.background + age + education.level.numeric + 
                        sex + income.numeric,
                      data = data,
                      family = binomial)

summary(model_mechanic)
model_interaction <- glm(Teacher ~ Rural.background * education.level.numeric + 
                           age + sex + income.numeric,
                         data = data,
                         family = binomial)
                           age + sex + income.numeric,
                         data = data,
                         family = binomial)
summary(model_interaction)

data$rural_emphasis <- ifelse(data$Rural.background %in% c(1, 2), 1, 0)

data$favored_rural_candidate <- ifelse(data$Teacher == 1 | data$Veteran == 1 | data$Mechanic == 1, 1, 0)
data$rural_emphasis <- ifelse(data$Rural.background %in% c(1, 2), 1, 0)
data$favored_rural_candidate <- ifelse(data$Teacher == 1 | data$Veteran == 1 | data$Mechanic == 1, 1, 0)
subset_data <- subset(data, Race == 0)

model_reframed <- glm(favored_rural_candidate ~ rural_emphasis + age + education.level.numeric + sex + income.numeric,
                      data = subset_data,
                      family = binomial)

summary(model_reframed)
library(nnet)
data$selected_candidate <- with(data, ifelse(Teacher == 1, "Teacher",
                                             ifelse(Veteran == 1, "Veteran",
                                                    ifelse(Mechanic == 1, "Mechanic", NA))))
data$selected_candidate <- factor(data$selected_candidate)
subset_data <- subset(data, Race == 0 & !is.na(selected_candidate))
library(nnet)

model_multi <- multinom(selected_candidate ~ rural_emphasis + age + education.level.numeric + sex + income.numeric,
                        data = subset_data)
summary(model_multi)
model_partisan <- glm(favored_rural_candidate ~ rural_emphasis + Party.registration.numeric +
                        age + education.level.numeric + sex + income.numeric,
                      data = subset_data,
                      family = binomial)

summary(model_partisan)
subset_data$partisan_simple <- recode(subset_data$Party.registration.numeric,
                                      `0` = 0,   # Republican
                                      `1` = 1,   # Independent
                                      `3` = 1,   # None → combine with Independent
                                      `2` = 2,   # Democrat
                                      .default = NA_real_)

model_partisan_simple <- glm(favored_rural_candidate ~ rural_emphasis + partisan_simple +
                               age + education.level.numeric + sex + income.numeric,
                             data = subset_data,
                             family = binomial)

summary(model_partisan_simple)
table(subset_data$favored_rural_candidate, subset_data$partisan_simple)
data$chose_teacher_only <- ifelse(data$Teacher == 1 & data$Veteran == 0 & data$Mechanic == 0, 1, 0)
glm(chose_teacher_only ~ rural_emphasis + partisan_simple + age + education.level.numeric + sex + income.numeric,
    data = subset_data,
    family = binomial)
# Add variable to full dataset
data$chose_teacher_only <- ifelse(data$Teacher == 1 & data$Veteran == 0 & data$Mechanic == 0, 1, 0)

# THEN subset to white rural respondents
subset_data <- subset(data, Race == 0)
model_teacher_only <- glm(chose_teacher_only ~ rural_emphasis + partisan_simple + age +
                            education.level.numeric + sex + income.numeric,
                          data = subset_data,
                          family = binomial)

summary(model_teacher_only)
# 1. Recode Party.registration.numeric into simplified partisanship
data$partisan_simple <- recode(data$Party.registration.numeric,
                               `0` = 0,  # Republican
                               `1` = 1,  # Independent
                               `3` = 1,  # None (combine with Independent)
                               `2` = 2,  # Democrat
                               .default = NA_real_)

# 2. Create the 'chose
data$chose_teacher_only <- ifelse(data$Teacher == 1 & data$Veteran == 0 & data$Mechanic == 0, 1, 0)
subset_data <- subset(data, Race == 0)
                            education.level.numeric + sex + income.numeric,
                          data = subset_data,
                          family = binomial)

summary(model_teacher_only)
subset_data$partisan_binary <- ifelse(subset_data$partisan_simple == 0, 0, 1)

library(logistf)

model_firth_teacher <- logistf(chose_teacher_only ~ rural_emphasis + partisan_simple +
                                 age + education.level.numeric + sex + income.numeric,
                               data = subset_data)

summary(model_firth_teacher)
# Veteran only
data$chose_veteran_only <- ifelse(data$Veteran == 1 & data$Teacher == 0 & data$Mechanic == 0, 1, 0)

# Mechanic only
data$chose_mechanic_only <- ifelse(data$Mechanic == 1 & data$Teacher == 0 & data$Veteran == 0, 1, 0)
library(logistf)

model_firth_veteran <- logistf(chose_veteran_only ~ rural_emphasis + partisan_simple +
                                 age + education.level.numeric + sex + income.numeric,
                               data = subset_data)

summary(model_firth_veteran)
# Veteran-only selection
data$chose_veteran_only <- ifelse(data$Veteran == 1 & data$Teacher == 0 & data$Mechanic == 0, 1, 0)

# Mechanic-only selection
data$chose_mechanic_only <- ifelse(data$Mechanic == 1 & data$Teacher == 0 & data$Veteran == 0, 1, 0)
subset_data <- subset(data, Race == 0)
library(logistf)

model_firth_veteran <- logistf(chose_veteran_only ~ rural_emphasis + partisan_simple +
                                 age + education.level.numeric + sex + income.numeric,
                               data = subset_data)

summary(model_firth_veteran)

model_firth_mechanic <- logistf(chose_mechanic_only ~ rural_emphasis + partisan_simple +
                                  age + education.level.numeric + sex + income.numeric,
                                data = subset_data)

summary(model_firth_mechanic)
model_pop_education <- lm(`Education...1.favorability` ~ Working.class.focus + partisan_simple + age +
                            education.level.numeric + sex + income.numeric,
                          data = subset_data)

names(subset_data) <- make.names(names(subset_data))
model_pop_education <- lm(Education...1.favorability ~ Working.class.focus + partisan_simple + age +
                            education.level.numeric + sex + income.numeric,
                          data = subset_data)

summary(model_pop_education)
grep("Education", names(subset_data), value = TRUE)
model_pop_education <- lm(Education..1.favorability ~ Working.class.focus + partisan_simple + age +
                            education.level.numeric + sex + income.numeric,
                          data = subset_data)

summary(model_pop_education)

grep("Taxes", names(subset_data), value = TRUE)
grep("Ag", names(subset_data), value = TRUE)
model_pop_taxes <- lm(Taxes..1.favorability ~ Working.class.focus + partisan_simple + age +
                        education.level.numeric + sex + income.numeric,
                      data = subset_data)

summary(model_pop_taxes)

model_pop_ag <- lm(Ag.2.favorability ~ Working.class.focus + partisan_simple + age +
                     education.level.numeric + sex + income.numeric,
                   data = subset_data)

summary(model_pop_ag)
subset_data$mean_populist_favorability <- rowMeans(
  subset_data[, c("Education..1.favorability", "Taxes..1.favorability", "Ag.2.favorability")],
  na.rm = TRUE
)
subset_data$high_wcfocus <- ifelse(subset_data$Working.class.focus %in% c(1, 2), 1, 0)
subset_data$selected_all_populist <- ifelse(
  subset_data$Education == 1 & subset_data$Taxes == 1 & subset_data$Ag == 1, 1, 0
)
model_h2_combined <- lm(mean_populist_favorability ~ high_wcfocus + selected_all_populist +
                          Wealth.inequality + partisan_simple + age +
                          education.level.numeric + sex + income.numeric,
                        data = subset_data)

summary(model_h2_combined)
grep("Wealth", names(subset_data), value = TRUE)
                          Wealth_inequality + partisan_simple + age +
                          education.level.numeric + sex + income.numeric,
                        data = subset_data)

summary(model_h2_combined)

# Visualizations
#--------------
ggplot(subset_data, aes(x = Education.Level)) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Education Level of Survey Participants", x = "Education Level", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot1 <- ggplot(subset_data, aes(x = Education.Level)) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Education Level", x = "Education", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot1)
windows()  # On

# Save the education level plot as a PNG
ggplot(subset_data, aes(x = Education.Level)) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Education Level", x = "Education", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("education_plot.png", width = 8, height = 5)
# Create and display the income plot
ggplot(subset_data, aes(x = income)) +
  geom_bar(fill = "mediumpurple") +
  labs(title = "Household Income of Survey Participants",
       x = "Income Bracket",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot as a PNG
ggsave("income_plot.png", width = 8, height = 5)
# Reorder income as an ordered factor
subset_data$income_ordered <- factor(subset_data$income, levels = c(
  "Less than $10,000",
  "$10,000 to $19,999",
  "$20,000 to $29,999",
  "$30,000 to $39,999",
  "$40,000 to $49,999",
  "$50,000 to $59,999",
  "$60,000 to $69,999",
  "$70,000 to $79,999",
  "$80,000 to $89,999",
  "$90,000 to $99,999",
  "$100,000 to $149,999",
  "$150,000 or more",
  "NA"
), ordered = TRUE)
ggplot(subset_data, aes(x = income_ordered)) +
  geom_bar(fill = "mediumpurple") +
  labs(title = "Household Income of Survey Participants",
       x = "Income Bracket",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("income_plot_fixed.png", width = 8, height = 5)
library(tidyr)
library(dplyr)

# Create a smaller dataframe with the two variables of interest
party_long <- subset_data %>%
  select(Party.registration, Self.identified.partisanship) %>%
  pivot_longer(cols = everything(), names_to = "Partisan_Type", values_to = "Party")
library(tidyr)
library(dplyr)
library(ggplot2)

library(tidyr)
library(dplyr)
library(ggplot2)

# Reshape and filter out NAs
party_long <- subset_data %>%
  select(Party.registration, Self.identified.partisanship) %>%
  pivot_longer(cols = everything(), names_to = "Partisan_Type", values_to = "Party") %>%
  filter(!is.na(Party))  # This line removes NAs from both columns

# Create grouped bar plot
ggplot(party_long, aes(x = Party, fill = Partisan_Type)) +
  geom_bar(position = "dodge") +
  labs(title = "Party Registration vs. Self-Identified Partisanship",
       x = "Party Affiliation",
       y = "Count",
       fill = "Reported As") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot
ggsave("party_comparison_plot.png", width = 8, height = 5)

library(ggplot2)

# Create the plot
ggplot(subset_data, aes(x = Self.identified.partisanship, fill = party_strength_leaning)) +
  geom_bar(position = "fill") +
  labs(title = "Party Strength and Leaning Breakdown",
       x = "Partisanship",
       y = "Proportion",
       fill = "Party Strength / Leaning") +
  theme_minimal()

# Save the plot
ggsave("party_strength_leaning_plot.png", width = 8, height = 5)
# Create the plot
plot <- ggplot(subset_data, aes(x = Self.identified.partisanship, fill = party_strength_leaning)) +
  geom_bar(position = "fill") +
  labs(title = "Party Strength and Leaning Breakdown",
       x = "Partisanship",
       y = "Proportion",
       fill = "Party Strength / Leaning") +
  theme_minimal()

# Display the plot in RStudio plot pane
print(plot)

# Save the plot to a file
ggsave("party_strength_leaning_plot.png", plot = plot, width = 8, height = 5)
subset_data$party_strength_leaning <- NA  # Initialize the column

# Democrats
subset_data$party_strength_leaning[subset_data$Self.identified.partisanship == "Democrat" & subset_data$Democrat.strength == 1] <- "Strong Democrat"
subset_data$party_strength_leaning[subset_data$Self.identified.partisanship == "Democrat" & subset_data$Democrat.strength == 0] <- "Not Very Strong Democrat"

# Republicans
subset_data$party_strength_leaning[subset_data$Self.identified.partisanship == "Republican" & subset_data$Republican.Strength == 1] <- "Strong Republican"
subset_data$party_strength_leaning[subset_data$Self.identified.partisanship == "Republican" & subset_data$Republican.Strength == 0] <- "Not Very Strong Republican"

# Independents
subset_data$party_strength_leaning[subset_data$Self.identified.partisanship == "Independent" & subset_data$Closer.to.Republican.or.Democrat == 1] <- "Closer to Republican"
subset_data$party_strength_leaning[subset_data$Self.identified.partisanship == "Independent" & subset_data$Closer.to.Republican.or.Democrat == 0] <- "Closer to Democrat"
subset_data$party_strength_leaning[subset_data$Self.identified.partisanship == "Independent" & is.na(subset_data$Closer.to.Republican.or.Democrat)] <- "Neither"
head(subset_data$party_strength_leaning)
# Create the plot
plot <- ggplot(subset_data, aes(x = Self.identified.partisanship, fill = party_strength_leaning)) +
  geom_bar(position = "fill") +
  labs(title = "Party Strength and Leaning Breakdown",
       x = "Partisanship",
       y = "Proportion",
       fill = "Party Strength / Leaning") +
  theme_minimal()

# Display the plot in RStudio
print(plot)

# Save the plot
ggsave("party_strength_leaning_plot.png", plot = plot, width = 8, height = 5)
# First, we need to create a summary of selections
candidate_selection <- subset_data %>%
  select(Teacher, Veteran, Mechanic) %>%
  summarise(
    Teacher_Selected = sum(Teacher, na.rm = TRUE),
    Veteran_Selected = sum(Veteran, na.rm = TRUE),
    Mechanic_Selected = sum(Mechanic, na.rm = TRUE)
  )

# Create a tidy version of the data for ggplot
candidate_selection_long <- gather(candidate_selection, key = "Candidate", value = "Selection")

# Create the bar chart
ggplot(candidate_selection_long, aes(x = Candidate, y = Selection, fill = Candidate)) +
  geom_bar(stat = "identity") +
  labs(title = "Selection of Rural-Identifying Candidates",
       x = "Candidate",
       y = "Number of Selections",
       fill = "Candidate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot as an image
ggsave("candidate_selection_plot.png", width = 8, height = 5)
# Create a tidy dataset that includes Party Registration
candidate_party_selection <- subset_data %>%
  select(Party.registration, Teacher, Veteran, Mechanic) %>%
  gather(key = "Candidate", value = "Selection", -Party.registration) %>%
  group_by(Party.registration, Candidate) %>%
  summarise(Selections = sum(Selection, na.rm = TRUE))

# Create a bar chart with Party.registration on the x-axis
ggplot(candidate_party_selection, aes(x = Candidate, y = Selections, fill = Party.registration)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Selection of Rural-Identifying Candidates by Party Registration",
       x = "Candidate",
       y = "Number of Selections",
       fill = "Party Registration") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot as an image
ggsave("candidate_selection_by_party_plot.png", width = 8, height = 5)
# Create a tidy dataset that includes Party Registration
candidate_party_selection <- subset_data %>%
  select(Party.registration, Teacher, Veteran, Mechanic) %>%
  gather(key = "Candidate", value = "Selection", -Party.registration) %>%
  group_by(Party.registration, Candidate) %>%
  summarise(Selections = sum(Selection, na.rm = TRUE))
# Create a grouped bar chart by Party.registration
ggplot(candidate_party_selection, aes(x = Candidate, y = Selections, fill = Party.registration)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Selection of Rural-Identifying Candidates by Party Registration",
       x = "Candidate",
       y = "Number of Selections",
       fill = "Party Registration") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Save the plot as an image
ggsave("candidate_selection_by_party_plot.png", width = 8, height = 5)
# Create the dataset with percentages
candidate_party_percentage <- subset_data %>%
  select(Party.registration, Teacher, Veteran, Mechanic) %>%
  gather(key = "Candidate", value = "Selection", -Party.registration) %>%
  group_by(Party.registration, Candidate) %>%
  summarise(Selections = sum(Selection, na.rm = TRUE)) %>%
  # Calculate the total number of respondents for each Party.registration
  group_by(Party.registration) %>%
  mutate(Total = sum(Selections)) %>%
  ungroup() %>%
  # Calculate the percentage of each candidate selection
  mutate(Percentage = Selections / Total * 100)

# Create the plot using percentages
ggplot(candidate_party_percentage, aes(x = Candidate, y = Percentage, fill = Party.registration)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Rural-Identifying Candidates Selected by Party Registration",
       x = "Candidate",
       y = "Percentage of Selections",
       fill = "Party Registration") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot as an image
ggsave("candidate_selection_by_party_percentage_plot.png", width = 8, height = 5)
head(candidate_party_percentage)
# Plot the data
ggplot(candidate_party_percentage, aes(x = Candidate, y = Percentage, fill = Party.registration)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Rural-Identifying Candidates Selected by Party Registration",
       x = "Candidate",
       y = "Percentage of Selections",
       fill = "Party Registration") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Save the plot as an image
ggsave("candidate_selection_by_party_percentage_plot.png", plot = last_plot(), width = 8, height = 5)
library(installr)
brew upgrade r     # Upgrades R if you already have it installed
# Create the dataset with percentages
candidate_party_percentage <- subset_data %>%
  select(Party.registration, Teacher, Veteran, Mechanic) %>%
  gather(key = "Candidate", value = "Selection", -Party.registration) %>%
  group_by(Party.registration, Candidate) %>%
  summarise(Selections = sum(Selection, na.rm = TRUE)) %>%
  # Calculate the total number of respondents for each Party.registration
  group_by(Party.registration) %>%
  mutate(Total = sum(Selections)) %>%
  ungroup() %>%
  # Calculate the percentage of each candidate selection
  mutate(Percentage = Selections / Total * 100)

# Create the plot using percentages
ggplot(candidate_party_percentage, aes(x = Candidate, y = Percentage, fill = Party.registration)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Rural-Identifying Candidates Selected by Party Registration",
       x = "Candidate",
       y = "Percentage of Selections",
       fill = "Party Registration") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot as an image
ggsave("candidate_selection_by_party_percentage_plot.png", width = 8, height = 5)
# Install the tidyverse package if not installed

# Load the dplyr and ggplot2 packages
library(dplyr)
library(ggplot2)
# Create the dataset with percentages
candidate_party_percentage <- subset_data %>%
  select(Party.registration, Teacher, Veteran, Mechanic) %>%
  gather(key = "Candidate", value = "Selection", -Party.registration) %>%
  group_by(Party.registration, Candidate) %>%
  summarise(Selections = sum(Selection, na.rm = TRUE)) %>%
  # Calculate the total number of respondents for each Party.registration
  group_by(Party.registration) %>%
  mutate(Total = sum(Selections)) %>%
  ungroup() %>%
  # Calculate the percentage of each candidate selection
  mutate(Percentage = Selections / Total * 100)

# Create the plot using percentages
ggplot(candidate_party_percentage, aes(x = Candidate, y = Percentage, fill = Party.registration)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Rural-Identifying Candidates Selected by Party Registration",
       x = "Candidate",
       y = "Percentage of Selections",
       fill = "Party Registration") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot as an image
ggsave("candidate_selection_by_party_percentage_plot.png", width = 8, height = 5)
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Create the dataset with percentages
candidate_party_percentage <- subset_data %>%
  select(Party.registration, Teacher, Veteran, Mechanic) %>%
  pivot_longer(cols = c(Teacher, Veteran, Mechanic), names_to = "Candidate", values_to = "Selection") %>%
  group_by(Party.registration, Candidate) %>%
  summarise(Selections = sum(Selection, na.rm = TRUE)) %>%
  # Calculate the total number of respondents for each Party.registration
  group_by(Party.registration) %>%
  mutate(Total = sum(Selections)) %>%
  ungroup() %>%
  # Calculate the percentage of each candidate selection
  mutate(Percentage = Selections / Total * 100)

# Create the plot using percentages
ggplot(candidate_party_percentage, aes(x = Candidate, y = Percentage, fill = Party.registration)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Rural-Identifying Candidates Selected by Party Registration",
       x = "Candidate",
       y = "Percentage of Selections",
       fill = "Party Registration") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot as an image
ggsave("candidate_selection_by_party_percentage_plot.png", width = 8, height = 5)
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Create the dataset with percentages and remove NA values
candidate_party_percentage <- subset_data %>%
  select(Party.registration, Teacher, Veteran, Mechanic) %>%
  pivot_longer(cols = c(Teacher, Veteran, Mechanic), names_to = "Candidate", values_to = "Selection") %>%
  filter(!is.na(Party.registration) & !is.na(Candidate)) %>%  # Remove NA values
  group_by(Party.registration, Candidate) %>%
  summarise(Selections = sum(Selection, na.rm = TRUE)) %>%
  # Calculate the total number of respondents for each Party.registration
  group_by(Party.registration) %>%
  mutate(Total = sum(Selections)) %>%
  ungroup() %>%
  # Calculate the percentage of each candidate selection
  mutate(Percentage = Selections / Total * 100)

# Create the plot using percentages
ggplot(candidate_party_percentage, aes(x = Candidate, y = Percentage, fill = Party.registration)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Rural-Identifying Candidates Selected by Party Registration",
       x = "Candidate",
       y = "Percentage of Selections",
       fill = "Party Registration") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot as an image
ggsave("candidate_selection_by_party_percentage_plot.png", width = 8, height = 5)
# Create the dataset with raw counts (number of selections) and remove NA values
candidate_party_counts <- subset_data %>%
  select(Party.registration, Teacher, Veteran, Mechanic) %>%
  pivot_longer(cols = c(Teacher, Veteran, Mechanic), names_to = "Candidate", values_to = "Selection") %>%
  filter(!is.na(Party.registration) & !is.na(Candidate)) %>%  # Remove NA values
  group_by(Party.registration, Candidate) %>%
  summarise(Selections = sum(Selection, na.rm = TRUE))  # Number of selections

# Create the plot showing the number of selections
ggplot(candidate_party_counts, aes(x = Candidate, y = Selections, fill = Party.registration)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Rural-Identifying Candidates Selected by Party Registration",
       x = "Candidate",
       y = "Number of Selections",
       fill = "Party Registration") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot as an image
ggsave("candidate_selection_by_party_counts_plot.png", width = 8, height = 5)
# Create the dataset with raw counts (number of selections) and remove NA values
candidate_party_counts <- subset_data %>%
  select(Party.registration, Teacher, Veteran, Mechanic) %>%
  pivot_longer(cols = c(Teacher, Veteran, Mechanic), names_to = "Candidate", values_to = "Selection") %>%
  filter(!is.na(Party.registration) & !is.na(Candidate)) %>%  # Remove NA values
  group_by(Party.registration, Candidate) %>%
  summarise(Selections = sum(Selection, na.rm = TRUE))  # Number of selections
# Create the plot showing the number of selections
ggplot(candidate_party_counts, aes(x = Candidate, y = Selections, fill = Party.registration)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Rural-Identifying Candidates Selected by Party Registration",
       x = "Candidate",
       y = "Number of Selections",
       fill = "Party Registration") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot as an image
ggsave("candidate_selection_by_party_counts_plot.png", width = 8, height = 5)
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Create a tidy data set for candidate favorability
favorability_data <- subset_data %>%
  select(Rural.background, Teacher.favorability, Veteran.favorability, Mechanic.favorability) %>%
  pivot_longer(cols = c(Teacher.favorability, Veteran.favorability, Mechanic.favorability), 
               names_to = "Candidate", 
               values_to = "Favorability") %>%
  filter(!is.na(Rural.background))  # Remove NAs

# Create a bar chart of candidate favorability by rural identity
ggplot(favorability_data, aes(x = Candidate, y = Favorability, fill = factor(Rural.background))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Candidate Favorability by Rural Identity",
       x = "Candidate",
       y = "Favorability Score",
       fill = "Rural Background") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot as an image
ggsave("candidate_favorability_by_rural_identity.png", width = 8, height = 5)
# Load necessary libraries
library(dplyr)
library(ggplot2)

# 'Veteran.favorability', and 'Mechanic.favorability'
# Create a tidy data set for candidate favorability
favorability_data <- subset_data %>%
  select(Rural.background, Teacher.favorability, Veteran.favorability, Mechanic.favorability) %>%
  pivot_longer(cols = c(Teacher.favorability, Veteran.favorability, Mechanic.favorability), 
               names_to = "Candidate", 
               values_to = "Favorability") %>%
  filter(!is.na(Rural.background))  # Remove NAs

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Create a tidy dataset counting the number of selections for each candidate
candidate_party_counts <- subset_data %>%
  select(Party.registration, Teacher, Veteran, Mechanic) %>%
  pivot_longer(cols = c(Teacher, Veteran, Mechanic), names_to = "Candidate", values_to = "Selection") %>%
  filter(!is.na(Party.registration)) %>%
  group_by(Party.registration, Candidate) %>%
  summarise(Selections = sum(Selection, na.rm = TRUE))  # Count the number of selections

# Create the plot showing the number of selections for each candidate by party registration
ggplot(candidate_party_counts, aes(x = Candidate, y = Selections, fill = Party.registration)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Rural-Identifying Candidates Selected by Party Registration",
       x = "Candidate",
       y = "Number of Selections",
       fill = "Party Registration") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot as an image
ggsave("candidate_selection_by_party_counts_plot.png" width = 8, height = 5)

# Logistic regression model for Teacher candidate selection
model_teacher <- glm(Teacher ~ selected_all_populist + Party.registration.numeric + age + education.level.numeric + sex + income.numeric, 
                     data = subset_data, 
                     family = binomial)

# Summary of the model for Teacher selection
summary(model_teacher)

# Logistic regression model for Veteran candidate selection
model_veteran <- glm(Veteran ~ selected_all_populist + Party.registration.numeric + age + education.level.numeric + sex + income.numeric, 
                     data = subset_data, 
                     family = binomial)

# Summary of the model for Veteran selection
summary(model_veteran)

# Logistic regression model for Mechanic candidate selection
model_mechanic <- glm(Mechanic ~ selected_all_populist + Party.registration.numeric + age + education.level.numeric + sex + income.numeric, 
                      data = subset_data, 
                      family = binomial)

# Summary of the model for Mechanic selection
summary(model_mechanic)
# Create a new dataset with different values for populist framing and control variables
populist_vals <- data.frame(selected_all_populist = c(1, 0),  # Populist vs non-populist framing
                            age = mean(subset_data$age, na.rm = TRUE),
                            education.level.numeric = mean(subset_data$education.level.numeric, na.rm = TRUE),
                            income.numeric = mean(subset_data$income.numeric, na.rm = TRUE))

# Get predicted probabilities for Teacher selection from the model
populist_vals$predicted_teacher <- predict(model_teacher, newdata = populist_vals, type = "response")

# Plot the predicted probabilities for Teacher selection by populist framing
ggplot(populist_vals, aes(x = factor(selected_all_populist), y = predicted_teacher, fill = factor(selected_all_populist))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Predicted Probability of Selecting Teacher by Populist Framing",
       x = "Populist Framing",
       y = "Predicted Probability of Selecting Teacher") +
  theme_minimal() +
  scale_x_discrete(labels = c("Non-Populist", "Populist"))
# Create a new dataset with values for populist framing and other control variables
populist_vals <- data.frame(
  selected_all_populist = c(1, 0),  # Populist vs non-populist framing
  age = mean(subset_data$age, na.rm = TRUE),
  education.level.numeric = mean(subset_data$education.level.numeric, na.rm = TRUE),
  income.numeric = mean(subset_data$income.numeric, na.rm = TRUE)
)
# Get predicted probabilities for Teacher selection from the model
populist_vals$predicted_teacher <- predict(model_teacher, newdata = populist_vals, type = "response")
str(subset_data$sex)
# Create a new dataset with values for populist framing and other control variables
populist_vals <- data.frame(
  selected_all_populist = c(1, 0),  # Populist vs non-populist framing
  age = mean(subset_data$age, na.rm = TRUE),
  education.level.numeric = mean(subset_data$education.level.numeric, na.rm = TRUE),
  sex = c(0, 1),  # Male = 0, Female = 1
  income.numeric = mean(subset_data$income.numeric, na.rm = TRUE)
)
# Get predicted probabilities for Teacher selection from the model
populist_vals$predicted_teacher <- predict(model_teacher, newdata = populist_vals, type = "response")
# Plot the predicted probabilities for Teacher selection by populist framing
ggplot(populist_vals, aes(x = factor(selected_all_populist), y = predicted_teacher, fill = factor(selected_all_populist))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Predicted Probability of Selecting Teacher by Populist Framing",
       x = "Populist Framing",
       y = "Predicted Probability of Selecting Teacher") +
  theme_minimal() +
  scale_x_discrete(labels = c("Non-Populist", "Populist"))
# Logistic regression model with interactions (e.g., economic populism * rural background)
model_teacher_interaction <- glm(Teacher ~ selected_all_populist * Party.registration.numeric + 
                                   age + education.level.numeric + sex + income.numeric, 
                                 data = subset_data, 
                                 family = binomial)

# Summary of the model with interaction terms
summary(model_teacher_interaction)
# Create a dataset with values for populist framing and party registration
populist_vals <- data.frame(
  selected_all_populist = c(1, 0),  # Populist vs non-populist framing
  Party.registration.numeric = c(1, 0),  # Republican, Democrat (for example)
  age = mean(subset_data$age, na.rm = TRUE),
  education.level.numeric = mean(subset_data$education.level.numeric, na.rm = TRUE),
  income.numeric = mean(subset_data$income.numeric, na.rm = TRUE)
)

# Create a dataset with values for populist framing and party registration
populist_vals <- data.frame(
  selected_all_populist = c(1, 0),  # Populist vs non-populist framing
  Party.registration.numeric = c(1, 0),  # Republican, Democrat (for example)
  age = mean(subset_data$age, na.rm = TRUE),
  education.level.numeric = mean(subset_data$education.level.numeric, na.rm = TRUE),
  income.numeric = mean(subset_data$income.numeric, na.rm = TRUE)
)

# Get predicted probabilities for Teacher selection from the model
populist_vals$predicted_teacher <- predict(model_teacher, newdata = populist_vals, type = "response")

# Plot interaction effect of populism and party registration on teacher selection
ggplot(populist_vals, aes(x = factor(Party.registration.numeric), y = predicted_teacher, 
                          color = factor(selected_all_populist))) +
  geom_line() +
  labs(title = "Interaction Between Populism and Party Registration on Teacher Selection",
       x = "Party Registration", 
       y = "Predicted Probability of Selecting Teacher", 
       color = "Populist Framing") +
  theme_minimal() +
  scale_x_discrete(labels = c("Republican", "Democrat"))
# Create a dataset with values for populist framing and party registration
populist_vals <- data.frame(
  selected_all_populist = c(1, 0),  # Populist vs non-populist framing
  Party.registration.numeric = c(1, 0),  # Republican, Democrat (for example)
  age = mean(subset_data$age, na.rm = TRUE),
  education.level.numeric = mean(subset_data$education.level.numeric, na.rm = TRUE),
  income.numeric = mean(subset_data$income.numeric, na.rm = TRUE)
)

# Get predicted probabilities for Teacher selection from the model
populist_vals$predicted_teacher <- predict(model_teacher, newdata = populist_vals, type = "response")

# Plot interaction effect of populism and party registration on teacher selection using geom_bar
ggplot(populist_vals, aes(x = factor(Party.registration.numeric), y = predicted_teacher, 
                          fill = factor(selected_all_populist))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Interaction Between Populism and Party Registration on Teacher Selection",
       x = "Party Registration", 
       y = "Predicted Probability of Selecting Teacher", 
       fill = "Populist Framing") +
  theme_minimal() +
  scale_x_discrete(labels = c("Republican", "Democrat"))
# Create a dataset with values for populist framing and party registration for Veteran
populist_vals_veteran <- data.frame(
  selected_all_populist = c(1, 0),  # Populist vs non-populist framing
  Party.registration.numeric = c(1, 0),  # Republican, Democrat (for example)
  age = mean(subset_data$age, na.rm = TRUE),
  education.level.numeric = mean(subset_data$education.level.numeric, na.rm = TRUE),
  income.numeric = mean(subset_data$income.numeric, na.rm = TRUE)
)

# Get predicted probabilities for Veteran selection from the model
populist_vals_veteran$predicted_veteran <- predict(model_veteran, newdata = populist_vals_veteran, type = "response")

# Plot interaction effect of populism and party registration on Veteran selection using geom_bar
ggplot(populist_vals_veteran, aes(x = factor(Party.registration.numeric), y = predicted_veteran, 
                                  fill = factor(selected_all_populist))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Interaction Between Populism and Party Registration on Veteran Selection",
       x = "Party Registration", 
       y = "Predicted Probability of Selecting Veteran", 
       fill = "Populist Framing") +
  theme_minimal() +
  scale_x_discrete(labels = c("Republican", "Democrat"))
# Create a dataset with values for populist framing and party registration for Mechanic
populist_vals_mechanic <- data.frame(
  selected_all_populist = c(1, 0),  # Populist vs non-populist framing
  Party.registration.numeric = c(1, 0),  # Republican, Democrat (for example)
  age = mean(subset_data$age, na.rm = TRUE),
  education.level.numeric = mean(subset_data$education.level.numeric, na.rm = TRUE),
  income.numeric = mean(subset_data$income.numeric, na.rm = TRUE)
)

# Get predicted probabilities for Mechanic selection from the model
populist_vals_mechanic$predicted_mechanic <- predict(model_mechanic, newdata = populist_vals_mechanic, type = "response")

# Plot interaction effect of populism and party registration on Mechanic selection using geom_bar
ggplot(populist_vals_mechanic, aes(x = factor(Party.registration.numeric), y = predicted_mechanic, 
                                   fill = factor(selected_all_populist))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Interaction Between Populism and Party Registration on Mechanic Selection",
       x = "Party Registration", 
       y = "Predicted Probability of Selecting Mechanic", 
       fill = "Populist Framing") +
  theme_minimal() +
  scale_x_discrete(labels = c("Republican", "Democrat"))

# Create a new dataset for candidate selection by populist framing and party registration
candidate_populist_party <- subset_data %>%
  select(Party.registration, selected_all_populist, Teacher, Veteran, Mechanic) %>%
  pivot_longer(cols = c(Teacher, Veteran, Mechanic), names_to = "Candidate", values_to = "Selection") %>%
  group_by(Party.registration, selected_all_populist, Candidate) %>%
  summarise(Selections = sum(Selection, na.rm = TRUE)) %>%
  ungroup()

# Plot a heatmap of selections by party, populist framing, and candidate selection
ggplot(candidate_populist_party, aes(x = Party.registration, y = Candidate, fill = Selections)) +
  geom_tile() +
  facet_wrap(~ selected_all_populist, labeller = as_labeller(c("0" = "Non-Populist", "1" = "Populist"))) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap of Candidate Selection by Party and Populist Framing",
       x = "Party Registration",
       y = "Candidate",
       fill = "Selections") +
  theme_minimal()

# Create a dataset for stacked bar chart
candidate_party_selection <- subset_data %>%
  select(Party.registration, Teacher, Veteran, Mechanic) %>%
  pivot_longer(cols = c(Teacher, Veteran, Mechanic), names_to = "Candidate", values_to = "Selection") %>%
  group_by(Party.registration, Candidate) %>%
  summarise(Selections = sum(Selection, na.rm = TRUE)) %>%
  group_by(Party.registration) %>%
  mutate(Total = sum(Selections)) %>%
  ungroup() %>%
  mutate(Percentage = Selections / Total * 100)

# Plot stacked bar chart showing party registration and selection of candidates
ggplot(candidate_party_selection, aes(x = Candidate, y = Percentage, fill = Party.registration)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Selection of Rural-Identifying Candidates by Party Registration",
       x = "Candidate",
       y = "Percentage of Selections",
       fill = "Party Registration") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
sum(is.na(subset_data$Veteran))

table(subset_data$Veteran, useNA = "ifany")
# Create dataset for heatmap
candidate_rep_strength <- subset_data %>%
  select(Republican.Strength, Teacher, Veteran, Mechanic) %>%
  pivot_longer(cols = c(Teacher, Veteran, Mechanic), names_to = "Candidate", values_to = "Selection") %>%
  group_by(Republican.Strength, Candidate) %>%
  summarise(Selections = sum(Selection, na.rm = TRUE)) %>%
  ungroup()

# Plot heatmap for Republican Strength and Candidate Selection
ggplot(candidate_rep_strength, aes(x = factor(Republican.Strength), y = Candidate, fill = Selections)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap of Candidate Selection by Republican Strength",
       x = "Republican Strength", 
       y = "Candidate", 
       fill = "Selections") +
  theme_minimal() +
  scale_x_discrete(labels = c("Not Very Strong", "Strong"))
# Create a dataset for candidate selection by Republican strength
candidate_rep_strength <- subset_data %>%
  select(Republican.Strength, Teacher, Veteran, Mechanic) %>%
  pivot_longer(cols = c(Teacher, Veteran, Mechanic), names_to = "Candidate", values_to = "Selection") %>%
  group_by(Republican.Strength, Candidate) %>%
  summarise(Selections = sum(Selection, na.rm = TRUE)) %>%
  ungroup()
# Plot heatmap for Republican Strength and Candidate Selection
ggplot(candidate_rep_strength, aes(x = factor(Republican.Strength), y = Candidate, fill = Selections)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap of Candidate Selection by Republican Strength",
       x = "Republican Strength", 
       y = "Candidate", 
       fill = "Selections") +
  theme_minimal() +
  scale_x_discrete(labels = c("Not Very Strong", "Strong"))
# Create a dataset for candidate selection by Republican strength, excluding NAs
candidate_rep_strength <- subset_data %>%
  select(Republican.Strength, Teacher, Veteran, Mechanic) %>%
  pivot_longer(cols = c(Teacher, Veteran, Mechanic), names_to = "Candidate", values_to = "Selection") %>%
  filter(!is.na(Selection)) %>%  # Remove rows where Selection is NA
  group_by(Republican.Strength, Candidate) %>%
  summarise(Selections = sum(Selection, na.rm = TRUE)) %>%
  ungroup()

# Plot heatmap for Republican Strength and Candidate Selection (without NAs)
ggplot(candidate_rep_strength, aes(x = factor(Republican.Strength), y = Candidate, fill = Selections)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap of Candidate Selection by Republican Strength",
       x = "Republican Strength", 
       y = "Candidate", 
       fill = "Selections") +
  theme_minimal() +
  scale_x_discrete(labels = c("Not Very Strong", "Strong"))

# Create a dataset for candidate selection by Republican strength, excluding NAs
candidate_rep_strength <- subset_data %>%
  select(Republican.Strength, Teacher, Veteran, Mechanic) %>%
  pivot_longer(cols = c(Teacher, Veteran, Mechanic), names_to = "Candidate", values_to = "Selection") %>%
  filter(!is.na(Selection)) %>%  # Remove rows where Selection is NA
  group_by(Republican.Strength, Candidate) %>%
  summarise(Selections = sum(Selection, na.rm = TRUE)) %>%
  ungroup()

# Plot heatmap for Republican Strength and Candidate Selection (without NAs)
ggplot(candidate_rep_strength, aes(x = factor(Republican.Strength), y = Candidate, fill = Selections)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap of Candidate Selection by Republican Strength",
       x = "Republican Strength", 
       y = "Candidate", 
       fill = "Selections") +
  theme_minimal() +
  scale_x_discrete(labels = c("Not Very Strong", "Strong"))
table(is.na(subset_data$Teacher))
table(is.na(subset_data$Veteran))
table(is.na(subset_data$Mechanic))
# Pivot the data for candidate selection and remove any rows with NA or zero values
candidate_rep_strength <- subset_data %>%
  select(Republican.Strength, Teacher, Veteran, Mechanic) %>%
  pivot_longer(cols = c(Teacher, Veteran, Mechanic), names_to = "Candidate", values_to = "Selection") %>%
  filter(!is.na(Selection) & Selection != 0) %>%  # Remove rows where Selection is NA or 0
  group_by(Republican.Strength, Candidate) %>%
  summarise(Selections = sum(Selection, na.rm = TRUE)) %>%
  ungroup()

# Plot heatmap for Republican Strength and Candidate Selection (with NAs and zeros removed)
ggplot(candidate_rep_strength, aes(x = factor(Republican.Strength), y = Candidate, fill = Selections)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap of Candidate Selection by Republican Strength",
       x = "Republican Strength", 
       y = "Candidate", 
       fill = "Selections") +
  theme_minimal() +
  scale_x_discrete(labels = c("Not Very Strong", "Strong"))
# Create a dataset for candidate selection by Republican strength, ensuring no NA or zero values
candidate_rep_strength <- subset_data %>%
  select(Republican.Strength, Teacher, Veteran, Mechanic) %>%
  pivot_longer(cols = c(Teacher, Veteran, Mechanic), names_to = "Candidate", values_to = "Selection") %>%
  filter(!is.na(Selection) & Selection != 0) %>%  # Remove NA and zero values
  group_by(Republican.Strength, Candidate) %>%
  summarise(Selections = n(), .groups = "drop")  # Count non-zero selections for each group

# Plot heatmap for Republican Strength and Candidate Selection (with NAs and zeros removed)
ggplot(candidate_rep_strength, aes(x = factor(Republican.Strength), y = Candidate, fill = Selections)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap of Candidate Selection by Republican Strength",
       x = "Republican Strength", 
       y = "Candidate", 
       fill = "Selections") +
  theme_minimal() +
  scale_x_discrete(labels = c("Not Very Strong", "Strong"))
# Create dataset for candidate selection by Republican strength, ensuring no NA or zero values
candidate_rep_strength <- subset_data %>%
  select(Republican.Strength, Teacher, Veteran, Mechanic) %>%
  pivot_longer(cols = c(Teacher, Veteran, Mechanic), names_to = "Candidate", values_to = "Selection") %>%
  filter(!is.na(Selection) & Selection != 0) %>%  # Remove NA and zero values from Selection column
  group_by(Republican.Strength, Candidate) %>%
  summarise(Selections = n(), .groups = "drop")  # Count non-zero selections for each group

head(candidate_rep_strength)
table(candidate_rep_strength$Selections)

# Now, plot heatmap for Republican Strength and Candidate Selection (with NAs and zeros removed)
ggplot(candidate_rep_strength, aes(x = factor(Republican.Strength), y = Candidate, fill = Selections)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap of Candidate Selection by Republican Strength",
       x = "Republican Strength", 
       y = "Candidate", 
       fill = "Selections") +
  theme_minimal() +
  scale_x_discrete(labels = c("Not Very Strong", "Strong"))
# Plot heatmap for Republican Strength and Candidate Selection (with NAs and zeros removed)
ggplot(candidate_rep_strength, aes(x = factor(Republican.Strength), y = Candidate, fill = Selections)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap of Candidate Selection by Republican Strength",
       x = "Republican Strength", 
       y = "Candidate", 
       fill = "Selections") +
  theme_minimal() +
  scale_x_discrete(labels = c("Not Very Strong", "Strong"))

# Create dataset for candidate selection by Republican strength, ensuring no NA or zero values
candidate_rep_strength <- subset_data %>%
  select(Republican.Strength, Teacher, Veteran, Mechanic) %>%
  pivot_longer(cols = c(Teacher, Veteran, Mechanic), names_to = "Candidate", values_to = "Selection") %>%
  filter(!is.na(Selection) & Selection != 0) %>%  # Remove NA and zero values from Selection column
  filter(!is.na(Republican.Strength)) %>%  # Remove NA values from Republican Strength
  group_by(Republican.Strength, Candidate) %>%
  summarise(Selections = n(), .groups = "drop")  # Count non-zero selections for each group

# Plot heatmap for Republican Strength and Candidate Selection (with NAs and zeros removed)
ggplot(candidate_rep_strength, aes(x = factor(Republican.Strength), y = Candidate, fill = Selections)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap of Candidate Selection by Republican Strength",
       x = "Republican Strength", 
       y = "Candidate", 
       fill = "Selections") +
  theme_minimal() +
  scale_x_discrete(labels = c("Not Very Strong", "Strong"))
# Create dataset for candidate selection by Independent Partisan Leaning (Republican vs. Democrat)
candidate_indep_strength <- subset_data %>%
  select(Closer.to.Republican.or.Democrat, Teacher, Veteran, Mechanic) %>%
  pivot_longer(cols = c(Teacher, Veteran, Mechanic), names_to = "Candidate", values_to = "Selection") %>%
  filter(!is.na(Selection) & Selection != 0) %>%  # Remove NA and zero values from Selection column
  filter(!is.na(Closer.to.Republican.or.Democrat)) %>%  # Remove NA values from Independent Leaning
  group_by(Closer.to.Republican.or.Democrat, Candidate) %>%
  summarise(Selections = n(), .groups = "drop")  # Count non-zero selections for each group

# Plot heatmap for Independent Partisan Leaning and Candidate Selection (with NAs and zeros removed)
ggplot(candidate_indep_strength, aes(x = factor(Closer.to.Republican.or.Democrat), y = Candidate, fill = Selections)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap of Candidate Selection by Independent Partisan Leaning",
       x = "Independent Partisan Leaning", 
       y = "Candidate", 
       fill = "Selections") +
  theme_minimal() +
  scale_x_discrete(labels = c("Democrat Leaning", "Republican Leaning"))

names(subset_data)

names(Merged_Data_Cleaned)

library(dplyr)
library(tidyr)

favorability_data <- Merged_Data_Cleaned %>%
  select(Education..1.favorability, Taxes..1.favorability, Ag.2.favorability) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Issue",
    values_to = "Favorability"
  ) %>%
  mutate(Issue = recode(Issue,
                        "Education..1.favorability" = "Education",
                        "Taxes..1.favorability" = "Taxes",
                        "Ag.2.favorability" = "Agriculture"))

library(ggplot2)

ggplot(favorability_data, aes(x = Issue, y = Favorability, fill = Issue)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") + 
  labs(title = "Favorability of Economic Populist Frames",
       x = "Political Issue",
       y = "Average Favorability Score",
       fill = "Issue") +
  theme_minimal()

# Create a data frame for the favorability of populist messages
favorability_data <- subset_data %>%
  select(Education., Taxes..1.favorability, Ag.2.favorability) %>%
  pivot_longer(cols = c(Education..1.favorability, Taxes..1.favorability, Ag.2.favorability), 
               names_to = "Issue", values_to = "Favorability")

# Create bar plot for favorability of populist frames
ggplot(favorability_data, aes(x = Issue, y = Favorability, fill = Issue)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") + 
  labs(title = "Favorability of Economic Populist Frames",
       x = "Political Issue",
       y = "Average Favorability Score",
       fill = "Issue") +
  theme_minimal()
# Create a data frame for the favorability of populist messages using the correct variable names
favorability_data <- subset_data %>%
  select(Education..1.favorability, Taxes..1.favorability, Ag.2.favorability) %>%
  pivot_longer(cols = c(Education..1.favorability, Taxes..1.favorability, Ag.2.favorability), 
               names_to = "Issue", values_to = "Favorability")

# Create bar plot for favorability of populist frames
ggplot(favorability_data, aes(x = Issue, y = Favorability, fill = Issue)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") + 
  labs(title = "Favorability of Economic Populist Frames",
       x = "Political Issue",
       y = "Average Favorability Score",
       fill = "Issue") +
  theme_minimal()
-------------------------------------
  # Create a data frame for the interaction plot with Education favorability and selected populist framing
  interaction_data <- subset_data %>%
  select(Education..1.favorability, selected_all_populist, Party.registration.numeric) %>%
  filter(!is.na(Education..1.favorability))  # Remove rows with NA in favorability

# Create the interaction plot to visualize the effect of populist framing by Party Registration on Education favorability
ggplot(interaction_data, aes(x = factor(selected_all_populist), y = Education..1.favorability, 
                             color = factor(Party.registration.numeric))) +
  geom_point(position = position_jitter(width = 0.1), alpha = 0.7) +  # Use points with slight jitter
  geom_smooth(method = "lm", aes(group = Party.registration.numeric), se = TRUE) +  # Add linear regression lines
  labs(title = "Interaction Between Populist Framing and Party Registration on Education Favorability",
       x = "Populist Framing (0 = Non-populist, 1 = Populist)",
       y = "Education Favorability Score",
       color = "Party Registration") +
  theme_minimal() +
  scale_x_discrete(labels = c("Non-populist", "Populist"))
# Create the interaction data frame
interaction_data <- subset_data %>%
  select(Education..1.favorability, selected_all_populist, Party.registration.numeric) %>%
  filter(!is.na(Education..1.favorability))  # Remove rows with NA in favorability

head(interaction_data)

# Create the interaction plot to visualize the effect of populist framing by Party Registration on Education favorability
ggplot(interaction_data, aes(x = factor(selected_all_populist), y = Education..1.favorability, 
                             color = factor(Party.registration.numeric))) +
  geom_point(position = position_jitter(width = 0.1), alpha = 0.7) +  # Use points with slight jitter
  geom_smooth(method = "lm", aes(group = Party.registration.numeric), se = TRUE) +  # Add linear regression lines
  labs(title = "Interaction Between Populist Framing and Party Registration on Education Favorability",
       x = "Populist Framing (0 = Non-populist, 1 = Populist)",
       y = "Education Favorability Score",
       color = "Party Registration") +
  theme_minimal() +
  scale_x_discrete(labels = c("Non-populist", "Populist"))
# Education Plot (Populist Framing vs. Party Registration)
education_data <- subset_data %>%
  select(Education..1.favorability, selected_all_populist, Party.registration.numeric) %>%
  filter(!is.na(Education..1.favorability))  # Remove rows with NA in favorability

ggplot(education_data, aes(x = factor(selected_all_populist), y = Education..1.favorability, 
                           color = factor(Party.registration.numeric))) +
  geom_point(position = position_jitter(width = 0.1), alpha = 0.7) +  # Points with jitter
  geom_smooth(method = "lm", aes(group = Party.registration.numeric), se = TRUE) +  # Regression lines
  labs(title = "Interaction Between Populist Framing and Party Registration on Education Favorability",
       x = "Populist Framing (0 = Non-populist, 1 = Populist)",
       y = "Education Favorability Score",
       color = "Party Registration") +
  theme_minimal() +
  scale_x_discrete(labels = c("Non-populist", "Populist"))

# Taxes Plot (Populist Framing vs. Party Registration)
taxes_data <- subset_data %>%
  select(Taxes..1.favorability, selected_all_populist, Party.registration.numeric) %>%
  filter(!is.na(Taxes..1.favorability))  # Remove rows with NA in favorability

ggplot(taxes_data, aes(x = factor(selected_all_populist), y = Taxes..1.favorability, 
                       color = factor(Party.registration.numeric))) +
  geom_point(position = position_jitter(width = 0.1), alpha = 0.7) +  # Points with jitter
  geom_smooth(method = "lm", aes(group = Party.registration.numeric), se = TRUE) +  # Regression lines
  labs(title = "Interaction Between Populist Framing and Party Registration on Taxes Favorability",
       x = "Populist Framing (0 = Non-populist, 1 = Populist)",
       y = "Taxes Favorability Score",
       color = "Party Registration") +
  theme_minimal() +
  scale_x_discrete(labels = c("Non-populist", "Populist"))

# Agriculture Plot (Populist Framing vs. Party Registration)
agriculture_data <- subset_data %>%
  select(Ag.2.f
         
# Agriculture Plot (Populist Framing vs. Party Registration)
agriculture_data <- subset_data %>%
 select(Ag.2.favorability, selected_all_populist, Party.registration.numeric) %>%
 filter(!is.na(Ag.2.favorability))  # Remove rows with NA in favorability

# Plot interaction between Populist Framing and Party Registration for Agriculture Favorability
ggplot(agriculture_data, aes(x = factor(selected_all_populist), y = Ag.2.favorability, 
                            color = factor(Party.registration.numeric))) +
 geom_point(position = position_jitter(width = 0.1), alpha = 0.7) +  # Points with jitter
 geom_smooth(method = "lm", aes(group = Party.registration.numeric), se = TRUE) +  # Regression lines
 labs(title = "Interaction Between Populist Framing and Party Registration on Agriculture Favorability",
      x = "Populist Framing (0 = Non-populist, 1 = Populist)",
      y = "Agriculture Favorability Score",
      color = "Party Registration") +
 theme_minimal() +
 scale_x_discrete(labels = c("Non-populist", "Populist"))
# Reshape the data to combine all three issues (Education, Taxes, Agriculture)
combined_data <- subset_data %>%
 select(income.numeric, selected_all_populist, Education..1.favorability, Taxes..1.favorability, Ag.2.favorability) %>%
 gather(key = "Issue", value = "Favorability", Education..1.favorability, Taxes..1.favorability, Ag.2.favorability) %>%
 filter(!is.na(Favorability))  # Remove rows with NA in favorability

# Plot the combined effect of income on economic populist favorability across all issues
ggplot(combined_data, aes(x = income.numeric, y = Favorability, color = factor(Issue))) +
 geom_jitter(aes(shape = factor(selected_all_populist)), alpha = 0.7) +  # Points with populist framing color and shape
 geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, color = "blue") +  # Logistic regression line
 labs(title = "Effect of Income on Economic Populism Favorability Across Issues",
      x = "Income",
      y = "Favorability Score",
      color = "Issue",
      shape = "Populist Framing (0 = Non-populist, 1 = Populist)") +
 theme_minimal() +
 scale_color_manual(values = c("red", "green", "blue"), labels = c("Education", "Taxes", "Agriculture"))

combined_data <- subset_data %>%
  select(income.numeric, selected_all_populist, Education..1.favorability, Taxes..1.favorability, Ag.2.favorability) %>%
  pivot_longer(cols = c(Education..1.favorability, Taxes..1.favorability, Ag.2.favorability), 
               names_to = "Issue", values_to = "Favorability") %>%
  filter(!is.na(Favorability))

combined_data <- Merged_Data_Cleaned %>%
  select(income.numeric, selected_all_populist, 
         Education..1.favorability, Taxes..1.favorability, Ag.2.favorability) %>%
  pivot_longer(
    cols = c(Education..1.favorability, Taxes..1.favorability, Ag.2.favorability),
    names_to = "Issue",
    values_to = "Favorability"
  ) %>%
  filter(!is.na(Favorability))

Merged_Data_Cleaned$selected_all_populist <- ifelse(
  Merged_Data_Cleaned$Education == 1 &
    Merged_Data_Cleaned$Taxes == 1 &
    Merged_Data_Cleaned$Ag == 1, 1, 0)

combined_data <- Merged_Data_Cleaned %>%
  select(income.numeric, selected_all_populist,
         Education..1.favorability, Taxes..1.favorability, Ag.2.favorability) %>%
  pivot_longer(
    cols = c(Education..1.favorability, Taxes..1.favorability, Ag.2.favorability),
    names_to = "Issue",
    values_to = "Favorability"
  ) %>%
  filter(!is.na(Favorability))
model_favorability <- lm(Favorability ~ income.numeric * selected_all_populist + Issue, 
                         data = combined_data)

summary(model_favorability)

library(tidyverse)

favorability_data <- Merged_Data_Cleaned %>%
  select(income.numeric, Education..1.favorability, Taxes..1.favorability, Ag.2.favorability,
         Education, Taxes, Ag) %>%
  mutate(
    Issue_Education = "Education",
    Issue_Taxes = "Taxes",
    Issue_Ag = "Agriculture"
  ) %>%
  pivot_longer(
    cols = c(Education..1.favorability, Taxes..1.favorability, Ag.2.favorability),
    names_to = "Issue",
    values_to = "Favorability"
  ) %>%
  mutate(
    selected_populist = case_when(
      Issue == "Education..1.favorability" ~ Education,
      Issue == "Taxes..1.favorability" ~ Taxes,
      Issue == "Ag.2.favorability" ~ Ag
    ),
    Issue = recode(Issue,
                   "Education..1.favorability" = "Education",
                   "Taxes..1.favorability" = "Taxes",
                   "Ag.2.favorability" = "Agriculture")
  ) %>%
  filter(!is.na(Favorability))

model_favorability <- lm(Favorability ~ income.numeric * selected_populist + Issue, 
                         data = favorability_data)

summary(model_favorability)
library(ggplot2)
library(dplyr)
library(tidyr)

# Step 1: Create the long-format dataframe
favorability_data <- Merged_Data_Cleaned %>%
  select(income.numeric,
         Education..1.favorability,
         Taxes..1.favorability,
         Ag.2.favorability,
         selected_all_populist) %>%
  pivot_longer(cols = c(Education..1.favorability,
                        Taxes..1.favorability,
                        Ag.2.favorability),
               names_to = "Issue",
               values_to = "Favorability") %>%
  filter(!is.na(Favorability))

# Optional: Clean up issue labels for plotting
favorability_data$Issue <- recode(favorability_data$Issue,
                                  "Education..1.favorability" = "Education",
                                  "Taxes..1.favorability" = "Taxes",
                                  "Ag.2.favorability" = "Agriculture")

# Step 2: Create the plot
ggplot(favorability_data, aes(x = income.numeric, y = Favorability,
                              color = Issue, shape = factor(selected_all_populist))) +
  geom_jitter(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "Effect of Income on Economic Populism Favorability Across Issues",
    x = "Income",
    y = "Favorability Score",
    color = "Issue",
    shape = "Populist Framing\n(0 = Non-populist, 1 = Populist)"
  ) +
  scale_color_manual(values = c("red", "green", "blue")) +
  theme_minimal()

library(stargazer)

# Run the linear model (if not already run)
library(tidyverse)

# Create long-format favorability data
favorability_data <- Merged_Data_Cleaned %>%
  select(income.numeric,
         Education..1.favorability, 
         Taxes..1.favorability, 
         Ag.2.favorability,
         selected_all_populist) %>%
  pivot_longer(cols = c(Education..1.favorability, Taxes..1.favorability, Ag.2.favorability),
               names_to = "Issue", values_to = "Favorability") %>%
  filter(!is.na(Favorability))

# Rename for easier labeling in plots/tables
favorability_data$Issue <- recode(favorability_data$Issue,
                                  "Education..1.favorability" = "Education",
                                  "Taxes..1.favorability" = "Taxes",
                                  "Ag.2.favorability" = "Agriculture")

# Create selected_populist variable
favorability_data$selected_populist <- Merged_Data_Cleaned$selected_all_populist[
  match(favorability_data$income.numeric, Merged_Data_Cleaned$income.numeric)
]
# Linear model with interaction
model_populism <- lm(Favorability ~ income.numeric * selected_populist + Issue,
                     data = favorability_data)

# Stargazer regression table
stargazer(model_populism,
          type = "html",
          title = "Linear Regression: Favorability Toward Economic Populist Messages",
          dep.var.labels = "Favorability Score",
          column.labels = "All Issues Combined",
          covariate.labels = c("Income",
                               "Populist Framing",
                               "Issue: Education",
                               "Issue: Taxes",
                               "Income × Populist Framing"),
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.stat = c("f", "ser"),
          notes.align = "l",
          no.space = TRUE,
          out = "Appendix_Table_EconPopulism.html"
)

nobs(model_populism)

library(ggplot2)
library(dplyr)

 theme(axis.text.x = element_text(angle = 45, hjust = 1))
 library(dplyr)
 library(tidyr)
 library(ggplot2)
 
 # Reshaping the dataset to long format
 Merged_Data_long <- Merged_Data %>%
   gather(key = "Reason", value = "Selected", Out_of_touch, Pronouns, Cities, Lack_of_effort) %>%
   group_by(Reason, Party.registration) %>%
   summarise(Count = sum(Selected, na.rm = TRUE)) %>%
   ungroup()

 # Plotting the results as a bar chart
 ggplot(Merged_Data_long, aes(x = Reason, y = Count, fill = Party.registration)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(title = "Reasons for Leaving the Democratic Party by Party Registration",
        x = "Reason",
        y = "Number of Selections",
        fill = "Party Registration") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
 # Create binary variables for each reason based on the presence of the text
 Merged_Data$Out_of_touch <- grepl("The Democratic Party is out of touch with the needs of rural voters", Merged_Data$Why.Dems.fail.ranked, ignore.case = TRUE)
 Merged_Data$Pronouns <- grepl("The Democratic Party is too concerned with identity politics like trans kids in sports and pronouns", Merged_Data$Why.Dems.fail.ranked, ignore.case = TRUE)
 Merged_Data$Cities <- grepl("The Democratic Party only cares about cities", Merged_Data$Why.Dems.fail.ranked, ignore.case = TRUE)
 Merged_Data$Lack_of_effort <- grepl("The Democratic Party does not make an effort to connect with rural voters", Merged_Data$Why.Dems.fail.ranked, ignore.case = TRUE)
 
 head(Merged_Data)
 library(dplyr)
 library(ggplot2)
 
 # Reshaping the dataset to long format for easier plotting
 survey_data_long <- Merged_Data %>%
   gather(key = "Reason", value = "Selected", Out_of_touch, Pronouns, Cities, Lack_of_effort) %>%
   group_by(Reason, Party.registration) %>%
   summarise(Count = sum(Selected, na.rm = TRUE)) %>%
   ungroup()
 
 # Plotting the reasons for leaving the Democratic Party by Party Registration
 ggplot(survey_data_long, aes(x = Reason, y = Count, fill = Party.registration)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(title = "Reasons for Leaving the Democratic Party by Party Registration",
        x = "Reason",
        y = "Number of Selections",
        fill = "Party Registration") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1
                                    library(dplyr)
                                    library(ggplot2)
                                              
# Reshaping the dataset to long format for easier plotting
survey_data_long <- Merged_Data %>%
  gather(key = "Reason", value = "Selected", Out_of_touch, Pronouns, Cities, Lack_of_effort) %>%
  group_by(Reason, Party.registration) %>%
  summarise(Count = sum(Selected, na.rm = TRUE)) %>%
  ungroup()

# Plotting the reasons for the Democratic Party losing support in Coshocton County by Party Registration
ggplot(survey_data_long, aes(x = Reason, y = Count, fill = Party.registration)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Reasons Why the Democratic Party Has Lost Support in Coshocton County by Party Registration",
       x = "Reason",
       y = "Number of Selections",
       fill = "Party Registration") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1
                                   library(dplyr)
                                   library(ggplot2)
                                   
# Reshaping the dataset to long format for easier plotting
survey_data_long <- Merged_Data %>%
gather(key = "Reason", value = "Selected", Out_of_touch, Pronouns, Cities, Lack_of_effort) %>%
filter(!is.na(Selected)) %>%  # Remove NA values
group_by(Reason, Party.registration) %>%
summarise(Count = sum(Selected, na.rm = TRUE)) %>%
ungroup()

# Plotting the reasons for the Democratic Party losing support in Coshocton County by Party Registration
ggplot(survey_data_long, aes(x = Reason, y = Count, fill = Party.registration)) +
geom_bar(stat = "identity", position = "dodge") +
labs(title = "Reasons Why the Democratic Party Has Lost Support in Coshocton County by Party Registration",
x = "Reason",
y = "Number of Selections",
fill = "Party Registration") +
theme_minimal() +
theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Reshaping the dataset to long format for easier plotting, while removing NAs
survey_data_long <- Merged_Data %>%
gather(key = "Reason", value = "Selected", Out_of_touch, Pronouns, Cities, Lack_of_effort) %>%
filter(!is.na(Selected)) %>%  # Remove NA values in the selected reasons
filter(!is.na(Party.registration)) %>%  # Remove NA values in Party.registration
group_by(Reason, Party.registration) %>%
summarise(Count = sum(Selected, na.rm = TRUE)) %>%
ungroup()

# Plotting the reasons for the Democratic Party losing support in Coshocton County by Party Registration
ggplot(survey_data_long, aes(x = Reason, y = Count, fill = Party.registration)) +
geom_bar(stat = "identity", position = "dodge") +
labs(title = "Reasons Why the Democratic Party Has Lost Support in Coshocton County by Party Registration",
x = "Reason",
y = "Number of Selections",
fill = "Party Registration") +
theme_minimal() +
theme(axis.text.x = element_text(angle = 45, hjust = 1)
 library(dplyr)
 # Filter out rows with NA values in Split.ticket.voting or Party.registration
 ggplot(Merged_Data %>% filter(!is.na(Split.ticket.voting), !is.na(Party.registration)),
        aes(x = factor(Split.ticket.voting), fill = Party.registration)) +
   geom_bar(position = "dodge") +
   labs(title = "Likelihood of Split-Ticket Voting by Party Registration",
        x = "Split-Ticket Voting (1 = Yes, 0 = No)", 
        y = "Count", fill = "Party Registration") +
   theme_minimal()
 ggplot(Merged_Data, aes(x = factor(Q6), fill = Party.registration)) +
   geom_bar(position = "dodge") +
   labs(title = "Most Important Issues for Rural Americans by Party Registration",
        x = "Issue", y = "Count", fill = "Party Registration") +
   theme_minimal()
 # Load required package
 library(stringr)
 
 # Split the 'important.issues' column into individual issues
 Merged_Data$split_issues <- str_split(Merged_Data$important.issues, ",")
 
 head(Merged_Data$split_issues)
 library(stringr)
 
 # Split the 'important.issues' column by commas, which creates a list column
 Merged_Data$split_issues <- str_split(Merged_Data$important.issues, ",")

 head(Merged_Data$split_issues)
 Merged_Data$Addiction <- ifelse(grepl("Drug addiction", Merged_Data$Important.issues), 1, 0)
 
 table(Merged_Data$Addiction)
 # Gather the selected issue variables into long format
 issues_long <- Merged_Data %>%
   gather(key = "Issue", value = "Selected", Broadband, Public_education, Addiction, Healthcare, 
          Rural_development, Wealth_inequality, Unions, Ag_investments) %>%
   filter(!is.na(Selected))  # Remove NA values if any
 
 # View the structure of the data to ensure it looks correct
 head(issues_long)
 # Bar chart showing the number of selections for each issue
 ggplot(issues_long, aes(x = Issue, fill = factor(Selected))) +
   geom_bar(stat = "count") +
   labs(title = "Distribution of Selected Issues by Respondents",
        x = "Issue",
        y = "Number of Selections",
        fill = "Selected (1 = Yes, 0 = No)") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
 # Boxplot to show the distribution of Political Ideology by Party Registration
 ggplot(Merged_Data, aes(x = Party.registration, y = Political.ideology, fill = Party.registration)) +
   geom_boxplot() +
   labs(title = "Distribution of Political Ideology by Party Registration",
        x = "Party Registration",
        y = "Political Ideology") +
   theme_minimal() +
   scale_x_discrete(labels = c("Republican", "Democrat")) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 # Scatter plot to visualize the relationship between Party Registration and Political Ideology
 ggplot(Merged_Data, aes(x = Party.registration, y = Political.scale)) +
   geom_point(aes(color = Party.registration), alpha = 0.7) +
   labs(title = "Relationship Between Party Registration and Political Ideology",
        x = "Party Registration",
        y = "Political Ideology",
        color = "Party Registration") +
   theme_minimal() +
   scale_x_discrete(labels = c("Republican", "Democrat")) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 # Scatter plot to visualize the relationship between Party Registration and Political Ideology
 ggplot(Merged_Data, aes(x = Party.registration, y = Political.scale)) +
   geom_point(aes(color = Party.registration), alpha = 0.7) +
   labs(title = "Relationship Between Party Registration and Political Ideology",
        x = "Party Registration",
        y = "Political Ideology",
        color = "Party Registration") +
   theme_minimal() +
   scale_x_discrete(labels = c("Republican", "Democrat")) +
--------------------###subsetting working.class.focus                                                                                           

Merged_Data_Cleaned <- Merged_Data_Cleaned %>%
  mutate(Working.Class.Candidate = case_when(
    Teacher == 1 ~ 1,
    Veteran == 1 ~ 1,
    Mechanic == 1 ~ 1,
    TRUE ~ 0
  ))
 Merged_Data_Cleaned <- Merged_Data_Cleaned %>%
   mutate(Working.Class.Candidate = case_when(
     Teacher == 1 ~ 1,      # If Teacher is selected, assign 1 (working-class candidate)
     Veteran == 1 ~ 1,      # If Veteran is selected, assign 1 (working-class candidate)
     Mechanic == 1 ~ 1,     # If Mechanic is selected, assign 1 (working-class candidate)
     TRUE ~ 0               # Default to 0 if none of the conditions are met
   ))
 # Create a new variable for Rural.Candidate
 Merged_Data_Cleaned <- Merged_Data_Cleaned %>%
   mutate(Rural.Candidate = case_when(
     Teacher == 1 ~ 1,  # If Teacher is selected, assign 1 (rural candidate)
     Veteran == 1 ~ 1,   # If Veteran is selected, assign 1 (rural candidate)
     TRUE ~ 0            # Default to 0 if neither Teacher nor Veteran
   ))
 
 Merged_Data_Clean$Rural_Party_Interaction <- interaction(Merged_Data_Clean$Rural.background, Merged_Data_Clean$Party.registration)
 # Interaction term between Rural background and Party registration
 Merged_Data_Cleaned$Rural_Party_Interaction <- interaction(Merged_Data_Cleaned$Rural.background, Merged_Data_Cleaned$Party.registration)
 
 model_rural_party <- glm(Voted.for.Dem ~ Rural_Party_Interaction + age + income + Education.Level + Race + sex, 
                          data = Merged_Data_Cleaned, family = binomial)
 
 summary(model_rural_party)
 library(car)
 # Create new variables to capture combinations of selected rural candidates
 Merged_Data_Cleaned <- Merged_Data_Cleaned %>%
   mutate(
     # Variable indicating if all three rural candidates were selected (Teacher, Veteran, Mechanic)
     All_Rural_Candidates_Selected = ifelse(Teacher == 1 & Veteran == 1 & Mechanic == 1, 1, 0),
     
     # Variable indicating if Teacher and Veteran were selected
     Teacher_Veteran_Selected = ifelse(Teacher == 1 & Veteran == 1 & Mechanic == 0, 1, 0),
     
     # Variable indicating if Teacher and Mechanic were selected
     Teacher_Mechanic_Selected = ifelse(Teacher == 1 & Mechanic == 1 & Veteran == 0, 1, 0),
     
     # Variable indicating if Veteran and Mechanic were selected
     Veteran_Mechanic_Selected = ifelse(Veteran == 1 & Mechanic == 1 & Teacher == 0, 1, 0),
     
     # Variable indicating if only Teacher was selected
     Only_Teacher_Selected = ifelse(Teacher == 1 & Veteran == 0 & Mechanic == 0, 1, 0),
     
     # Variable indicating if only Veteran was selected
     Only_Veteran_Selected = ifelse(Veteran == 1 & Teacher == 0 & Mechanic == 0, 1, 0),
     
     # Variable indicating if only Mechanic was selected
     Only_Mechanic_Selected = ifelse(Mechanic == 1 & Teacher == 0 & Veteran == 0, 1, 0)
   )
 
 library(stargazer)
 
 stargazer(model_teacher, model_veteran, model_mechanic,
           type = "html",  # Change to "latex" if you need LaTeX for a PDF thesis
           title = "Logistic Regression Models Predicting Candidate Choice by Political Ideology and Demographics",
           column.labels = c("Teacher", "Veteran", "Mechanic"),
           covariate.labels = c("Political Ideology", "Age", "Income", "Education Level", "Race", "Sex"),
           dep.var.labels = "Candidate Selection (1 = Chosen)",
           omit.stat = c("ll", "aic"),  # Optional: remove log likelihood/AIC if you want it cleaner
           no.space = TRUE,
           digits = 3,
           star.cutoffs = c(0.05, 0.01, 0.001),
           notes.align = "l",
           out = "candidate_models.html"  # Saves to your working directory
 )
 
 levels(Merged_Data_Cleaned$income)
 
 Merged_Data_Cleaned$income <- factor(Merged_Data_Cleaned$income)
 
 levels(Merged_Data_Cleaned$income)
 
 library(dplyr)
 library(ggplot2)
 library(broom)
 library(ggeffects)
 install(ggeff)
 library(ggeffects)
 
 Merged_Data_Cleaned$income <- relevel(Merged_Data_Cleaned$income, ref = "Less than $10,000")
 
 model_teacher <- glm(Teacher ~ Political.scale + age + income + Education.Level + Race + sex, 
                      data = Merged_Data_Cleaned, family = binomial)
 
 summary(model_teacher)
 model_veteran <- glm(Veteran ~ Political.scale + age + income + Education.Level + Race + sex, 
                      data = Merged_Data_Cleaned, family = binomial)
 
 summary(model_veteran)
 model_mechanic <- glm(Mechanic ~ Political.scale + age + income + Education.Level + Race + sex, 
                       data = Merged_Data_Cleaned, family = binomial)
 
 summary(model_mechanic)
 # Generate predicted probabilities using ggeffects
 pred_teacher <- ggpredict(model_teacher, terms = "Political.scale")
 pred_veteran <- ggpredict(model_veteran, terms = "Political.scale")
 pred_mechanic <- ggpredict(model_mechanic, terms = "Political.scale")
 
 # Add a variable to identify the candidate type
 pred_teacher$Candidate <- "Teacher"
 pred_veteran$Candidate <- "Veteran"
 pred_mechanic$Candidate <- "Mechanic"
 
 # Combine the data frames
 pred_all <- bind_rows(pred_teacher, pred_veteran, pred_mechanic)
 
 library(stargazer)
 
 stargazer(model_teacher, model_veteran, model_mechanic,
           type = "html",  # change to "latex" if you're exporting to PDF
           title = "Logistic Regression Models Predicting Candidate Selection",
           column.labels = c("Teacher", "Veteran", "Mechanic"),
           covariate.labels = c(
             "Political Ideology", "Age",
             "Income: $10,000–19,999", "Income: $20,000–29,999", "Income: $30,000–39,999",
             "Income: $40,000–49,999", "Income: $50,000–59,999", "Income: $60,000–69,999",
             "Income: $70,000–79,999", "Income: $80,000–89,999", "Income: $90,000–99,999",
             "Income: $100,000–149,999", "Income: $150,000 or more",
             "Education: Some College", "Education: Associate Degree", 
             "Education: Bachelor's", "Education: Master's", "Education: Professional", 
             "Education: Doctorate", 
             "Race: Non-White", "Sex: Female"
           ),
           dep.var.labels = "Candidate Selection (1 = Chosen)",
           omit.stat = c("ll", "aic"),
           no.space = TRUE,
           digits = 3,
           star.cutoffs = c(0.05, 0.01, 0.001),
           notes.align = "l",
           out = "Candidate_Models_Table.html"
 )
 
 # Plot
 ggplot(pred_all, aes(x = x, y = predicted, color = Candidate)) +
   geom_line(size = 1.2) +
   geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Candidate), alpha = 0.2, color = NA) +
   labs(title = "Predicted Probability of Selecting Each Candidate by Political Ideology",
        x = "Political Ideology (0 = Liberal, 7 = Conservative)",
        y = "Predicted Probability of Candidate Selection") +
   theme_minimal() +
   theme(legend.position = "top")
 
 # Create interaction variable
 Merged_Data_Cleaned$Rural_Party_Interaction <- interaction(Merged_Data_Cleaned$Rural.background,
                                                            Merged_Data_Cleaned$Party.registration)
 
 # Logistic regression: Does rural identity matter more for some party registrations?
                          data = Merged_Data_Cleaned, family = binomial)
 
 # View summary
 summary(model_rural_party)
 # Logistic regression with interaction: Political Ideology × Education Level
 model_teacher_edu <- glm(Teacher ~ Political.scale * Education.Level, 
                          data = Merged_Data_Cleaned, 
                          family = binomial)
 
 # View model results
 summary(model_teacher_edu)
 library(ggeffects)
 library(ggplot2)
 
 # Get predicted probabilities
 plot_teacher_edu <- ggpredict(model_teacher_edu, terms = c("Political.scale", "Education.Level"))
 
 # Plot
 ggplot(plot_teacher_edu, aes(x = x, y = predicted, color = group)) +
   geom_line(size = 1.2) +
   geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
   labs(
     title = "Interaction: Political Ideology × Education Level",
     subtitle = "Predicted Probability of Selecting the Teacher Candidate",
     x = "Political Ideology (0 = Liberal, 7 = Conservative)",
     y = "Predicted Probability",
     color = "Education Level",
     fill = "Education Level"
   ) +
   theme_minimal()
 # Create a new simplified education variable
 Merged_Data_Cleaned$Edu_Collapsed <- case_when(
   Merged_Data_Cleaned$Education.Level %in% c("Less than high school degree", "High school graduate (high school diploma or equivalent including GED)") ~ "No College",
   Merged_Data_Cleaned$Education.Level %in% c("Some college but no degree", "Associate degree in college (2-year)") ~ "Some College",
   Merged_Data_Cleaned$Education.Level %in% c("Bachelor's degree in college (4-year)") ~ "Bachelor's",
   Merged_Data_Cleaned$Education.Level %in% c("Master's degree", "Professional degree (JD, MD)", "Doctoral degree") ~ "Grad Degree",
   TRUE ~ NA_character_
 )
 # New model with collapsed education variable
 model_teacher_edu_simple <- glm(Teacher ~ Political.scale * Edu_Collapsed, 
                                 data = Merged_Data_Cleaned, 
                                 family = binomial)

 summary(model
         ) library(ggeffects)
 library(ggplot2)
 
 plot_simple <- ggpredict(model_teacher_edu_simple, terms = c("Political.scale", "Edu_Collapsed"))
 
 ggplot(plot_simple, aes(x = x, y = predicted, color = group)) +
   geom_line(size = 1.2) +
   geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
   labs(
     title = "Political Ideology × Education Level",
     subtitle = "Predicted Probability of Selecting the Teacher Candidate",
     x = "Political Ideology (0 = Liberal, 7 = Conservative)",
     y = "Predicted Probability",
     color = "Education Group",
     fill = "Education Group"
   ) +
   theme_minimal()
 # Logistic regression with interaction term
 model_veteran_edu <- glm(Veteran ~ Political.scale * Education.Level, 
                          data = Merged_Data_Cleaned, family = binomial)
 
 # Predicted probabilities
 library(ggeffects)
 preds_veteran <- ggpredict(model_veteran_edu, terms = c("Political.scale", "Education.Level"))
 
 # Plot
 library(ggplot2)
 plot(preds_veteran) +
   labs(
     title = "Political Ideology × Education Level",
     subtitle = "Predicted Probability of Selecting the Veteran Candidate",
     x = "Political Ideology (0 = Liberal, 7 = Conservative)",
     y = "Predicted Probability"
   ) +
   theme_minimal()
 Merged_Data_Cleaned$Education.Group <- case_when(
   Merged_Data_Cleaned$Education.Level %in% c("Less than high school degree", 
                                              "High school graduate (high school diploma or equivalent including GED)") ~ "No College",
   Merged_Data_Cleaned$Education.Level %in% c("Some college but no degree", 
                                              "Associate degree in college (2-year)") ~ "Some College",
   Merged_Data_Cleaned$Education.Level == "Bachelor's degree in college (4-year)" ~ "Bachelor's",
   Merged_Data_Cleaned$Education.Level %in% c("Master's degree", "Doctoral degree", 
                                              "Professional degree (JD, MD)") ~ "Grad Degree"
 )
 
 Merged_Data_Cleaned$Education.Group <- case_when(
   Merged_Data_Cleaned$Education.Level %in% c("Less than high school degree", 
                                              "High school graduate (high school diploma or equivalent including GED)") ~ "No College",
   Merged_Data_Cleaned$Education.Level %in% c("Some college but no degree", 
                                              "Associate degree in college (2-year)") ~ "Some College",
   Merged_Data_Cleaned$Education.Level == "Bachelor's degree in college (4-year)" ~ "Bachelor's",
   Merged_Data_Cleaned$Education.Level %in% c("Master's degree", "Doctoral degree", 
                                              "Professional degree (JD, MD)") ~ "Grad Degree"
 )
 
 # Factor with levels in logical order
 Merged_Data_Cleaned$Education.Group <- factor(Merged_Data_Cleaned$Education.Group,
                                               levels = c("No College", "Some College", "Bachelor's", "Grad Degree"))
 library(ggeffects)
 model_veteran_grouped <- glm(Veteran ~ Political.scale * Education.Group, 
                              data = Merged_Data_Cleaned, family = binomial)
 preds_vet_grouped <- ggpredict(model_veteran_grouped, terms = c("Political.scale", "Education.Group"))
 library(ggplot2)
 
 plot(preds_vet_grouped) +
   labs(
     title = "Political Ideology × Education Level",
     subtitle = "Predicted Probability of Selecting the Veteran Candidate",
     x = "Political Ideology (0 = Liberal, 7 = Conservative)",
     y = "Predicted Probability",
     color = "Education Group"
   ) +
   theme_minimal()
 # Run the logistic regression with interaction
 model_mechanic_grouped <- glm(Mechanic ~ Political.scale * Education.Group, 
                               data = Merged_Data_Cleaned, 
                               family = binomial)
 
 # Load the ggeffects package if not already loaded
 library(ggeffects)
 
 # Generate predicted probabilities from the interaction model
 preds_mech_grouped <- ggpredict(model_mechanic_grouped, terms = c("Political.scale", "Education.Group"))
 
 # Plot the results
 library(ggplot2)
 plot(preds_mech_grouped) +
   labs(
     title = "Political Ideology × Education Level",
     subtitle = "Predicted Probability of Selecting the Mechanic Candidate",
     x = "Political Ideology (0 = Liberal, 7 = Conservative)",
     y = "Predicted Probability"
   ) +
   theme_minimal() +
   scale_y_continuous(labels = scales::percent_format()) +
   scale_color_manual(values = c("red", "green", "cyan", "purple")) +
   scale_fill_manual(values = c("red", "green", "cyan", "purple")) +
   labs(color = "Education Group", fill = "Education Group")
 Merged_Data_Cleaned$Income.Group <- case_when(
   Merged_Data_Cleaned$income %in% c("Less than $10,000", "$10,000 to $19,999", "$20,000 to $29,999") ~ "Low Income",
   Merged_Data_Cleaned$income %in% c("$30,000 to $39,999", "$40,000 to $49,999", "$50,000 to $59,999") ~ "Lower-Middle Income",
   Merged_Data_Cleaned$income %in% c("$60,000 to $69,999", "$70,000 to $79,999", "$80,000 to $89,999") ~ "Upper-Middle Income",
   Merged_Data_Cleaned$income %in% c("$90,000 to $99,999", "$100,000 to $149,999", "$150,000 or more") ~ "High Income",
   TRUE ~ NA_character_
 )
 
 Merged_Data_Cleaned$Education.Group <- case_when(
   Merged_Data_Cleaned$Education.Level %in% c("Less than high school degree", 
                                              "High school graduate (high school diploma or equivalent including GED)") ~ "No College",
   Merged_Data_Cleaned$Education.Level %in% c("Some college but no degree", 
                                              "Associate degree in college (2-year)") ~ "Some College",
   Merged_Data_Cleaned$Education.Level == "Bachelor's degree in college (4-year)" ~ "Bachelor's",
   Merged_Data_Cleaned$Education.Level %in% c("Master's degree", "Doctoral degree", 
                                              "Professional degree (JD, MD)") ~ "Grad Degree"
 )
 
 Merged_Data_Cleaned$Education.Group <- factor(
   Merged_Data_Cleaned$Education.Group,
   levels = c("No College", "Some College", "Bachelor's", "Grad Degree")
 )
 
 model_teacher_grouped <- glm(Teacher ~ Political.scale * Education.Group, 
                              data = Merged_Data_Cleaned, family = binomial)
 
                              data = Merged_Data_Cleaned, family = binomial)
 
                               data = Merged_Data_Cleaned, family = binomial)
 
 preds_teacher <- ggpredict(model_teacher_grouped, terms = c("Political.scale", "Education.Group"))
 preds_veteran <- ggpredict(model_veteran_grouped, terms = c("Political.scale", "Education.Group"))
 preds_mechanic <- ggpredict(model_mechanic_grouped, terms = c("Political.scale", "Education.Group"))
 
 model_teacher_grouped
 model_veteran_grouped
 model_mechanic_grouped
 
 library(ggeffects)
 library(ggplot2)
 
 # Predicted probabilities
 preds_teacher <- ggpredict(model_teacher_grouped, terms = c("Political.scale", "Education.Group"))
 preds_veteran <- ggpredict(model_veteran_grouped, terms = c("Political.scale", "Education.Group"))
 preds_mechanic <- ggpredict(model_mechanic_grouped, terms = c("Political.scale", "Education.Group"))
 
 # Add candidate type label
 preds_teacher$Candidate <- "Teacher"
 preds_veteran$Candidate <- "Veteran"
 preds_mechanic$Candidate <- "Mechanic"
 
 # Combine all predictions
 preds_all <- rbind(preds_teacher, preds_veteran, preds_mechanic)
 
 ggplot(preds_all, aes(x = x, y = predicted, color = group)) +
   geom_line(size = 1.2) +
   geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
   facet_wrap(~Candidate) +
   labs(
     title = "Predicted Probability of Selecting Candidate",
     subtitle = "By Political Ideology and Education Level",
     x = "Political Ideology (0 = Liberal, 7 = Conservative)",
     y = "Predicted Probability",
     color = "Education Level",
     fill = "Education Level"
   ) +
   theme_minimal()
 library(stargazer)
 
 stargazer(model_teacher_grouped, model_veteran_grouped, model_mechanic_grouped,
           type = "html",  # Use "latex" if you're writing a LaTeX/PDF thesis
           title = "Logistic Regression: Candidate Selection by Political Ideology × Education Level",
           column.labels = c("Teacher", "Veteran", "Mechanic"),
           covariate.labels = c(
             "Political Ideology",
             "Some College", "Bachelor's", "Grad Degree",
             "Ideology × Some College", "Ideology × Bachelor's", "Ideology × Grad Degree"
           ),
           dep.var.labels = "Candidate Selected (1 = Yes)",
           omit.stat = c("ll", "aic"),
           digits = 3,
           star.cutoffs = c(0.05, 0.01, 0.001),
           notes.align = "l",
           no.space = TRUE,
           out = "Appendix_Table_Interaction.html"
 )
 
 model_teacher_income <- glm(Teacher ~ Political.scale * Income.Group, 
                             data = Merged_Data_Cleaned, family = binomial)
 
 model_veteran_income <- glm(Veteran ~ Political.scale * Income.Group, 
                             data = Merged_Data_Cleaned, family = binomial)
 
 model_mechanic_income <- glm(Mechanic ~ Political.scale * Income.Group, 
                              data = Merged_Data_Cleaned, family = binomial)
 
 library(ggeffects)
 
 pred_teacher_income <- ggpredict(model_teacher_income, terms = c("Political.scale", "Income.Group"))
 pred_veteran_income <- ggpredict(model_veteran_income, terms = c("Political.scale", "Income.Group"))
 pred_mechanic_income <- ggpredict(model_mechanic_income, terms = c("Political.scale", "Income.Group"))
 
 # Add label for candidate
 pred_teacher_income$Candidate <- "Teacher"
 pred_veteran_income$Candidate <- "Veteran"
 pred_mechanic_income$Candidate <- "Mechanic"
 
 # Combine all
 pred_all_income <- rbind(pred_teacher_income, pred_veteran_income, pred_mechanic_income)
 
 table(Merged_Data_Cleaned$Income.Group, Merged_Data_Cleaned$Mechanic)
 table(Merged_Data_Cleaned$Income.Group, Merged_Data_Cleaned$Teacher)
 table(Merged_Data_Cleaned$Income.Group, Merged_Data_Cleaned$Veteran)
 
 summary(pred_all_income)
 library(ggplot2)
 
 ggplot(pred_all_income, aes(x = x, y = predicted, color = group)) +
   geom_line(size = 1.2) +
   geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
   facet_wrap(~Candidate) +
   labs(
     title = "Predicted Probability of Selecting Candidate",
     subtitle = "By Political Ideology and Income Group",
     x = "Political Ideology (0 = Liberal, 7 = Conservative)",
     y = "Predicted Probability",
     color = "Income Group",
     fill = "Income Group"
   ) +
   theme_minimal()
 
 Merged_Data_Cleaned$Income.Group <- factor(Merged_Data_Cleaned$Income.Group)
                             data = Merged_Data_Cleaned, family = binomial)
 
 library(ggeffects)
 preds_teacher_income <- ggpredict(model_teacher_income, terms = c("Political.scale", "Income.Group"))
 summary(model_teacher_income)
 library(ggplot2)
 plot(preds_teacher_income) +
   labs(title = "Political Ideology × Income Level",
        subtitle = "Predicted Probability of Selecting the Teacher Candidate",
        x = "Political Ideology (0 = Liberal, 7 = Conservative)",
        y = "Predicted Probability",
        color = "Income Group", fill = "Income Group") +
   theme_minimal()
                             data = Merged_Data_Cleaned, family = binomial)
 
 preds_veteran_income <- ggpredict(model_veteran_income, terms = c("Political.scale", "Income.Group"))
 summary(model_veteran_income)
 summary(preds_veteran_income)
 plot(preds_veteran_income) +
   labs(title = "Political Ideology × Income Level",
        subtitle = "Predicted Probability of Selecting the Veteran Candidate",
        x = "Political Ideology (0 = Liberal, 7 = Conservative)",
        y = "Predicted Probability",
        color = "Income Group", fill = "Income Group") +
   theme_minimal()
                              data = Merged_Data_Cleaned, family = binomial)
 summary(model_mechanic_income)
 preds_mechanic_income <- ggpredict(model_mechanic_income, terms = c("Political.scale", "Income.Group"))
 
 plot(preds_mechanic_income) +
   labs(title = "Political Ideology × Income Level",
        subtitle = "Predicted Probability of Selecting the Mechanic Candidate",
        x = "Political Ideology (0 = Liberal, 7 = Conservative)",
        y = "Predicted Probability",
        color = "Income Group", fill = "Income Group") +
   theme_minimal()
 # Education
 t.test(Merged_Data_Cleaned$Education..1.favorability,
        Merged_Data_Cleaned$Education..2.favorability,
        paired = TRUE)
 
 # Taxes
 t.test(Merged_Data_Cleaned$Taxes..1.favorability,
        Merged_Data_Cleaned$Taxes..2.favorability,
        paired = TRUE)
 
 # Agriculture
 t.test(Merged_Data_Cleaned$Ag.2.favorability,
        Merged_Data_Cleaned$Ag.1.favorability,
        paired = TRUE)
 # Linear regression predicting favorability of economic populist education message
 model_edu_populist <- lm(Education..1.favorability ~ Political.scale + Working.class.focus + 
                            income + Education.Level + Race + sex,
                          data = Merged_Data_Cleaned)
 
 summary(model_edu_populist)
 # Load ggeffects if you haven't already
 library(ggeffects)
 
 # Generate predictions across political ideology
 preds_edu_populist <- ggpredict(model_edu_populist, terms = "Political.scale")
 
 # Plot
 plot(preds_edu_populist) +
   labs(
     title = "Predicted Favorability of Economic Populist Education Message",
     subtitle = "by Political Ideology",
     x = "Political Ideology (0 = Liberal, 7 = Conservative)",
     y = "Predicted Favorability (1 = Strongly Disagreeable, 5 = Strongly Agreeable)"
   ) +
   theme_minimal()
 # Run linear model with interaction
 model_edu_income <- lm(Education..1.favorability ~ Political.scale * Income.Group + 
                          Education.Level + Race + sex,
                        data = Merged_Data_Cleaned)
 
 # Load ggeffects and get predicted values
 library(ggeffects)
 preds_edu_income <- ggpredict(model_edu_income, terms = c("Political.scale", "Income.Group"))
 
 # Plot
 library(ggplot2)
 plot(preds_edu_income) +
   labs(
     title = "Political Ideology × Income Group",
     subtitle = "Predicted Favorability of Economic Populist Education Message",
     x = "Political Ideology (0 = Liberal, 7 = Conservative)",
     y = "Predicted Favorability (1–5 Scale)",
     color = "Income Group"
   ) +
   theme_minimal()

  Merged_Data_Cleaned$Income.Group <- factor(
   Merged_Data_Cleaned$Income.Group,
   levels = c("High Income", "Upper-Middle Income", "Lower-Middle Income", "Low Income")
 )
 # Run linear model
 model_taxes_income <- lm(Taxes..1.favorability ~ Political.scale * Income.Group +
                            Education.Level + Race + sex,
                          data = Merged_Data_Cleaned)
 
 # Predictions
 preds_taxes_income <- ggpredict(model_taxes_income, terms = c("Political.scale", "Income.Group"))
 
 # Plot
 plot(preds_taxes_income) +
   labs(
     title = "Political Ideology × Income Group",
     subtitle = "Predicted Favorability of Economic Populist Taxes Message",
     x = "Political Ideology (0 = Liberal, 7 = Conservative)",
     y = "Predicted Favorability (1–5 Scale)",
     color = "Income Group"
   ) +
   theme_minimal()
 # Run linear model
 model_ag_income <- lm(Ag.2.favorability ~ Political.scale * Income.Group +
                         Education.Level + Race + sex,
                       data = Merged_Data_Cleaned)
 
 # Predictions
 preds_ag_income <- ggpredict(model_ag_income, terms = c("Political.scale", "Income.Group"))
 
 # Plot
 plot(preds_ag_income) +
   labs(
     title = "Political Ideology × Income Group",
     subtitle = "Predicted Favorability of Economic Populist Agriculture Message",
     x = "Political Ideology (0 = Liberal, 7 = Conservative)",
     y = "Predicted Favorability (1–5 Scale)",
     color = "Income Group"
   ) +
   theme_minimal()
 # Reorder Income.Group levels (High to Low)
 Merged_Data_Cleaned$Income.Group <- factor(Merged_Data_Cleaned$Income.Group,
                                            levels = c("High Income", 
                                                       "Upper-Middle Income", 
                                                       "Lower-Middle Income", 
                                                       "Low Income"))
 
 # Model: Political ideology × Income group for favorability of education populist message
 model_edu_income <- lm(Education..1.favorability ~ Political.scale * Income.Group,
                        data = Merged_Data_Cleaned)
 
 # Predicted values
 library(ggeffects)
 preds_edu_income <- ggpredict(model_edu_income, terms = c("Political.scale", "Income.Group"))
 
 # Plot
 library(ggplot2)
 ggplot(preds_edu_income, aes(x = x, y = predicted, color = group, fill = group)) +
   geom_line(size = 1.2) +
   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
   labs(title = "Political Ideology × Income Group",
        subtitle = "Predicted Favorability of Economic Populist Education Message",
        x = "Political Ideology (0 = Liberal, 7 = Conservative)",
        y = "Predicted Favorability (1–5 Scale)",
        color = "Income Group",
        fill = "Income Group") +
   theme_minimal() +
   theme(legend.position = "right")
 Merged_Data_Cleaned$Income.Group <- factor(
   Merged_Data_Cleaned$Income.Group,
   levels = c("High Income", "Upper-Middle Income", "Lower-Middle Income", "Low Income")
 )
 
 # Models for each issue
                          Education.Level + Race + sex,
                        data = Merged_Data_Cleaned)
 
                            Education.Level + Race + sex,
                          data = Merged_Data_Cleaned)
 
                         Education.Level + Race + sex,
                       data = Merged_Data_Cleaned)
 
 library(ggeffects)
 model_edu_income
 model_taxes_income
 model_ag_income
 
 preds_edu <- ggpredict(model_edu_income, terms = c("Political.scale", "Income.Group"))
 preds_edu$Issue <- "Education"
 
 preds_taxes <- ggpredict(model_taxes_income, terms = c("Political.scale", "Income.Group"))
 preds_taxes$Issue <- "Taxes"
 
 preds_ag <- ggpredict(model_ag_income, terms = c("Political.scale", "Income.Group"))
 preds_ag$Issue <- "Agriculture"
 
 # Combine all predictions
 preds_all <- rbind(preds_edu, preds_taxes, preds_ag)
 
 library(ggeffects)
 
 # Education Message
 preds_edu <- ggpredict(model_edu_income, terms = c("Political.scale", "Income.Group"))
 preds_edu$Issue <- "Education"
 
 # Taxes Message
 preds_taxes <- ggpredict(model_taxes_income, terms = c("Political.scale", "Income.Group"))
 preds_taxes$Issue <- "Taxes"
 
 # Agriculture Message
 preds_ag <- ggpredict(model_ag_income, terms = c("Political.scale", "Income.Group"))
 preds_ag$Issue <- "Agriculture"
 
 preds_all <- rbind(preds_edu, preds_taxes, preds_ag)
 
 library(ggplot2)
 
 ggplot(preds_all, aes(x = x, y = predicted, color = group, fill = group)) +
   geom_line(size = 1.2) +
   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
   facet_wrap(~Issue) +
   labs(
     title = "Political Ideology × Income Group",
     subtitle = "Predicted Favorability of Economic Populist Messages by Issue",
     x = "Political Ideology (0 = Liberal, 7 = Conservative)",
     y = "Predicted Favorability (1–5 Scale)",
     color = "Income Group",
     fill = "Income Group"
   ) +
   theme_minimal() +
   theme(legend.position = "bottom")
 
                              data = Merged_Data_Cleaned, family = binomial)
 
 library(ggeffects)
 preds_mech_income <- ggpredict(model_mechanic_income, terms = c("Political.scale", "Income.Group"))
 
 library(ggplot2)
 plot(preds_mech_income) +
   labs(
     title = "Political Ideology × Income Level",
     subtitle = "Predicted Probability of Selecting the Mechanic Candidate",
     x = "Political Ideology (0 = Liberal, 7 = Conservative)",
     y = "Predicted Probability",
     color = "Income Group",
     fill = "Income Group"
   ) +
   theme_minimal()
                             data = Merged_Data_Cleaned, family = binomial)
 
 preds_vet_income <- ggpredict(model_veteran_income, terms = c("Political.scale", "Income.Group"))
 
 plot(preds_vet_income) +
   labs(
     title = "Political Ideology × Income Level",
     subtitle = "Predicted Probability of Selecting the Veteran Candidate",
     x = "Political Ideology (0 = Liberal, 7 = Conservative)",
     y = "Predicted Probability",
     color = "Income Group",
     fill = "Income Group"
   ) +
   theme_minimal()
                             data = Merged_Data_Cleaned, family = binomial)
 
 preds_teacher_income <- ggpredict(model_teacher_income, terms = c("Political.scale", "Income.Group"))
 
 plot(preds_teacher_income) +
   labs(
     title = "Political Ideology × Income Level",
     subtitle = "Predicted Probability of Selecting the Teacher Candidate",
     x = "Political Ideology (0 = Liberal, 7 = Conservative)",
     y = "Predicted Probability",
     color = "Income Group",
     fill = "Income Group"
   ) +
   theme_minimal()
 library(tidyverse)
 
 # Select relevant columns
 issue_columns <- c("Public_education", "Healthcare", "Rural_development", "Broadband",
                    "Wealth_inequality", "Unions", "Ag_investments", "Good_job", "Addiction")
 
 # Pivot to long format and recode issue names as factor labels
 issue_long <- Merged_Data_Cleaned %>%
   select(Political.scale, all_of(issue_columns)) %>%
   pivot_longer(cols = all_of(issue_columns), names_to = "Issue", values_to = "Selected") %>%
   mutate(Issue = forcats::fct_recode(Issue,
                                      "Public Education" = "Public_education",
                                      "Healthcare" = "Healthcare",
                                      "Rural Development" = "Rural_development",
                                      "Broadband Access" = "Broadband",
                                      "Wealth Inequality" = "Wealth_inequality",
                                      "Labor Unions" = "Unions",
                                      "Agricultural Investment" = "Ag_investments",
                                      "Good Jobs" = "Good_job",
                                      "Addiction Crisis" = "Addiction"))
 
 # Summarize selection rate
 issue_summary <- issue_long %>%
   group_by(Political.scale, Issue) %>%
   summarise(SelectionRate = mean(Selected, na.rm = TRUE), .groups = "drop")
 
 # Plot heatmap
 ggplot(issue_summary, aes(x = Political.scale, y = fct_reorder(Issue, SelectionRate), fill = SelectionRate)) +
   geom_tile(color = "white") +
   scale_fill_gradient(low = "white", high = "darkgreen") +
   labs(title = "Importance of Issues by Political Ideology (0 = Liberal, 7 = Conservative)",
        x = "Political Ideology Score",
        y = "Issue",
        fill = "Mean Selection Rate") +
   theme_minimal()
 library(tidyverse)
 library(scales)
 
 # Select the issue variables
 issue_vars <- c("Public_education", "Healthcare", "Rural_development", "Broadband",
                 "Wealth_inequality", "Unions", "Ag_investments", "Good_job", "Addiction")
 
 # Pivot to long format
 issue_long <- Merged_Data_Cleaned %>%
   select(all_of(issue_vars), Political.scale, Income.Group) %>%
   pivot_longer(cols = all_of(issue_vars), names_to = "Issue", values_to = "Selected")
 
 # Clean up issue labels
 issue_long <- issue_long %>%
   mutate(Issue = recode(Issue,
                         Public_education = "Public Education",
                         Healthcare = "Healthcare",
                         Rural_development = "Rural Development",
                         Broadband = "Broadband Access",
                         Wealth_inequality = "Wealth Inequality",
                         Unions = "Labor Unions",
                         Ag_investments = "Agricultural Investment",
                         Good_job = "Good Jobs",
                         Addiction = "Addiction Crisis"))
 
 # Make Income Group an ordered factor (High to Low)
 issue_long$Income.Group <- factor(issue_long$Income.Group, 
                                   levels = c("High Income", "Upper-Middle Income", 
                                              "Lower-Middle Income", "Low Income"))
 
 # Summarize selection rate by ideology and issue, within income group
 issue_summary <- issue_long %>%
   group_by(Income.Group, Political.scale, Issue) %>%
   summarise(SelectionRate = mean(Selected, na.rm = TRUE), .groups = "drop")
 
 # Plot heatmap faceted by income group
 ggplot(issue_summary, aes(x = Political.scale, y = fct_reorder(Issue, SelectionRate), fill = SelectionRate)) +
   geom_tile(color = "white") +
   geom_text(aes(label = round(SelectionRate, 2)), color = "white", size = 3.5) +
   facet_wrap(~Income.Group) +
   scale_fill_gradient(low = "#00441b", high = "#a1d99b") +
   labs(
     title = "Importance of Issues by Political Ideology and Income Group",
     x = "Political Ideology Score (0 = Liberal, 7 = Conservative)",
     y = "Issue",
     fill = "Mean Selection Rate"
   ) +
   theme_minimal() +
   theme(
     axis.text.x = element_text(angle = 0),
     strip.text = element_text(size = 11, face = "bold")
   )
 library(tidyverse)
 library(scales)
 
 # Select the issue variables
 issue_vars <- c("Public_education", "Healthcare", "Rural_development", "Broadband",
                 "Wealth_inequality", "Unions", "Ag_investments", "Good_job", "Addiction")
 
 # Pivot to long format
 issue_long <- Merged_Data_Cleaned %>%
   select(all_of(issue_vars), Political.scale, income.gr) %>%
   pivot_longer(cols = all_of(issue_vars), names_to = "Issue", values_to = "Selected") %>%
   filter(!is.na(income.group))  # <--- Remove NA income group
 
 # Clean up issue labels
 issue_long <- issue_long %>%
   mutate(Issue = recode(Issue,
                         Public_education = "Public Education",
                         Healthcare = "Healthcare",
                         Rural_development = "Rural Development",
                         Broadband = "Broadband Access",
                         Wealth_inequality = "Wealth Inequality",
                         Unions = "Labor Unions",
                         Ag_investments = "Agricultural Investment",
                         Good_job = "Good Jobs",
                         Addiction = "Addiction Crisis"))
 
 # Order Income Group
 issue_long$Income.Group <- factor(issue_long$Income.Group,
                                   levels = c("High Income", "Upper-Middle Income",
                                              "Lower-Middle Income", "Low Income"))
 
 # Summarize selection rate
 issue_summary <- issue_long %>%
   group_by(Income.Group, Political.scale, Issue) %>%
   summarise(SelectionRate = mean(Selected, na.rm = TRUE), .groups = "drop")
 
 # Plot
 ggplot(issue_summary, aes(x = Political.scale, y = fct_reorder(Issue, SelectionRate), fill = SelectionRate)) +
   geom_tile(color = "white") +
   geom_text(aes(label = round(SelectionRate, 2)), color = "white", size = 3.5) +
   facet_wrap(~Income.Group) +
   scale_fill_gradient(low = "#00441b", high = "#a1d99b") +
   labs(
     title = "Importance of Issues by Political Ideology and Income Group",
     x = "Political Ideology Score (0 = Liberal, 7 = Conservative)",
     y = "Issue",
     fill = "Mean Selection Rate"
   ) +
   theme_minimal() +
   theme(
     axis.text.x = element_text(angle = 0),
     strip.text = element_text(size = 11, face = "bold")
   )
 # Fix the Addiction variable from the original checkbox response
 Merged_Data_Cleaned$Addiction <- ifelse(grepl("addiction", Merged_Data_Cleaned$Important.issues, ignore.case = TRUE), 1, 0)
 library(tidyverse)
 
 # List of all issue variables
 issue_vars <- c("Public_education", "Healthcare", "Good_job", "Rural_development",
                 "Broadband", "Wealth_inequality", "Unions", "Ag_investments", "Addiction")
 
 # Pivot longer
 issue_long <- Merged_Data_Cleaned %>%
   select(Political.scale, Income.Group, all_of(issue_vars)) %>%
   pivot_longer(cols = all_of(issue_vars), names_to = "Issue", values_to = "Selected") %>%
   filter(!is.na(Income.Group))  # Remove NA group for cleaner plots
 
 # Optional: rename issue labels for the plot
 issue_long$Issue <- recode(issue_long$Issue,
                            Public_education = "Public Education",
                            Healthcare = "Healthcare",
                            Good_job = "Good Jobs",
                            Rural_development = "Rural Development",
                            Broadband = "Broadband Access",
                            Wealth_inequality = "Wealth Inequality",
                            Unions = "Labor Unions",
                            Ag_investments = "Agricultural Investment",
                            Addiction = "Addiction Crisis")
 issue_long$Issue <- dplyr::recode(issue_long$Issue,
                                   "Public_education" = "Public Education",
                                   "Healthcare" = "Healthcare",
                                   "Good_job" = "Good Jobs",
                                   "Rural_development" = "Rural Development",
                                   "Broadband" = "Broadband Access",
                                   "Wealth_inequality" = "Wealth Inequality",
                                   "Unions" = "Labor Unions",
                                   "Ag_investments" = "Agricultural Investment",
                                   "Addiction" = "Addiction Crisis"
 )
 
 # Recreate the Addiction variable from Important.issues text column
 Merged_Data_Cleaned$Addiction <- ifelse(grepl("addiction", Merged_Data_Cleaned$Important.issues), 1, 0)
 # Define issue columns again if needed
 issue_vars <- c("Public_education", "Healthcare", "Good_job", "Rural_development",
                 "Broadband", "Wealth_inequality", "Unions", "Ag_investments", "Addiction")
 
 # Pivot longer
 issue_long <- Merged_Data_Cleaned %>%
   select(Political.scale, Income.Group, all_of(issue_vars)) %>%
   pivot_longer(cols = all_of(issue_vars), names_to = "Issue", values_to = "Selected") %>%
   filter(!is.na(Income.Group))  # Remove NA group for cleaner plots
 
 # Rename issues nicely for display
 issue_long$Issue <- dplyr::recode(issue_long$Issue,
                                   "Public_education" = "Public Education",
                                   "Healthcare" = "Healthcare",
                                   "Good_job" = "Good Jobs",
                                   "Rural_development" = "Rural Development",
                                   "Broadband" = "Broadband Access",
                                   "Wealth_inequality" = "Wealth Inequality",
                                   "Unions" = "Labor Unions",
                                   "Ag_investments" = "Agricultural Investment",
                                   "Addiction" = "Addiction Crisis"
 )
 
 # Summarize selection rate by ideology and issue
 issue_summary <- issue_long %>%
   group_by(Political.scale, Issue, Income.Group) %>%
   summarise(SelectionRate = mean(Selected, na.rm = TRUE), .groups = "drop")
 
 # Plot
 ggplot(issue_summary, aes(x = Political.scale, y = fct_reorder(Issue, SelectionRate), fill = SelectionRate)) +
   geom_tile(color = "white") +
   geom_text(aes(label = round(SelectionRate, 2)), color = "white", size = 3) +
   scale_fill_gradient(low = "white", high = "darkgreen") +
   facet_wrap(~Income.Group, ncol = 2) +
   labs(title = "Importance of Issues by Political Ideology and Income Group",
        x = "Political Ideology Score (0 = Liberal, 7 = Conservative)",
        y = "Issue",
        fill = "Mean Selection Rate") +
   theme_minimal() +
   theme(strip.text = element_text(face = "bold", size = 12))
 library(networkD3)
 library(dplyr)
 # Create ideology bins
 Merged_Data_Cleaned <- Merged_Data_Cleaned %>%
   mutate(Ideology_Group = case_when(
     Political.scale <= 2 ~ "Liberal",
     Political.scale <= 4 ~ "Moderate",
     TRUE ~ "Conservative"
   ))
 
 # Group counts 
 sankey_data <- Merged_Data_Cleaned %>%
   count(Ideology_Group, Teacher) %>%
   mutate(Teacher_Label = ifelse(Teacher == 1, "Selected Teacher", "Did Not Select Teacher"))
 # Create a list of unique labels
 nodes <- data.frame(name = unique(c(sankey_data$Ideology_Group, sankey_data$Teacher_Label)))
 
 # Create links between ideology group and teacher choice
 links <- sankey_data %>%
   mutate(source = match(Ideology_Group, nodes$name) - 1,
          target = match(Teacher_Label, nodes$name) - 1,
          value = n) %>%
   select(source, target, value)
 sankeyNetwork(Links = links, Nodes = nodes,
               Source = "source", Target = "target",
               Value = "value", NodeID = "name",
               fontSize = 14, nodeWidth = 30)
 # Step 1: Create a dataframe of the flows
 sankey_data <- Merged_Data_Cleaned %>%
   filter(!is.na(Self.identified.partisanship), !is.na(Rural.background)) %>%
   count(Self.identified.partisanship, Rural.background) %>%
   rename(source = Self.identified.partisanship, target = Rural.background, value = n)
 
 # Step 2: Create a list of unique nodes
 nodes <- data.frame(name = unique(c(sankey_data$source, sankey_data$target)))
 
 # Step 3: Recode node names to numeric indices for Sankey
 sankey_data$source_id <- match(sankey_data$source, nodes$name) - 1
 sankey_data$target_id <- match(sankey_data$target, nodes$name) - 1
 
 # Step 4: Create the Sankey diagram
 sankeyNetwork(Links = sankey_data,
               Nodes = nodes,
               Source = "source_id",
               Target = "target_id",
               Value = "value",
               NodeID = "name",
               fontSize = 13,
               nodeWidth = 30,
               sinksRight = FALSE) 
 colnames(Merged_Data_Cleaned)
 # Load necessary libraries
 library(dplyr)
 library(ggplot2)
 library(tidyr)
 library(forcats)  # For better factor handling
 
 # Pivot the dataset to a long format
 rank_data <- Merged_Data_Cleaned %>%
   select(Travels.to.rural, Rural.background, Rural.priority, Working.class.focus) %>%
   pivot_longer(cols = everything(), names_to = "Trait", values_to = "Rank")
 
 # Count the occurrences of each rank for each trait
 rank_counts <- rank_data %>%
   group_by(Trait, Rank) %>%
   summarise(n = n()) %>%
   mutate(pct = n / sum(n))  # Get proportions of each rank
 
 # Create a diverging stacked bar plot
 ggplot(rank_counts, aes(x = fct_reorder(Trait, -Rank), y = pct, fill = factor(Rank))) +
   geom_bar(stat = "identity", position = "fill") +
   scale_fill_brewer(palette = "RdYlBu", direction = -1, name = "Rank (1 = Most Appealing)") +
   labs(title = "How Rural Voters Ranked Democratic Candidate Traits",
        x = "Candidate Trait",
        y = "Proportion of Respondents") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
 # Clean up the names of candidate traits
 rank_counts$Trait <- gsub("\\.", " ", rank_counts$Trait)  # Remove periods
 
 # Create the diverging stacked bar plot again
 ggplot(rank_counts, aes(x = fct_reorder(Trait, -Rank), y = pct, fill = factor(Rank))) +
   geom_bar(stat = "identity", position = "fill") +
   scale_fill_brewer(palette = "RdYlBu", direction = -1, name = "Rank (1 = Most Appealing)") +
   labs(title = "How Rural Voters Ranked Democratic Candidate Traits",
        x = "Candidate Trait",
        y = "Proportion of Respondents") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold", color = "darkblue"), 
         axis.title.x = element_text(size = 14, face = "bold", color = "darkblue"),
         axis.title.y = element_text(size = 14, face = "bold", color = "darkblue"),
         plot.title = element_text(size = 16, face = "bold", color = "darkblue"))
 library(ggplot2)
 
 library(ggplot2)
 
 # Reorder Education.Level factor to ensure proper ordering
 Merged_Data_Cleaned$Education.Level <- factor(Merged_Data_Cleaned$Education.Level, 
                                               levels = c("Less than high school degree",
                                                          "High school graduate (high school diploma or equivalent including GED)",
                                                          "Some college but no degree",
                                                          "Associate degree in college (2-year)",
                                                          "Bachelor's degree in college (4-year)",
                                                          "Master's degree",
                                                          "Doctoral degree",
                                                          "Professional degree (JD, MD)"))
 
 library(ggplot2)
 
 # Ensure the Education level is a factor
 Merged_Data_Cleaned$Education.Level <- factor(Merged_Data_Cleaned$Education.Level, 
                                               levels = c("Less than high school degree", 
                                                          "High school graduate (high school diploma or equivalent including GED)",
                                                          "Some college but no degree",
                                                          "Associate degree in college (2-year)",
                                                          "Bachelor's degree in college (4-year)",
                                                          "Master's degree", 
                                                          "Doctoral degree", 
                                                          "Professional degree (JD, MD)"))
 
 library(ggplot2)
 library(dplyr)
 
 # Collapse Education Levels into broader categories
 Merged_Data_Cleaned <- Merged_Data_Cleaned %>%
   mutate(Collapsed_Education_Level = case_when(
     Education.Level %in% c("Less than high school degree", 
                            "High school graduate (high school diploma or equivalent including GED)", 
                            "Some college but no degree") ~ "Less than Bachelor's",
     Education.Level %in% c("Associate degree in college (2-year)", 
                            "Bachelor's degree in college (4-year)") ~ "Bachelor's Degree",
     Education.Level %in% c("Master's degree", "Doctoral degree", "Professional degree (JD, MD)") ~ "Advanced Degree",
     TRUE ~ Education.Level  # Just in case there's any other category
   ))
 
 # Create the plot with collapsed education levels and larger text for x-axis labels
 library(ggplot2)
 library(dplyr)
 
 ggplot(
   Merged_Data_Cleaned %>% filter(!is.na(Collapsed_Education_Level)),
   aes(x = Collapsed_Education_Level)
 ) +
   geom_bar(fill = "skyblue", color = "black") +
   theme_minimal() +
   labs(
     title = "Distribution of Simplified Education Level of Participants",
     x = "Education Level",
     y = "Count of Participants"
   ) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14))
 
 # Create the plot
 ggplot(Merged_Data_Cleaned, aes(x = age, fill = sex)) +
   geom_histogram(binwidth = 5, alpha = 0.6, position = "identity", color = "black") +
   facet_wrap(~ sex) +  # Facet by sex for separate distributions
   labs(title = "Age Distribution by Sex",
        x = "Age",
        y = "Count of Participants") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1), 
         legend.position = "none")
 ggplot(Merged_Data_Cleaned, aes(x = age, fill = sex)) +
   geom_histogram(binwidth = 5, alpha = 0.6, position = "identity", color = "black") +
   facet_wrap(~ sex) +  # Facet by sex for separate distributions
   labs(title = "Age Distribution by Sex",
        x = "Age",
        y = "Count of Participants") +
   scale_fill_manual(values = c("0" = "darkgrey", "1" = "lightblue"),
                     labels = c("0" = "Male", "1" = "Female")) +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1),
         legend.title = element_blank(),
         legend.position = "bottom")
 # Convert sex variable to a factor
 Merged_Data_Cleaned$sex <- factor(Merged_Data_Cleaned$sex, levels = c(0, 1), labels = c("Male", "Female"))
 
 # Plot the histogram
 ggplot(Merged_Data_Cleaned, aes(x = age, fill = sex)) +
   geom_histogram(binwidth = 5, alpha = 0.6, position = "identity", color = "black") +
   facet_wrap(~ sex) +  # Facet by sex for separate distributions
   labs(title = "Age Distribution by Sex",
        x = "Age",
        y = "Count of Participants") +
   scale_fill_manual(values = c("Male" = "darkgrey", "Female" = "lightblue")) +  # Set color manually
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1),
         legend.title = element_blank(),
         legend.position = "bottom")
mean(Merged_Data_Cleaned$Age)
str(Merged_Data_Cleaned$age)
mean(Merged_Data_Cleaned$age, na.rm = TRUE)
summary(Merged_Data_Cleaned$age, na.rm = TRUE)
# Political Affiliation Distribution
party_distribution <- table(Merged_Data_Cleaned$Party.registration) 
party_percent <- prop.table(party_distribution) * 100 

# Display percentages
party_percent
# Calculate the average economic populism score for each respondent
Merged_Data_Cleaned$Economic_Populism_Score <- rowMeans(Merged_Data_Cleaned[c("Education..1.favorability", 
                                                                              "Taxes..1.favorability", 
                                                                              "Ag..2.favorability")], 
                                                        na.rm = TRUE)

# Create Economic Populism Score by averaging the favorability of economic populist messages
Merged_Data_Cleaned$Economic_Populism_Score <- rowMeans(Merged_Data_Cleaned[c("Education..1.favorability", 
                                                                              "Taxes..1.favorability", 
                                                                              "Ag.2.favorability")], 
                                                        na.rm = TRUE)
# Calculate the average populism score across all respondents
average_populism_score <- mean(Merged_Data_Cleaned$Economic_Populism_Score, na.rm = TRUE)

# Display the average economic populism score
average_populism_score
# Create Non-Economic Populism Score by averaging the favorability of non-populist messages
Merged_Data_Cleaned$Non_Economic_Populism_Score <- rowMeans(Merged_Data_Cleaned[c("Education..2.favorability", 
                                                                                  "Taxes..2.favorability", 
# Convert the 'sex' variable to numeric (1 for Female, 0 for Male)
Merged_Data_Cleaned$sex_numeric <- ifelse(Merged_Data_Cleaned$sex == "Female", 1, 0)

# Now calculate the male and female percentages
male_count <- sum(Merged_Data_Cleaned$sex_numeric == 0, na.rm = TRUE)
female_count <- sum(Merged_Data_Cleaned$sex_numeric == 1, na.rm = TRUE)
total_respondents <- nrow(Merged_Data_Cleaned)

# Calculate the percentages
male_percentage <- (male_count / total_respondents) * 100
female_percentage <- (female_count / total_respondents) * 100

# Display results
cat("Male: ", round(male_percentage, 2), "%\n")
cat("Female: ", round(female_percentage, 2), "%\n")
# Convert the 'sex' variable to numeric (1 for Female, 0 for Male)
Merged_Data_Cleaned$sex_numeric <- ifelse(Merged_Data_Cleaned$sex == "Female", 1, 0)

# Now calculate the male and female percentages
male_count <- sum(Merged_Data_Cleaned$sex_numeric == 0, na.rm = TRUE)
female_count <- sum(Merged_Data_Cleaned$sex_numeric == 1, na.rm = TRUE)
total_respondents <- nrow(Merged_Data_Cleaned)

# Calculate the percentages
male_percentage <- (male_count / total_respondents) * 100
female_percentage <- (female_count / total_respondents) * 100

# Display results
cat("Male: ", round(male_percentage, 2), "%\n")
cat("Female: ", round(female_percentage, 2), "%\n")
# Calculate the count of each educational level
education_counts <- table(Merged_Data_Cleaned$Education.Level)

# Calculate the total number of respondents
total_respondents <- nrow(Merged_Data_Cleaned)

# Calculate the percentage for each educational level
education_percentage <- (education_counts / total_respondents) * 100

# Display the results
print(education_percentage)
# Calculate the count of each simplified educational level
education_counts_simplified <- table(Merged_Data_Cleaned$Collapsed_Education_Level)

# Calculate the total number of respondents
total_respondents <- nrow(Merged_Data_Cleaned)

# Calculate the percentage for each simplified educational level
education_percentage_simplified <- (education_counts_simplified / total_respondents) * 100

# Display the results
print(education_percentage_simplified)
# Calculate the percentage of respondents in each income bracket
income_percentage <- prop.table(table(Merged_Data_Cleaned$income)) * 100

# Display the results
print(income_percentage)
# Self.identified.partisanship percentages
partisanship_counts <- table(Merged_Data_Cleaned$Self.identified.partisanship)
partisanship_percentages <- prop.table(partisanship_counts) * 100
print(partisanship_percentages)

# Party.registration percentages
party_registration_counts <- table(Merged_Data_Cleaned$Party.registration)
party_registration_percentages <- prop.table(party_registration_counts) * 100
print(party_registration_percentages)
# Calculate the percentage distribution for the 'Race' variable
race_percentage <- table(Merged_Data_Cleaned$Race) / nrow(Merged_Data_Cleaned) * 100

# Display the result
race_percentage
# Replace 'your_data' with your actual dataset name
Merged_Data_Cleaned$party_strength_leaning[Merged_Data_Cleaned$Self.identified.partisanship == "Democrat" & Merged_Data_Cleaned$Democrat.strength == 1] <- "Strong Democrat"
Merged_Data_Cleaned$party_strength_leaning[Merged_Data_Cleaned$Self.identified.partisanship == "Democrat" & Merged_Data_Cleaned$Democrat.strength == 0] <- "Not Very Strong Democrat"

# Republicans
Merged_Data_Cleaned$party_strength_leaning[Merged_Data_Cleaned$Self.identified.partisanship == "Republican" & Merged_Data_Cleaned$Republican.Strength == 1] <- "Strong Republican"
Merged_Data_Cleaned$party_strength_leaning[Merged_Data_Cleaned$Self.identified.partisanship == "Republican" & Merged_Data_Cleaned$Republican.Strength == 0] <- "Not Very Strong Republican"

# Independents
Merged_Data_Cleaned$party_strength_leaning[Merged_Data_Cleaned$Self.identified.partisanship == "Independent" & Merged_Data_Cleaned$Closer.to.Republican.or.Democrat == 1] <- "Closer to Republican"
Merged_Data_Cleaned$party_strength_leaning[Merged_Data_Cleaned$Self.identified.partisanship == "Independent" & Merged_Data_Cleaned$Closer.to.Republican.or.Democrat == 0] <- "Closer to Democrat"
Merged_Data_Cleaned$party_strength_leaning[Merged_Data_Cleaned$Self.identified.partisanship == "Independent" & is.na(Merged_Data_Cleaned$Closer.to.Republican.or.Democrat)] <- "Neither"

# Remove NA values for party strength
Merged_Data_Cleaned <- Merged_Data_Cleaned[!is.na(Merged_Data_Cleaned$party_strength_leaning), ]

# Create the plot
library(ggplot2)
plot <- ggplot(Merged_Data_Cleaned, aes(x = Self.identified.partisanship, fill = party_strength_leaning)) +
  geom_bar(position = "fill") +
  labs(title = "Party Strength and Leaning Breakdown",
       x = "Partisanship",
       y = "Proportion",
       fill = "Party Strength / Leaning") +
  scale_fill_manual(values = c("Strong Democrat" = "#1f77b4",  # Blue shades for Democrats
                               "Not Very Strong Democrat" = "#aec7e8",
                               "Strong Republican" = "#d62728",  # Red shades for Republicans
                               "Not Very Strong Republican" = "#ff9896",
                               "Closer to Republican" = "#ff7f0e",  # Orange shades for Independents
                               "Closer to Democrat" = "#ffbb78",
                               "Neither" = "#98df8a")) +  # Green for 'Neither'
  theme_minimal()

# Show the plot
print(plot)
# Remove NA values from Political.scale
cleaned_political_scale <- na.omit(Merged_Data_Cleaned$Political.scale)

# Summary of the cleaned Political.scale variable
summary(cleaned_political_scale)

# Distribution of cleaned Political.scale using a histogram
library(ggplot2)
ggplot(data.frame(cleaned_political_scale), aes(x = cleaned_political_scale)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Political Ideology",
       x = "Political Ideology Scale (0 = Liberal, 7 = Conservative)",
       y = "Count of Participants") +
  theme_minimal()
library(dplyr)
library(tidyr)
library(ggplot2)

# Clean up the "Why.Dems.fail.ranked" responses by splitting each response
cleaned_why_dems_fail <- Merged_Data_Cleaned %>%
  filter(!is.na(Why.Dems.fail.ranked)) %>%
  separate_rows(Why.Dems.fail.ranked, sep = ",") %>%
  mutate(Why.Dems.fail.ranked = trimws(Why.Dems.fail.ranked))

# Count the frequency of each reason
reason_counts <- cleaned_why_dems_fail %>%
  count(Why.Dems.fail.ranked, sort = TRUE) %>%
  rename(Reason = Why.Dems.fail.ranked, Frequency = n)

# Plot the frequency of each reason
ggplot(reason_counts, aes(x = reorder(Reason, Frequency), y = Frequency, fill = Reason)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Frequency of Reasons for Democratic Party Shortcomings",
       x = "Reason", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual(values = rainbow(length(reason_counts$Reason)))
library(dplyr)
library(ggplot2)

# Remove the "please type your thoughts below" response
cleaned_why_dems_fail_filtered <- cleaned_why_dems_fail %>%
  filter(Why.Dems.fail.ranked != "please type your thoughts below:")

# Count the frequency of each reason (after filtering out unwanted reason)
reason_counts_filtered <- cleaned_why_dems_fail_filtered %>%
  count(Why.Dems.fail.ranked, sort = TRUE) %>%
  rename(Reason = Why.Dems.fail.ranked, Frequency = n)

# Create the plot with a narrower width
ggplot(reason_counts_filtered, aes(x = reorder(Reason, Frequency), y = Frequency, fill = Reason)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # This makes the chart less wide and more readable
  labs(title = "Frequency of Reasons for Democratic Party Shortcomings",
       x = "Reason", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.margin = margin(1, 1, 1, 3, "cm")) +  # Adjust the margin for a better fit
  scale_fill_manual(values = c("cyan", "green", "blue", "yellow", "red", "magenta"))  # Adjust colors
# Calculate the average economic populism favorability across the issues
Merged_Data_Cleaned$Economic_Populism_Avg <- rowMeans(Merged_Data_Cleaned[c("Education..1.favorability", 
                                                                            "Taxes..1.favorability", 
                                                                            "Ag.2.favorability")], 
                                                      na.rm = TRUE)
# Create the regression model with Party.registration as the independent variable
model_econ_populism <- lm(Economic_Populism_Avg ~ Party.registration + age + income + Education.Level + Race + sex, 
                          data = Merged_Data_Cleaned)

# Display the summary of the model
summary(model_econ_populism)
# Install the necessary package
library(broom)

# Get tidy results from the model
tidy_model <- tidy(lm(Economic_Populism_Avg ~ Party.registration + age + income + Education.Level + Race + sex, data = Merged_Data_Cleaned))

# Create a coefficient plot
library(ggplot2)
ggplot(tidy_model, aes(x = term, y = estimate)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) + 
  labs(title = "Coefficient Plot of Economic Populism Regression Model",
       x = "Predictors",
       y = "Estimate of Coefficients") + 
  theme_minimal() +
  coord_flip()

library(ggplot2)
library(broom)
library(dplyr)
# Center the age variable (subtract mean age)
Merged_Data_Cleaned$age_centered <- scale(Merged_Data_Cleaned$age, center = TRUE, scale = FALSE)

# Run your regression model
model_populism_predictors <- lm(mean_populist_favorability ~ 
                                  Political.scale * Education.Group +
                                  Political.scale * Income.Group +
                                  Party.registration +
                                  sex + Race + age_centered,
                                data = Merged_Data_Cleaned)

# Tidy the results
coef_data <- tidy(model_populism_predictors, conf.int = TRUE)

# Clean and format labels for better readability
coef_data$term <- recode(coef_data$term,
                         "Political.scale" = "Political Ideology",
                         "sexFemale" = "Sex: Female",
                         "Race" = "Race (Nonwhite)",
                         "age_centered" = "Age (Centered)",
                         "Party.registrationIndependent" = "Party: Independent",
                         "Party.registrationDemocratic" = "Party: Democrat",
                         "Party.registrationNone" = "Party: None",
                         "Education.GroupBachelor's" = "Education: Bachelor's",
                         "Education.GroupGrad Degree" = "Education: Graduate Degree",
                         "Education.GroupSome College" = "Education: Some College",
                         "Education.GroupNo College" = "Education: No College",
                         "Income.GroupLow Income" = "Income: Low",
                         "Income.GroupLower-Middle Income" = "Income: Lower-Middle",
                         "Income.GroupUpper-Middle Income" = "Income: Upper-Middle",
                         "Income.GroupHigh Income" = "Income: High",
                         "Political.scale:Education.GroupSome College" = "Ideology × Some College",
                         "Political.scale:Education.GroupBachelor's" = "Ideology × Bachelor's",
                         "Political.scale:Education.GroupGrad Degree" = "Ideology × Graduate Degree",
                         "Political.scale:Income.GroupLow Income" = "Ideology × Low Income",
                         "Political.scale:Income.GroupLower-Middle Income" = "Ideology × Lower-Middle Income",
                         "Political.scale:Income.GroupUpper-Middle Income" = "Ideology × Upper-Middle Income"
)

# Filter out intercept
coef_data <- coef_data %>% filter(term != "(Intercept)")

# Plot with improved labels
ggplot(coef_data, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.25) +
  labs(
    title = "Coefficient Plot: Predictors of Economic Populism Favorability",
    subtitle = "With 95% Confidence Intervals",
    x = "Coefficient Estimate",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    plot.margin = margin(20, 30, 20, 30)
  )

# Ensure necessary packages are loaded
library(broom)
library(ggplot2)

# Run your model
                                  Political.scale * Education.Group +
                                  Political.scale * Income.Group +
                                  Party.registration +
                                  sex + Race + age_centered,
                                data = Merged_Data_Cleaned)

# Tidy the model results
tidy_model <- tidy(model_populism_predictors)

# Optionally clean up labels for clarity
tidy_model$term <- recode(tidy_model$term,
                          "Political.scale" = "Political Ideology",
                          "Party.registrationDemocratic" = "Party: Democrat",
                          "Party.registrationIndependent" = "Party: Independent",
                          "Party.registrationNone" = "Party: None",
                          "sexFemale" = "Sex: Female",
                          "RaceNonwhite" = "Race (Nonwhite)",
                          "age_centered" = "Age (Centered)",
                          "Education.GroupBachelor's" = "Education: Bachelor's",
                          "Education.GroupGrad Degree" = "Education: Graduate Degree",
                          "Education.GroupSome College" = "Education: Some College",
                          "Education.GroupHigh school graduate" = "Education: High School Graduate",
                          "Education.GroupLess than high school degree" = "Education: <HS Degree",
                          "Political.scale:Education.GroupSome College" = "Ideology × Some College",
                          "Political.scale:Education.GroupGrad Degree" = "Ideology × Graduate Degree",
                          "Political.scale:Education.GroupBachelor's" = "Ideology × Bachelor's",
                          "Political.scale:Income.GroupLow Income" = "Political.scale x Income.Group: Low",
                          "Political.scale:Income.GroupLower-Middle Income" = "Political.scale x Income.Group:Lower-Middle",
                          "Political.scale:Income.GroupUpper-Middle Income" = "Political.scale:Income.GroupUpper-Middle",
                          "Political.scale:Income.GroupHigh Income" = "Political.scale:Income.GroupHigh"
)

# Plot
ggplot(tidy_model, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error), height = 0.2) +
  labs(
    title = "Coefficient Plot: Predictors of Economic Populism Favorability",
    subtitle = "With 95% Confidence Intervals",
    x = "Coefficient Estimate",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    panel.grid.minor = element_blank()
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray")

# Load necessary libraries
library(broom)
library(ggplot2)

# Tidy model
tidy_model <- tidy(model_populism_predictors)

# Rename predictors for readability
tidy_model$term <- recode(tidy_model$term,
                          "(Intercept)" = "(Intercept)",
                          "Political.scale" = "Political Ideology",
                          "age_centered" = "Age (Centered)",
                          "sex" = "sex",
                          "Race" = "Race (Nonwhite)",
                          
                          "Party.registrationDemocratic" = "Party: Democrat",
                          "Party.registrationIndependent" = "Party: Independent",
                          "Party.registrationNone" = "Party: None",
                          
                          "Education.GroupSome College" = "Education: Some College",
                          "Education.GroupBachelor's" = "Education: Bachelor's",
                          "Education.GroupGrad Degree" = "Education: Graduate Degree",
                          
                          "Income.GroupLow Income" = "Income: Low",
                          "Income.GroupLower-Middle Income" = "Income: Lower-Middle",
                          "Income.GroupUpper-Middle Income" = "Income: Upper-Middle",
                          "Income.GroupHigh Income" = "Income: High",
                          
                          "Political.scale:Education.GroupSome College" = "Ideology × Education: Some College",
                          "Political.scale:Education.GroupBachelor's" = "Ideology × Education: Bachelor's",
                          "Political.scale:Education.GroupGrad Degree" = "Ideology × Education: Graduate Degree",
                          
                          "Political.scale:Income.GroupLow Income" = "Ideology × Income: Low",
                          "Political.scale:Income.GroupLower-Middle Income" = "Ideology × Income: Lower-Middle",
                          "Political.scale:Income.GroupUpper-Middle Income" = "Ideology × Income: Upper-Middle",
                          "Political.scale:Income.GroupHigh Income" = "Ideology × Income: High"
)

custom_order <- c(
  "(Intercept)",
  "Political Ideology",
  "Age (Centered)",
  "sex",
  "Race (Nonwhite)",
  
  "Party: Democrat",
  "Party: Independent",
  "Party: None",
  
  "Education: Some College",
  "Education: Bachelor's",
  "Education: Graduate Degree",
  
  "Income: Low",
  "Income: Lower-Middle",
  "Income: Upper-Middle",
  "Income: High",
  
  "Ideology × Education: Some College",
  "Ideology × Education: Bachelor's",
  "Ideology × Education: Graduate Degree",
  
  "Ideology × Income: Low",
  "Ideology × Income: Lower-Middle",
  "Ideology × Income: Upper-Middle",
  "Ideology × Income: High"
)

Merged_Data_Cleaned$Party.registration <- factor(Merged_Data_Cleaned$Party.registration)
Merged_Data_Cleaned$Party.registration <- relevel(Merged_Data_Cleaned$Party.registration, ref = "Republican")

# Load libraries
library(broom)
library(ggplot2)
library(dplyr)

# Tidy model
tidy_model <- tidy(model_populism_predictors, conf.int = TRUE)

# Recode terms for readability
tidy_model$term <- recode(tidy_model$term,
                          "(Intercept)" = "(Intercept)",
                          "Political.scale" = "Political Ideology",
                          "age_centered" = "Age (Centered)",
                          "sex" = "sex",
                          "Race" = "Race (Nonwhite)",
                          
                          "Party.registrationDemocratic" = "Party: Democrat",
                          "Party.registrationIndependent" = "Party: Independent",
                          "Party.registrationNone" = "Party: None",
                          
                          "Education.GroupSome College" = "Education: Some College",
                          "Education.GroupBachelor's" = "Education: Bachelor's",
                          "Education.GroupGrad Degree" = "Education: Graduate Degree",
                          
                          "Income.GroupLow Income" = "Income: Low",
                          "Income.GroupLower-Middle Income" = "Income: Lower-Middle",
                          "Income.GroupUpper-Middle Income" = "Income: Upper-Middle",
                          "Income.GroupHigh Income" = "Income: High",
                          
                          "Political.scale:Education.GroupSome College" = "Ideology × Education: Some College",
                          "Political.scale:Education.GroupBachelor's" = "Ideology × Education: Bachelor's",
                          "Political.scale:Education.GroupGrad Degree" = "Ideology × Education: Graduate Degree",
                          
                          "Political.scale:Income.GroupLow Income" = "Ideology × Income: Low",
                          "Political.scale:Income.GroupLower-Middle Income" = "Ideology × Income: Lower-Middle",
                          "Political.scale:Income.GroupUpper-Middle Income" = "Ideology × Income: Upper-Middle",
                          "Political.scale:Income.GroupHigh Income" = "Ideology × Income: High"
)

# Define custom order
custom_order <- c(
  "(Intercept)",
  "Political Ideology",
  "Age (Centered)",
  "sex",
  "Race (Nonwhite)",
  
  "Party: Democrat",
  "Party: Independent",
  "Party: None",
  
  "Education: Some College",
  "Education: Bachelor's",
  "Education: Graduate Degree",
  
  "Income: Low",
  "Income: Lower-Middle",
  "Income: Upper-Middle",
  "Income: High",
  
  "Ideology × Education: Some College",
  "Ideology × Education: Bachelor's",
  "Ideology × Education: Graduate Degree",
  
  "Ideology × Income: Low",
  "Ideology × Income: Lower-Middle",
  "Ideology × Income: Upper-Middle",
  "Ideology × Income: High"
)

# Reorder factor for plotting
tidy_model$term <- factor(tidy_model$term, levels = custom_order)

# Plot with custom order
ggplot(tidy_model, aes(x = estimate, y = term)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "Coefficient Plot: Predictors of Economic Populism Favorability",
    subtitle = "With 95% Confidence Intervals",
    x = "Coefficient Estimate",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    plot.margin = margin(20, 30, 20, 30)
  )
# Load necessary libraries
library(broom)
library(ggplot2)
library(dplyr)

# Tidy your model and include confidence intervals
tidy_model <- tidy(model_populism_predictors, conf.int = TRUE)

# Recode terms for clean labeling
tidy_model$term <- recode(tidy_model$term,
                          "(Intercept)" = "(Intercept)",
                          "Political.scale" = "Political Ideology",
                          "age_centered" = "Age (Centered)",
                          "sex" = "Sex: Female",
                          "Race" = "Race (Nonwhite)",
                          
                          "Party.registrationDemocratic" = "Party: Democrat",
                          "Party.registrationIndependent" = "Party: Independent",
                          "Party.registrationNone" = "Party: None",
                          
                          "Education.GroupSome College" = "Education: Some College",
                          "Education.GroupBachelor's" = "Education: Bachelor's",
                          "Education.GroupGrad Degree" = "Education: Graduate Degree",
                          
                          "Income.GroupLow Income" = "Income: Low",
                          "Income.GroupLower-Middle Income" = "Income: Lower-Middle",
                          "Income.GroupUpper-Middle Income" = "Income: Upper-Middle",
                          "Income.GroupHigh Income" = "Income: High",
                          
                          "Political.scale:Education.GroupSome College" = "Ideology × Education: Some College",
                          "Political.scale:Education.GroupBachelor's" = "Ideology × Education: Bachelor's",
                          "Political.scale:Education.GroupGrad Degree" = "Ideology × Education: Graduate Degree",
                          
                          "Political.scale:Income.GroupLow Income" = "Ideology × Income: Low",
                          "Political.scale:Income.GroupLower-Middle Income" = "Ideology × Income: Lower-Middle",
                          "Political.scale:Income.GroupUpper-Middle Income" = "Ideology × Income: Upper-Middle",
                          "Political.scale:Income.GroupHigh Income" = "Ideology × Income: High"
)

# Set custom order for better visual structure
custom_order <- c(
  "(Intercept)",
  "Political Ideology",
  "Age (Centered)",
  "Sex: Female",
  "Race (Nonwhite)",
  
  "Party: Democrat",
  "Party: Independent",
  "Party: None",
  
  "Education: Some College",
  "Education: Bachelor's",
  "Education: Graduate Degree",
  
  "Income: Low",
  "Income: Lower-Middle",
  "Income: Upper-Middle",
  "Income: High",
  
  "Ideology × Education: Some College",
  "Ideology × Education: Bachelor's",
  "Ideology × Education: Graduate Degree",
  
  "Ideology × Income: Low",
  "Ideology × Income: Lower-Middle",
  "Ideology × Income: Upper-Middle",
  "Ideology × Income: High"
)

# Remove NA terms and apply ordering
tidy_model <- tidy_model %>%
  filter(!is.na(term)) %>%
  mutate(term = factor(term, levels = custom_order))

# Plot
ggplot(tidy_model, aes(x = estimate, y = term)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "Coefficient Plot: Predictors of Economic Populism Favorability",
    subtitle = "With 95% Confidence Intervals",
    x = "Coefficient Estimate",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11)
  )

# Remove intercept from tidy_model
tidy_model_filtered <- tidy_model %>%
  filter(term != "(Intercept)")

# Plot without intercept
ggplot(tidy_model_filtered, aes(x = estimate, y = term)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "Coefficient Plot: Predictors of Economic Populism Favorability",
    subtitle = "With 95% Confidence Intervals",
    x = "Coefficient Estimate",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11)
  )

# Run the model if not already
                                  Political.scale * Education.Group +
                                  Political.scale * Income.Group +
                                  Party.registration +
                                  sex + Race + age_centered,
                                data = Merged_Data_Cleaned)

# Load stargazer
library(stargazer)

# Create the table
stargazer(model_populism_predictors,
          type = "html",  # or "text" if viewing in R console
          title = "Linear Regression: Predictors of Favorability Toward Economic Populism",
          dep.var.labels = "Favorability Score (Avg. Populist Message Rating)",
          covariate.labels = c(
            "Political Ideology",
            "Education: Some College",
            "Education: Bachelor's",
            "Education: Graduate Degree",
            "Income: Low",
            "Income: Lower-Middle",
            "Income: Upper-Middle",
            "Income: High",
            "Party: Democrat",
            "Party: Independent",
            "Party: None",
            "Sex: Female",
            "Race (Nonwhite)",
            "Age (Centered)",
            "Ideology × Some College",
            "Ideology × Bachelor's",
            "Ideology × Graduate Degree",
            "Ideology × Income: Low",
            "Ideology × Income: Lower-Middle",
            "Ideology × Income: Upper-Middle",
            "Ideology × Income: High"
          ),
          star.cutoffs = c(0.05, 0.01, 0.001),
          digits = 3,
          no.space = TRUE,
          notes.align = "l",
          omit.stat = c("f", "ser"),
          out = "Appendix_Table_EconPopulism_Predictors.html")

library(dplyr)

# Step 1: Add an "included" column to flag rows with ≤10 missing values
Original_Data$included <- ifelse(rowSums(is.na(Original_Data)) <= 10, "Included", "Excluded")

# Step 2: Ensure variables are character for comparison
Original_Data$Race <- as.character(Original_Data$Race)
Original_Data$sex <- as.character(Original_Data$sex)
Original_Data$Party.registration <- as.character(Original_Data$Party.registration)

# Step 3: Group summary by inclusion
group_summary <- Original_Data %>%
  group_by(included) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    mean_ideology = mean(Political.scale, na.rm = TRUE),
    percent_female = mean(sex == "Female", na.rm = TRUE) * 100,
    percent_nonwhite = mean(Race != "White" & !is.na(Race), na.rm = TRUE) * 100,
    mean_income = mean(income.numeric, na.rm = TRUE)
  )

# Step 4: Party registration breakdown
party_breakdown <- Original_Data %>%
  filter(!is.na(Party.registration)) %>%
  group_by(included, Party.registration) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(included) %>%
  mutate(percent = round(100 * count / sum(count), 1))

# Step 5: View the output
print(group_summary)
print(party_breakdown)

library(dplyr)

# Step 1: Mark whether each observation was included
Original_Data$included <- ifelse(rowSums(is.na(Original_Data)) <= 10, "Included", "Excluded")

# Step 2: Clean and recode factors (if necessary)
Original_Data$sex <- as.factor(Original_Data$sex)
Original_Data$Race <- as.factor(Original_Data$Race)
Original_Data$Party.registration <- as.factor(Original_Data$Party.registration)
Original_Data$Income.Group <- as.factor(Original_Data$Income.Group)
Original_Data$Education.Group <- as.factor(Original_Data$Education.Group)

# Convert to numeric if needed
Original_Data$income.numeric <- as.numeric(as.character(Original_Data$income.numeric))
Original_Data$education.level.numeric <- as.numeric(as.character(Original_Data$education.level.numeric))
levels(as.factor(Original_Data$income))

Original_Data$income.numeric <- recode(Original_Data$income,
                                       "Less than $10,000" = 5000,
                                       "$10,000 to $19,999" = 15000,
                                       "$20,000 to $29,999" = 25000,
                                       "$30,000 to $39,999" = 35000,
                                       "$40,000 to $49,999" = 45000,
                                       "$50,000 to $59,999" = 55000,
                                       "$60,000 to $69,999" = 65000,
                                       "$70,000 to $79,999" = 75000,
                                       "$80,000 to $89,999" = 85000,
                                       "$90,000 to $99,999" = 95000,
                                       "$100,000 to $149,999" = 125000,
                                       "$150,000 or more" = 160000,
                                       .default = NA_real_
)

Original_Data$Income.Group <- cut(
  Original_Data$income.numeric,
  breaks = c(-Inf, 29999, 59999, 99999, Inf),
  labels = c("Low Income", "Lower-Middle Income", "Upper-Middle Income", "High Income"),
  right = TRUE
)

levels(Original_Data$Education.Level)

Original_Data$Education.Group <- recode(Original_Data$Education.Level,
                                        "Less than high school degree" = "No College",
                                        "High school graduate (high school diploma or equivalent including GED)" = "No College",
                                        "Some college but no degree" = "Some College",
                                        "Bachelor's degree in college (4-year)" = "Bachelor's",
                                        "Master's degree" = "Grad Degree",
                                        "Doctoral degree" = "Grad Degree",
                                        "Professional degree (JD, MD)" = "Grad Degree",
                                        .default = NA_character_
)

Original_Data$Education.Group <- factor(Original_Data$Education.Group,
                                        levels = c("No College", "Some College", "Bachelor's", "Grad Degree"))
table(Original_Data$Education.Group, useNA = "ifany")

library(dplyr)

# Recreate 'included' flag (your cleaned dataset has 155 observations)
Original_Data$included <- ifelse(row_number() %in% rownames(Merged_Data_Cleaned), "Included", "Excluded")

# Convert variables to factors if needed
Original_Data$sex <- as.factor(Original_Data$sex)
Original_Data$Race <- as.factor(Original_Data$Race)
Original_Data$Party.registration <- as.factor(Original_Data$Party.registration)
Original_Data$Income.Group <- factor(Original_Data$Income.Group, levels = c("Low Income", "Lower-Middle Income", "Upper-Middle Income", "High Income"))
Original_Data$Education.Group <- factor(Original_Data$Education.Group, levels = c("No College", "Some College", "Bachelor's", "Grad Degree"))

# Summarize continuous variables
group_summary <- Original_Data %>%
  group_by(included) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    mean_ideology = mean(Political.scale, na.rm = TRUE),
    percent_female = mean(sex == "Female", na.rm = TRUE),
    percent_nonwhite = mean(Race != "White", na.rm = TRUE)
  )

# Summarize categorical variables
party_breakdown <- Original_Data %>%
  filter(!is.na(Party.registration)) %>%
  count(included, Party.registration) %>%
  group_by(included) %>%
  mutate(percent = round(100 * n / sum(n), 1))

income_breakdown <- Original_Data %>%
  filter(!is.na(Income.Group)) %>%
  count(included, Income.Group) %>%
  group_by(included) %>%
  mutate(percent = round(100 * n / sum(n), 1))

education_breakdown <- Original_Data %>%
  filter(!is.na(Education.Group)) %>%
  count(included, Education.Group) %>%
  group_by(included) %>%
  mutate(percent = round(100 * n / sum(n), 1))

# View summaries
print(group_summary)
print(party_breakdown)
print(income_breakdown)
print(education_breakdown)

library(dplyr)

# Step 1: Add an ID to each row if not present
Original_Data <- Original_Data %>% mutate(ID = row_number())
Merged_Data_Cleaned <- Merged_Data_Cleaned %>% mutate(ID = row_number())

# Step 2: Flag whether the observation was included in the cleaned dataset
Original_Data$included <- ifelse(Original_Data$ID %in% Merged_Data_Cleaned$ID, "Included", "Excluded")

# Step 3: Convert relevant variables to factor if needed
Original_Data$sex <- as.factor(Original_Data$sex)
Original_Data$Race <- as.factor(Original_Data$Race)

Original_Data$Party.registration <- as.factor(Original_Data$Party.registration)

# Step 4: Summarize continuous demographics
group_summary <- Original_Data %>%
  group_by(included) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    mean_ideology = mean(Political.scale, na.rm = TRUE),
    percent_female = mean(sex == "Female", na.rm = TRUE),
    percent_nonwhite = mean(Race != "White", na.rm = TRUE)
  )

# Step 5: Categorical breakdowns
party_breakdown <- Original_Data %>%
  filter(!is.na(Party.registration)) %>%
  count(included, Party.registration) %>%
  group_by(included) %>%
  mutate(percent = round(100 * n / sum(n), 1))

income_breakdown <- Original_Data %>%
  filter(!is.na(Income.Group)) %>%
  count(included, Income.Group) %>%
  group_by(included) %>%
  mutate(percent = round(100 * n / sum(n), 1))

education_breakdown <- Original_Data %>%
  filter(!is.na(Education.Group)) %>%
  count(included, Education.Group) %>%
  group_by(included) %>%
  mutate(percent = round(100 * n / sum(n), 1))

# Step 6: Print everything
print(group_summary)
print(party_breakdown)
print(income_breakdown)
print(education_breakdown)
percent_female = mean(sex == "Female", na.rm = TRUE)
percent_female = mean(Sex == 1, na.rm = TRUE)
group_summary <- Original_Data %>%
  group_by(included) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    mean_ideology = mean(Political.scale, na.rm = TRUE),
    percent_female = mean(sex == 1, na.rm = TRUE),
    percent_nonwhite = mean(Race != "White", na.rm = TRUE)
  )

# Step 1: Count missing values per row in the original dataset
Original_Data$missing_count <- apply(Original_Data, 1, function(x) sum(is.na(x)))

# Step 2: Create "included" label based on number of NAs
Original_Data$included <- ifelse(Original_Data$missing_count > 10, "Excluded", "Included")

# Step 3: Convert to factor (optional but useful for grouped summaries)
Original_Data$included <- factor(Original_Data$included)

table(Original_Data$included)

library(dplyr)

# Grouped demographic summary
group_summary <- Original_Data %>%
  group_by(included) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    mean_ideology = mean(Political.scale, na.rm = TRUE),
    percent_female = mean(Sex == 1, na.rm = TRUE),
    percent_nonwhite = mean(Race != "White", na.rm = TRUE)
  )

# Categorical breakdowns
party_breakdown <- Original_Data %>%
  group_by(included, Party.registration) %>%
  summarise(n = n()) %>%
  group_by(included) %>%
  mutate(percent = round(n / sum(n) * 100, 1))

income_breakdown <- Original_Data %>%
  group_by(included, Income.Group) %>%
  summarise(n = n()) %>%
  group_by(included) %>%
  mutate(percent = round(n / sum(n) * 100, 1))

education_breakdown <- Original_Data %>%
  group_by(included, Education.Group) %>%
  summarise(n = n()) %>%
  group_by(included) %>%
  mutate(percent = round(n / sum(n) * 100, 1))

library(dplyr)
library(ggplot2)

# Make sure your breakdowns are loaded
party_breakdown <- Original_Data %>%
  group_by(included, Party.registration) %>%
  summarise(n = n()) %>%
  mutate(percent = round(100 * n / sum(n), 1),
         Category = "Party Registration",
         Group = Party.registration)

income_breakdown <- Original_Data %>%
  group_by(included, Income.Group) %>%
  summarise(n = n()) %>%
  mutate(percent = round(100 * n / sum(n), 1),
         Category = "Income Group",
         Group = Income.Group)

education_breakdown <- Original_Data %>%
  group_by(included, Education.Group) %>%
  summarise(n = n()) %>%
  mutate(percent = round(100 * n / sum(n), 1),
         Category = "Education Level",
         Group = Education.Group)

# Combine all into one data frame
combined_breakdown <- bind_rows(party_breakdown, income_breakdown, education_breakdown)

# Plot all three in one faceted plot
ggplot(combined_breakdown, aes(x = Group, y = percent, fill = included)) +
  geom_col(position = "dodge") +
  facet_wrap(~ Category, scales = "free_x") +
  labs(title = "Comparison of Included vs Excluded Respondents",
       y = "Percentage", x = NULL, fill = "Group") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Now create Income Group
Original_Data$Income.Group <- cut(
  Original_Data$income.numeric,
  breaks = c(-Inf, 2, 4, 6, Inf),
  labels = c("Low Income", "Lower-Middle Income", "Upper-Middle Income", "High Income"),
  right = TRUE
)

# Create Education Group
Original_Data$Education.Group <- cut(
  Original_Data$education.level.numeric,
  breaks = c(-Inf, 1.5, 2.5, 3.5, Inf),
  labels = c("No College", "Some College", "Bachelor's", "Grad Degree"),
  right = TRUE
)

# Convert to factors (optional, for plotting or tables)
Original_Data$Income.Group <- as.factor(Original_Data$Income.Group)
Original_Data$Education.Group <- as.factor(Original_Data$Education.Group)

# Convert to factors
Original_Data$Income.Group <- as.factor(Original_Data$Income.Group)
Original_Data$Education.Group <- as.factor(Original_Data$Education.Group)

# Step 3: Create age variable (if only year of birth is available)
Original_Data$age <- 2025 - Original_Data$Year.of.Birth

# Step 4: Summarize continuous and binary variables
demo_summary <- Original_Data %>%
  group_by(included) %>%
  summarise(
    n = n(),
    mean_age = mean(age, na.rm = TRUE),
    mean_ideology = mean(Political.scale, na.rm = TRUE),
    percent_female = mean(sex == "Female", na.rm = TRUE) * 100,
    percent_nonwhite = mean(Race != "White", na.rm = TRUE) * 100
  )

print(demo_summary)

# Step 5: Compare categorical variables: Party, Income, Education
party_summary <- Original_Data %>%
  filter(!is.na(Party.registration)) %>%
  group_by(included, Party.registration) %>%
  summarise(count = n()) %>%
  mutate(percent = round(100 * count / sum(count), 1))

income_summary <- Original_Data %>%
  filter(!is.na(Income.Group)) %>%
  group_by(included, Income.Group) %>%
  summarise(count = n()) %>%
  mutate(percent = round(100 * count / sum(count), 1))

education_summary <- Original_Data %>%
  filter(!is.na(Education.Group)) %>%
  group_by(included, Education.Group) %>%
  summarise(count = n()) %>%
  mutate(percent = round(100 * count / sum(count), 1))

# View each table
print(party_summary)
print(income_summary)
print(education_summary)

unique(tidy_model$term)

tidy_model$term <- recode(tidy_model$term,
                          "(Intercept)" = "(Intercept)",
                          "Political.scale" = "Political Ideology",
                          "age_centered" = "Age (Centered)",
                          "sex" = "Sex: Female",
                          "Race" = "Race (Nonwhite)",
                          
                          "Party.registrationDemocratic" = "Party: Democrat",
                          "Party.registrationIndependent" = "Party: Independent",
                          "Party.registrationNone" = "Party: None",
                          
                          "Education.GroupSome College" = "Education: Some College",
                          "Education.GroupBachelor's" = "Education: Bachelor's",
                          "Education.GroupGrad Degree" = "Education: Graduate Degree",
                          
                          "Income.GroupLow Income" = "Income: Low",
                          "Income.GroupLower-Middle Income" = "Income: Lower-Middle",
                          "Income.GroupUpper-Middle Income" = "Income: Upper-Middle",
                          "Income.GroupHigh Income" = "Income: High",
                          
                          "Political.scale:Education.GroupSome College" = "Ideology × Education: Some College",
                          "Political.scale:Education.GroupBachelor's" = "Ideology × Education: Bachelor's",
                          "Political.scale:Education.GroupGrad Degree" = "Ideology × Education: Graduate Degree",
                          
                          "Political.scale:Income.GroupLow Income" = "Ideology × Income: Low",
                          "Political.scale:Income.GroupLower-Middle Income" = "Ideology × Income: Lower-Middle",
                          "Political.scale:Income.GroupUpper-Middle Income" = "Ideology × Income: Upper-Middle",
                          "Political.scale:Income.GroupHigh Income" = "Ideology × Income: High"
)

custom_order <- c(
  "(Intercept)",
  "Political Ideology",
  "Age (Centered)",
  "Sex: Female",
  "Race (Nonwhite)",
  
  "Party: Democrat",
  "Party: Independent",
  "Party: None",
  
  "Education: Some College",
  "Education: Bachelor's",
  "Education: Graduate Degree",
  
  "Income: Low",
  "Income: Lower-Middle",
  "Income: Upper-Middle",
  "Income: High",
  
  "Ideology × Education: Some College",
  "Ideology × Education: Bachelor's",
  "Ideology × Education: Graduate Degree",
  
  "Ideology × Income: Low",
  "Ideology × Income: Lower-Middle",
  "Ideology × Income: Upper-Middle",
  "Ideology × Income: High"
)

Original_Data %>%
  filter(included == "Excluded") %>%
  summarise(
    missing_education = sum(is.na(Education.Group)),
    missing_income = sum(is.na(Income.Group)),
    missing_party = sum(is.na(Party.registration)),
    total_excluded = n()
  )

tidy_model <- tidy_model %>%
  filter(!is.na(term)) %>%
  mutate(term = factor(term, levels = custom_order))

# Convert Party.registration to an unordered factor
Merged_Data_Cleaned$Party.registration <- factor(Merged_Data_Cleaned$Party.registration)

# Relevel Party.registration so that Republican is the reference group
Merged_Data_Cleaned$Party.registration <- relevel(Merged_Data_Cleaned$Party.registration, ref = "Republican")

levels(Merged_Data_Cleaned$Party.registration)

# Re-create the plot with smaller font size and better label spacing
ggplot(coefficients_df, aes(x = Estimate, y = Predictor, xmin = Estimate - `Std. Error`, xmax = Estimate + `Std. Error`)) +
  geom_point() +
  geom_errorbarh(height = 0.2) +
  labs(title = "Coefficient Plot of Economic Populism Regression Model",
       x = "Estimate of Coefficients",
       y = "Predictors") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8, face = "bold", angle = 0, hjust = 1),  # Smaller font size for y-axis labels
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 15))  # Wrap y-axis labels for better readability

# First, filter out non-significant coefficients (p-value > 0.05)
coefficients_df <- coefficients_df[coefficients_df$`Pr(>|t|)` <= 0.05, ]

# Re-create the plot with smaller font size, better label spacing, and only significant coefficients
ggplot(coefficients_df, aes(x = Estimate, y = Predictor, xmin = Estimate - `Std. Error`, xmax = Estimate + `Std. Error`)) +
  geom_point() +
  geom_errorbarh(height = 0.2) +
  labs(title = "Coefficient Plot of Economic Populism Regression Model",
       x = "Estimate of Coefficients",
       y = "Predictors") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8, face = "bold", angle = 0, hjust = 1),  # Smaller font size for y-axis labels
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 15))  # Wrap y-axis labels for better readability

# Load the libraries
library(tm)
library(wordcloud)
library(RColorBrewer)

# Extract the open-ended responses (assuming your column is 'Why.Dems.fail.open')
open_responses <- Merged_Data_Cleaned$Why.Dems.fail.open

# Clean the text data
corpus <- Corpus(VectorSource(open_responses))  # Create a text corpus
corpus <- tm_map(corpus, content_transformer(tolower))  # Convert to lowercase
corpus <- tm_map(corpus, removePunctuation)  # Remove punctuation
corpus <- tm_map(corpus, removeNumbers)  # Remove numbers
corpus <- tm_map(corpus, removeWords, stopwords("en"))  # Remove common stop words
corpus <- tm_map(corpus, stripWhitespace)  # Remove extra whitespace

# Optional: Remove any other custom stopwords if needed
custom_stopwords <- c("some_custom_word", "another_word")
corpus <- tm_map(corpus, removeWords, custom_stopwords)
# Create a term-document matrix
tdm <- TermDocumentMatrix(corpus)

# Convert the term-document matrix to a matrix
m <- as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)  # Sort the terms by frequency
word_freq_table <- data.frame(word = names(word_freqs), freq = word_freqs)

# Generate the word cloud
set.seed(1234)  # For reproducibility
wordcloud(words = word_freq_table$word, 
          freq = word_freq_table$freq, 
          min.freq = 1,    # Minimum frequency to include
          scale = c(3, 0.5),  # Scaling range for word size
          colors = brewer.pal(8, "Dark2"),  # Color palette
          random.order = FALSE,   # Arrange words by frequency
          rot.per = 0.25)   # Word rotation percentage
# Adjust stopwords (keep essential words)
stopwords <- c("the", "and", "is", "of", "a", "to", "for", "in", "that", "on", "with", "this", "by")

# Create a term-document matrix and filter out stopwords
tdm <- TermDocumentMatrix(corpus, control = list(stopwords = stopwords))

# Convert term-document matrix to a matrix
m <- as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)  # Sort by frequency
word_freq_table <- data.frame(word = names(word_freqs), freq = word_freqs)

# Set minimum frequency and max words to include
min.freq <- 2  # Increase the minimum frequency if you see too many smaller words
max.words <- 100  # Adjust the number of words shown in the cloud

# Create the word cloud with adjusted settings
set.seed(1234)  # For reproducibility
wordcloud(words = word_freq_table$word, 
          freq = word_freq_table$freq, 
          min.freq = min.freq,
          scale = c(4, 0.5),  # Adjust scaling for more balanced display
          max.words = max.words,  # Limit the number of words
          colors = brewer.pal(8, "Dark2"),  # Use a color palette that is distinct
          random.order = FALSE,  # Arrange words by frequency
          rot.per = 0.25,  # Control rotation of words
          random.color = TRUE)  # Enable random coloring
# Install and load the necessary packages
library(wordcloud2)

# Adjust the margins before plotting
par(mar = c(1, 1, 3, 1))  # Adjust bottom, left, top, right margins

# Create the wordcloud again
wordcloud2(data = word_freq_table, size = 0.5, minSize = 15, color = "random-light", backgroundColor = "white")

library(wordcloud2)

# Word cloud data with adjusted parameters
wordcloud2(data = word_freq_table, 
           size = 0.8,  # Increase size for clarity
           minSize = 10,  # Ensure smaller words are included
           color = "random-light", 
           backgroundColor = "white",
           rotateRatio = 0.25)  # Slight rotation for better fitting
library(wordcloud2)

# Creating the word cloud with better margin settings
wordcloud2(data = word_freq_table, 
           size = 0.8, 
           color = "random-light", 
           backgroundColor = "white",
           rotateRatio = 0.3,  # Adjust rotation for more even word spread
           minSize = 10, 
           margin = 1)  # Reducing margin to ensure all words fit in
library(wordcloud2)

# Adjusting the wordcloud2 with better fitting size and rotation
wordcloud2(data = word_freq_table, 
           size = 0.8, 
           color = "random-light", 
           backgroundColor = "white",
           rotateRatio = 0.3,  # Rotation for better word spread
           minSize = 10)  # Minimum font size for words
# Define a list of stop words you want to remove
custom_stopwords <- c("the", "and", "to", "of", "a", "in", "for", "on", "with", "that", "this", "is", "it", "we", "you", "are", "be","republican", "voters", "area", "vote", "much", "make")

# Create the word cloud with custom stop words and adjusted margins
wordcloud2(data = word_freq_table, 
           size = 0.8, 
           color = "random-light", 
           backgroundColor = "white", 
           rotateRatio = 0.3,  # Adjust rotation for more even word spread
           minSize = 10, 
           wordStopWords = custom_stopwords,  # Remove custom stop words
           padding = 1)  # Adjust padding to better fit words in the cloud
# Define stop words
custom_stopwords <- c("the", "and", "to", "of", "a", "in", "for", "on", "with", "that", "this", "is", "it", "we", "you", "are", "be","republican", "voters", "area", "vote", "much", "make")

# Filter out stopwords from your word frequency table
word_freq_table_filtered <- word_freq_table[!(word_freq_table$word %in% custom_stopwords), ]

# Create the word cloud with custom stop words and adjusted margins
wordcloud2(data = word_freq_table_filtered, 
           size = 0.8, 
           color = "random-light", 
           backgroundColor = "white", 
           rotateRatio = 0.3,  # Adjust rotation for more even word spread
           minSize = 10) 
Merged_Data_Cleaned$Economic_Populism_Avg <- rowMeans(Merged_Data_Cleaned[c("Education..1.favorability", 
                                                                            "Taxes..1.favorability", 
                                                                            "Ag.2.favorability")], 
                                                      na.rm = TRUE)
model_populist <- lm(Economic_Populism_Avg ~ Political.scale + Rural.background + age + income + Education.Level + Race + sex, 
                     data = Merged_Data_Cleaned)

summary(model_populist)

# Linear regression using Working Class Focus as a predictor
model_with_working_class_focus <- lm(Economic_Populism_Avg ~ Political.scale + Working.class.focus + 
                                       age + income + Education.Level + Race + sex, data = Merged_Data_Cleaned)
summary(model_with_working_class_focus)

# Interaction between Working Class Focus and Political Ideology
model_with_interaction <- lm(Economic_Populism_Avg ~ Political.scale * Working.class.focus + 
                               age + income + Education.Level + Race + sex, data = Merged_Data_Cleaned)
summary(model_with_interaction)
# Calculate VIFs to check for multicollinearity
library(car)
vif(model_with_working_class_focus)
# Adding quadratic terms for age
model_with_quadratic <- lm(Economic_Populism_Avg ~ Political.scale + Rural.background + 
                             poly(age, 2) + income + Education.Level + Race + sex, 
                           data = Merged_Data_Cleaned)
summary(model_with_quadratic)
# Convert Education and Income to factor variables to use collapsed groups
Merged_Data_Cleaned$Education.Group <- as.factor(Merged_Data_Cleaned$Education.Group)
Merged_Data_Cleaned$Income.Group <- as.factor(Merged_Data_Cleaned$Income.Group)

# Linear regression model using collapsed Education and Income groups
model_collapsed_groups <- lm(Economic_Populism_Avg ~ Political.scale + 
                               Education.Group + Income.Group + 
                               Rural.background + 
                               age + Race + sex,
                             data = Merged_Data_Cleaned)

# Summary of the model
summary(model_collapsed_groups)

library(car)
vif(model_collapsed_groups)

model_collapsed_groups_interaction <- lm(Economic_Populism_Avg ~ Political.scale * Education.Group + 
                                           Income.Group * Political.scale + 
                                           Rural.background + 
                                           age + Race + sex,
                                         data = Merged_Data_Cleaned)
summary(model_collapsed_groups_interaction)
# Add Party.registration to the model and try removing non-significant variables
model_improved <- lm(Economic_Populism_Avg ~ Political.scale * Education.Group + 
                       Political.scale * Income.Group + 
                       age + Race + sex + Party.registration,
                     data = Merged_Data_Cleaned)

# Summary of the improved model
summary(model_improved)

library(car)
vif(model_improved)

# If necessary, consider centering or scaling continuous variables (age)
Merged_Data_Cleaned$age_centered <- scale(Merged_Data_Cleaned$age, center = TRUE, scale = TRUE)

# Rerun the model with centered age
model_improved_centered <- lm(Economic_Populism_Avg ~ Political.scale * Education.Group + 
                                Political.scale * Income.Group + 
                                age_centered + Race + sex + Party.registration, 
                              data = Merged_Data_Cleaned)

# Summary of the model with centered age
summary(model_improved_centered)

# Create a data frame of model coefficients and their standard errors
model_coefs <- summary(model_improved_centered)$coefficients
coefficients_df <- as.data.frame(model_coefs)
coefficients_df$Predictor <- rownames(coefficients_df)

# Remove intercept and make labels more readable
coefficients_df <- coefficients_df[!coefficients_df$Predictor %in% "(Intercept)", ]
coefficients_df$Predictor <- gsub("Education.Level", "Education Level", coefficients_df$Predictor)
coefficients_df$Predictor <- gsub("Income.Group", "Income Group", coefficients_df$Predictor)
coefficients_df$Predictor <- gsub("Political.scale", "Political Ideology", coefficients_df$Predictor)

# Create the coefficient plot
library(ggplot2)
ggplot(coefficients_df, aes(x = Estimate, y = reorder(Predictor, Estimate), xmin = Estimate - 1.96 * Std..Error, xmax = Estimate + 1.96 * Std..Error)) +
  geom_point() +
  geom_errorbarh(height = 0.2) +
  labs(title = "Coefficient Plot: Predictors of Economic Populism Favorability",
       x = "Coefficient Estimate (with 95% CI)",
       y = "Predictors") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10, face = "bold"))
colnames(coefficients_df)

ggplot(coefficients_df, aes(x = Estimate, y = reorder(Predictor, Estimate), xmin = Estimate - 1.96 * `Std. Error`, xmax = Estimate + 1.96 * `Std. Error`)) +
  geom_point() +
  geom_errorbarh(height = 0.2) +
  labs(title = "Coefficient Plot: Predictors of Economic Populism Favorability",
       x = "Coefficient Estimate (with 95% CI)",
       y = "Predictors") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10, face = "bold"))
# Load necessary libraries
library(dplyr)
library(tm)
library(wordcloud)
library(RColorBrewer)

# Pull the open response column
responses <- Merged_Data_Cleaned$Why.Dems.fail.open

# Clean and preprocess the text
corpus <- Corpus(VectorSource(responses))
corpus <- corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace)

# Create a document-term matrix
dtm <- TermDocumentMatrix(corpus)
matrix <- as.matrix(dtm)
word_freqs <- sort(rowSums(matrix), decreasing = TRUE)
df <- data.frame(word = names(word_freqs), freq = word_freqs)

# Create the word cloud
set.seed(123)
wordcloud(words = df$word,
          freq = df$freq,
          min.freq = 2,
          max.words = 100,
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))

library(ggplot2)

top_words <- df %>% top_n(15, freq)

ggplot(top_words, aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top Words from 'Why Democrats are Failing in Coshocton County'",
       x = "Words", y = "Frequency") +
  theme_minimal()
dems <- Merged_Data_Cleaned %>% filter(Party.registration == "Democrat")
reps <- Merged_Data_Cleaned %>% filter(Party.registration == "Republican")
inds <- Merged_Data_Cleaned %>% filter(Party.registration == "Independent")
nones <- Merged_Data_Cleaned %>% filter(Party.registration == "None")

library(tm)
library(ggplot2)
library(dplyr)

custom_stopwords <- c("party", "people", "democratic", "democrats", "republicans", 
                      "voters", "vote", "get", "dont", "think", "way", "needs", 
                      "many", "believe", "county", "coshocton", "rural")

analyze_open_response <- function(data, title_text) {
  responses <- data$Why.Dems.fail.open
  responses <- responses[!is.na(responses)]  # Remove missing
  
  corpus <- Corpus(VectorSource(responses))
  corpus <- corpus %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, c(stopwords("english"), custom_stopwords)) %>%
    tm_map(stripWhitespace)
  
  dtm <- TermDocumentMatrix(corpus)
  matrix <- as.matrix(dtm)
  word_freqs <- sort(rowSums(matrix), decreasing = TRUE)
  df <- data.frame(word = names(word_freqs), freq = word_freqs)
  
  top_words <- df %>% top_n(15, freq)
  
  ggplot(top_words, aes(x = reorder(word, freq), y = freq)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = title_text, x = "Words", y = "Frequency") +
    theme_minimal()
}
analyze_open_response(dems, "Why Democrats are Failing — Responses from Registered Democrats")
analyze_open_response(reps, "Why Democrats are Failing — Responses from Registered Republicans")
analyze_open_response(inds, "Why Democrats are Failing — Responses from Registered Independents")
analyze_open_response(nones, "Why Democrats are Failing — Responses from Non-Affiliated Respondents")

df <- data.frame(word = as.character(names(word_freqs)), freq = word_freqs)

library(dplyr)
library(tm)
library(ggplot2)
custom_stopwords <- c("party", "people", "democratic", "democrats", "republicans", 
                      "voters", "vote", "get", "dont", "think", "way", "needs", 
                      "many", "believe", "county", "coshocton", "rural")

dems <- Merged_Data_Cleaned %>% filter(Party.registration == "Democrat")
reps <- Merged_Data_Cleaned %>% filter(Party.registration == "Republican")
inds <- Merged_Data_Cleaned %>% filter(Party.registration == "Independent")
nones <- Merged_Data_Cleaned %>% filter(Party.registration == "None")
analyze_open_response <- function(data, title_text) {
  responses <- data$Why.Dems.fail.open
  responses <- responses[!is.na(responses)]  # Remove missing
  
  if (length(responses) == 0) {
    print(paste("No responses for:", title_text))
    return(NULL)
  }
  
  corpus <- Corpus(VectorSource(responses))
  corpus <- corpus %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, c(stopwords("english"), custom_stopwords)) %>%
    tm_map(stripWhitespace)
  
  dtm <- TermDocumentMatrix(corpus)
  matrix <- as.matrix(dtm)
  word_freqs <- sort(rowSums(matrix), decreasing = TRUE)
  
  if (length(word_freqs) == 0) {
    print(paste("No words left after cleaning for:", title_text))
    return(NULL)
  }
  
  df <- data.frame(word = as.character(names(word_freqs)), freq = word_freqs)
  
  top_words <- df %>%
    top_n(15, freq) %>%
    mutate(word = as.character(word))
  
  ggplot(top_words, aes(x = reorder(word, freq), y = freq)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = title_text, x = "Words", y = "Frequency") +
    theme_minimal()
}
analyze_open_response(dems, "Why Democrats are Failing — Registered Democrats")
analyze_open_response(reps, "Why Democrats are Failing — Registered Republicans")

backup_path <- file.path("/Users/baileybowman/Desktop", "ThesisScript.R")

file.copy("ThesisScript.R", backup_path)
file.copy("/Users/baileybowman/Desktop/CleanThesisScript.R", "CleanThesisScript.R")
list.files("/Users/baileybowman/Desktop")
file.copy("/Users/baileybowman/Desktop/CleanThesisScript copy.R", "CleanThesisScript.R")

# Filter for Republicans
republicans <- Merged_Data_Cleaned[Merged_Data_Cleaned$Party.registration == "Republican", ]

# Count how many Republicans never chose any of the working-class candidates
no_working_class_choices <- sum(
  republicans$Teacher == 0 &
    republicans$Mechanic == 0 &
    republicans$Veteran == 0,
  na.rm = TRUE
)

# Print the result
no_working_class_choices

# Total number of Republicans
total_republicans <- nrow(republicans)

# Percentage
percent_no_working_class <- (no_working_class_choices / total_republicans) * 100

# Print the percentage
percent_no_working_class

# Filter for Republicans
republicans <- Merged_Data_Cleaned[Merged_Data_Cleaned$Party.registration == "Republican", ]

# Sum how many working-class candidates each Republican selected
republicans$working_class_total <- rowSums(republicans[, c("Teacher", "Mechanic", "Veteran")], na.rm = TRUE)

# Filter to only those who selected at least one working-class candidate
republicans_with_one_or_more <- republicans[republicans$working_class_total > 0, ]

# Calculate the average number selected
average_selected <- mean(republicans_with_one_or_more$working_class_total, na.rm = TRUE)

# Print the result
average_selected

# Filter for Republicans
republicans <- Merged_Data_Cleaned[Merged_Data_Cleaned$Party.registration == "Republican", ]

# Create a column that sums the working-class candidate selections
republicans$working_class_total <- rowSums(republicans[, c("Teacher", "Mechanic", "Veteran")], na.rm = TRUE)

# Calculate the average number selected (including zero)
average_selected_all <- mean(republicans$working_class_total, na.rm = TRUE)

# Print result
average_selected_all
# First, create the working-class total column (if not already done)
Merged_Data_Cleaned$working_class_total <- rowSums(
  Merged_Data_Cleaned[, c("Teacher", "Mechanic", "Veteran")], na.rm = TRUE
)

# Calculate the average for each party group
avg_by_party <- tapply(
  Merged_Data_Cleaned$working_class_total,
  Merged_Data_Cleaned$Party.registration,
  mean,
  na.rm = TRUE
)

# Print the result
avg_by_party

# Calculate mean favorability scores for Republicans, converting to numeric
mean_edu <- mean(as.numeric(republicans$Education..1.favorability), na.rm = TRUE)
mean_tax <- mean(as.numeric(republicans$Taxes..1.favorability), na.rm = TRUE)
mean_ag  <- mean(as.numeric(republicans$Ag.2.favorability), na.rm = TRUE)

# Combine into a named vector to view results together
econ_populist_means <- c(
  Education = mean_edu,
  Taxes = mean_tax,
  Agriculture = mean_ag
)

econ_populist_means

table(republicans$Agriculture.2.favorability, useNA = "always")
table(Merged_Data_Cleaned$Agriculture.2.favorability, useNA = "always")
colnames(Merged_Data_Cleaned)

# Expanded Kitchen Sink Regression for Taxes Economic Populist Message Favorability
model_tax_favorability_expanded <- lm(Taxes..1.favorability ~ 
                                        Political.scale + 
                                        income.numeric + 
                                        Education.Level + 
                                        Race + 
                                        sex + 
                                        Ag.2.favorability + 
                                        Working.class.focus + 
                                        Rural.background + 
                                        Rural.priority + 
                                        Mechanic + 
                                        Split.ticket.voting + 
                                        Party.registration + 
                                        Veteran + 
                                        Employment + 
                                        Wealth_inequality + 
                                        Healthcare + 
                                        Teacher + 
                                        Unions + 
                                        Ag_investments + 
                                        Good_job,
                                      data = Merged_Data_Cleaned)
 
# Show the summary of the model
summary(model_tax_favorability_expanded)

# Example of visualizing predicted values
predicted_values <- predict(model_tax_favorability_expanded, newdata = Merged_Data_Cleaned)

# Plotting predicted favorability against political ideology
ggplot(Merged_Data_Cleaned, aes(x = Political.scale, y = predicted_values)) +
  geom_point(aes(color = Income.Group), alpha = 0.6) +
  labs(title = "Predicted Favorability for Taxes Economic Populist Message by Political Ideology and Income Group",
       x = "Political Ideology",
       y = "Predicted Favorability",
       color = "Income Group") +
  theme_minimal()

# Make sure ggplot2 is loaded
library(ggplot2)

# Now you can create the plot
ggplot(Merged_Data_Cleaned, aes(x = Political.scale, y = predicted_values, color = income.numeric)) +
  geom_line() +
  labs(title = "Predicted Favorability for Taxes Economic Populist Message by Political Ideology and Income Group",
       x = "Political Ideology",
       y = "Predicted Favorability",
       color = "Income Group") +
  theme_minimal()

# Average score for Travels.to.rural in the entire dataset
mean_travels_to_rural <- mean(Merged_Data_Cleaned$Travels.to.rural, na.rm = TRUE)
cat("Average score for Travels.to.rural in the entire dataset: ", round(mean_travels_to_rural, 2), "\n")

# Average score for Travels.to.rural for just Republicans
mean_travels_to_rural_republicans <- mean(Merged_Data_Cleaned$Travels.to.rural[Merged_Data_Cleaned$Self.identified.partisanship == "Republican"], na.rm = TRUE)
cat("Average score for Travels.to.rural for Republicans: ", round(mean_travels_to_rural_republicans, 2), "\n")

# Average score for Travels.to.rural for Independents or non-affiliated voters (not Democrat or Republican)
mean_travels_to_rural_independents <- mean(Merged_Data_Cleaned$Travels.to.rural[Merged_Data_Cleaned$Self.identified.partisanship != "Democrat" & Merged_Data_Cleaned$Self.identified.partisanship != "Republican"], na.rm = TRUE)
cat("Average score for Travels.to.rural for Independents or non-affiliated voters: ", round(mean_travels_to_rural_independents, 2), "\n")

# Average political ideology score for Republicans
mean_political_ideology_republicans <- mean(Merged_Data_Cleaned$Political.scale[Merged_Data_Cleaned$Self.identified.partisanship == "Republican"], na.rm = TRUE)
cat("Average political ideology score for Republicans: ", round(mean_political_ideology_republicans, 2), "\n")

# Average political ideology score for Independents or non-affiliated voters
mean_political_ideology_independents <- mean(Merged_Data_Cleaned$Political.scale[Merged_Data_Cleaned$Self.identified.partisanship == "Independent" | Merged_Data_Cleaned$Self.identified.partisanship == "No preference"], na.rm = TRUE)
cat("Average political ideology score for Independents or non-affiliated voters: ", round(mean_political_ideology_independents, 2), "\n")

# Filter for Republicans and calculate the mode of Political.scale
republicans_data <- Merged_Data_Cleaned[Merged_Data_Cleaned$Self.identified.partisanship == "Republican", ]

# Function to calculate the mode
get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

# Calculate the mode of Political.scale for Republicans
mode_political_ideology_republicans <- get_mode(republicans_data$Political.scale)

cat("Most common political ideology score (mode) for Republicans: ", mode_political_ideology_republicans, "\n")

# Filter for Republicans
republicans_data <- Merged_Data_Cleaned[Merged_Data_Cleaned$Self.identified.partisanship == "Republican", ]

# View the frequency of willingness to split ticket vote (Split.ticket.voting) for Republicans
table(republicans_data$Split.ticket.voting)

# Filter for Republicans
republicans_data <- Merged_Data_Cleaned[Merged_Data_Cleaned$Self.identified.partisanship == "Republican", ]

# View how many Republicans answered 'Yes' to voting for a Democrat
table(republicans_data$Voted.for.Dem)

# Filter for Republicans
republicans_data <- Merged_Data_Cleaned[Merged_Data_Cleaned$Self.identified.partisanship == "Republican", ]

# View frequency of Republicans who voted for a Democrat before
table(republicans_data$Voted.for.Dem.when)

# Example: Reload the unfiltered data if you saved it
Merged_Data_Unfiltered <- read.csv("Merged_Data.csv")

sum(rowSums(is.na(Merged_Data_Cleaned)) >= 40)  # number of rows being dropped

Merged_Data_Cleaned <- Merged_Data_Cleaned[rowSums(is.na(Merged_Data_Cleaned)) < 40, ]

library(ggplot2)

ggplot(Merged_Data_Cleaned %>% filter(!is.na(income)), aes(x = income)) +
  geom_bar(fill = "mediumpurple") +
  labs(
    title = "Household Income Distribution of Survey Participants",
    x = "Income Bracket",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))

# Define your desired order of income brackets
income_levels_ordered <- c(
  "Less than $10,000",
  "$10,000 to $19,999",
  "$20,000 to $29,999",
  "$30,000 to $39,999",
  "$40,000 to $49,999",
  "$50,000 to $59,999",
  "$60,000 to $69,999",
  "$70,000 to $79,999",
  "$80,000 to $89,999",
  "$90,000 to $99,999",
  "$100,000 to $149,999",
  "$150,000 or more"
)

# Convert income to an ordered factor
Merged_Data_Cleaned$income <- factor(
  Merged_Data_Cleaned$income,
  levels = income_levels_ordered,
  ordered = TRUE
)

ggplot(Merged_Data_Cleaned %>% filter(!is.na(income)), aes(x = income)) +
  geom_bar(fill = "mediumpurple") +
  labs(
    title = "Household Income Distribution of Survey Participants",
    x = "Income Bracket",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))

library(dplyr)
library(tidyr)

# Step 1: Create a smaller dataframe with just the two variables of interest
subset_data <- Merged_Data_Cleaned %>%
  select(Party.registration, Self.identified.partisanship)

# Step 2: Reshape the data to long format
party_long <- subset_data %>%
  pivot_longer(cols = everything(),
               names_to = "Partisan_Type",
               values_to = "Party") %>%
  filter(!is.na(Party))  # Optional: removes rows with NA party values

library(tidyr)
library(dplyr)

party_long <- subset_data %>%
  select(Party.registration, Self.identified.partisanship) %>%
  pivot_longer(cols = everything(), names_to = "Partisan_Type", values_to = "Party") %>%
  filter(!is.na(Party))

library(dplyr)
library(tidyr)
library(ggplot2)

# Step 1: Create and reshape the dataset
subset_data <- Merged_Data_Cleaned %>%
  select(Party.registration, Self.identified.partisanship)

party_long <- subset_data %>%
  pivot_longer(cols = everything(), names_to = "Partisan_Type", values_to = "Party") %>%
  filter(!is.na(Party)) %>%
  mutate(
    Party = case_when(
      Party %in% c("Democrat", "Democratic") ~ "Democrat",
      Party %in% c("None", "No preference", "Other") ~ "Unaffiliated/Other",
      TRUE ~ Party
    )

# Step 2: Plot
ggplot(party_long, aes(x = Party, fill = Partisan_Type)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Party Registration vs. Self-Identified Partisanship",
    x = "Party Affiliation",
    y = "Count",
    fill = "Reported As"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(dplyr)
library(scales)

# Get percent breakdown for Self-identified partisanship
party_percentages <- party_long %>%
  filter(Partisan_Type == "Self.identified.partisanship") %>%
  count(Party) %>%
  mutate(
    Percent = n / sum(n) * 100,
    Label = paste0(Party, " (", round(Percent, 2), "%)")
  )

party_percentages

library(dplyr)

# Use the version where Party labels are already cleaned
self_id_percentages <- party_long %>%
  filter(Partisan_Type == "Self.identified.partisanship") %>%
  count(Party) %>%
  mutate(
    Percent = round(n / sum(n) * 100, 2),
    Label = paste0(Party, " (", Percent, "%)")
  )

self_id_percentages

subset_data <- subset_data %>%
  mutate(
    party_strength_leaning = case_when(
      Republican.Strength == 1 ~ "Strong Republican",
      Republican.Strength == 0 ~ "Not Very Strong Republican",
      Democrat.strength == 1 ~ "Strong Democrat",
      Democrat.strength == 0 ~ "Not Very Strong Democrat",
      Closer.to.Republican.or.Democrat == 1 ~ "Leans Republican",
      Closer.to.Republican.or.Democrat == 0 ~ "Leans Democrat",
      TRUE ~ "Other/Unknown"
    )

ggplot(subset_data, aes(x = Self.identified.partisanship, fill = party_strength_leaning)) +
  geom_bar(position = "fill") +
  labs(title = "Party Strength and Leaning Breakdown",
       x = "Self-Identified Partisanship",
       y = "Proportion",
       fill = "Party Strength / Leaning") +
  theme_minimal()

names(subset_data)

subset_data <- subset_data %>%
  mutate(
    party_strength_leaning = case_when(
      Republican.Strength == 1 ~ "Strong Republican",
      Republican.Strength == 0 ~ "Not Very Strong Republican",
      Democrat.strength == 1 ~ "Strong Democrat",
      Democrat.strength == 0 ~ "Not Very Strong Democrat",
      Closer.to.Republican.or.Democrat == 1 ~ "Leans Republican",
      Closer.to.Republican.or.Democrat == 0 ~ "Leans Democrat",
      TRUE ~ "Other/Unknown"
    )

ggplot(subset_data, aes(x = Self.identified.partisanship, fill = party_strength_leaning)) +
  geom_bar(position = "fill") +
  labs(title = "Party Strength and Leaning Breakdown",
       x = "Self-Identified Partisanship",
       y = "Proportion",
       fill = "Party Strength / Leaning") +
  theme_minimal()

subset_data <- Merged_Data_Cleaned %>%
  select(Self.identified.partisanship,
         Republican.Strength,
         Democrat.strength,
         Closer.to.Republican.or.Democrat)

subset_data <- subset_data %>%
  mutate(
    party_strength_leaning = case_when(
      Republican.Strength == 1 ~ "Strong Republican",
      Republican.Strength == 0 ~ "Not Very Strong Republican",
      Democrat.strength == 1 ~ "Strong Democrat",
      Democrat.strength == 0 ~ "Not Very Strong Democrat",
      Closer.to.Republican.or.Democrat == 1 ~ "Leans Republican",
      Closer.to.Republican.or.Democrat == 0 ~ "Leans Democrat",
      TRUE ~ "Other/Unknown"
    )

ggplot(subset_data, aes(x = Self.identified.partisanship, fill = party_strength_leaning)) +
  geom_bar(position = "fill") +
  labs(title = "Party Strength and Leaning Breakdown",
       x = "Self-Identified Partisanship",
       y = "Proportion",
       fill = "Party Strength / Leaning") +
  theme_minimal()

names(full_data)
names(Merged_Data_Cleaned)

library(dplyr)
library(ggplot2)
library(scales)

# Step 1: Create a working dataset with the correct fields
subset_data <- Merged_Data_Cleaned %>%
  select(Self.identified.partisanship,
         Republican.Strength,
         Democrat.strength,
         Closer.to.Republican.or.Democrat) %>%
  mutate(
    party_strength_leaning = case_when(
      Democrat.strength == 1 ~ "Strong Democrat",
      Democrat.strength == 0 ~ "Not Very Strong Democrat",
      Republican.Strength == 1 ~ "Strong Republican",
      Republican.Strength == 0 ~ "Not Very Strong Republican",
      Closer.to.Republican.or.Democrat == 0 ~ "Closer to Democrat",
      Closer.to.Republican.or.Democrat == 1 ~ "Closer to Republican",
      TRUE ~ "Other/Unknown"
    )

# Optional: Filter to major partisan self-ID groups
subset_data <- subset_data %>%
  filter(Self.identified.partisanship %in% c("Democrat", "Independent", "Republican"))

# Step 2: Create the plot
ggplot(subset_data, aes(x = Self.identified.partisanship, fill = party_strength_leaning)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Party Strength and Leaning Breakdown",
    x = "Self-Identified Partisanship",
    y = "Proportion",
    fill = "Party Strength / Leaning"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

table(Merged_Data_Cleaned$Republican.Strength, useNA = "always")
table(Merged_Data_Cleaned$Democrat.strength, useNA = "always")
table(Merged_Data_Cleaned$Closer.to.Republican.or.Democrat, useNA = "always")

library(dplyr)

subset_data <- Merged_Data_Cleaned %>%
  select(Self.identified.partisanship,
         Republican.Strength,
         Democrat.strength,
         Closer.to.Republican.or.Democrat) %>%
  mutate(
    party_strength_leaning = case_when(
      Democrat.strength == "Strong" ~ "Strong Democrat",
      Democrat.strength == "Not very strong" ~ "Not Very Strong Democrat",
      Republican.Strength == "Strong" ~ "Strong Republican",
      Republican.Strength == "Not very strong" ~ "Not Very Strong Republican",
      Closer.to.Republican.or.Democrat == "Democratic" ~ "Closer to Democrat",
      Closer.to.Republican.or.Democrat == "Republican" ~ "Closer to Republican",
      TRUE ~ "Other/Unknown"
    )
  ) %>%
  filter(Self.identified.partisanship %in% c("Democrat", "Independent", "Republican"))

library(ggplot2)
library(scales)

ggplot(subset_data, aes(x = Self.identified.partisanship, fill = party_strength_leaning)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Party Strength and Leaning Breakdown",
    x = "Self-Identified Partisanship",
    y = "Proportion",
    fill = "Party Strength / Leaning"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

subset_data <- subset_data %>%
  filter(party_strength_leaning != "Other/Unknown")

custom_colors <- c(
  "Strong Democrat" = "#377eb8",         # muted blue
  "Not Very Strong Democrat" = "#a6cee3", # light blue
  "Closer to Democrat" = "#fdbf6f",       # orange
  "Closer to Republican" = "#ff7f00",     # darker orange
  "Not Very Strong Republican" = "#fb9a99", # light red
  "Strong Republican" = "#e31a1c"         # strong red
)
library(ggplot2)
library(scales)

ggplot(subset_data, aes(x = Self.identified.partisanship, fill = party_strength_leaning)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = custom_colors) +
  labs(
    title = "Party Strength and Leaning Breakdown",
    x = "Self-Identified Partisanship",
    y = "Proportion",
    fill = "Party Strength / Leaning"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )

library(dplyr)
library(ggplot2)
library(scales)

# Re-create the subset_data (only keeping expected group combinations)
subset_data <- Merged_Data_Cleaned %>%
  select(Self.identified.partisanship,
         Republican.Strength,
         Democrat.strength,
         Closer.to.Republican.or.Democrat) %>%
  mutate(
    party_strength_leaning = case_when(
      Self.identified.partisanship == "Democrat" & Democrat.strength == "Strong" ~ "Strong Democrat",
      Self.identified.partisanship == "Democrat" & Democrat.strength == "Not very strong" ~ "Not Very Strong Democrat",
      Self.identified.partisanship == "Republican" & Republican.Strength == "Strong" ~ "Strong Republican",
      Self.identified.partisanship == "Republican" & Republican.Strength == "Not very strong" ~ "Not Very Strong Republican",
      Self.identified.partisanship == "Independent" & Closer.to.Republican.or.Democrat == "Democratic" ~ "Closer to Democrat",
      Self.identified.partisanship == "Independent" & Closer.to.Republican.or.Democrat == "Republican" ~ "Closer to Republican",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(party_strength_leaning))  # Drop unmatched/misfit combos

# Reorder fill levels for legend grouping
subset_data$party_strength_leaning <- factor(subset_data$party_strength_leaning, levels = c(
  "Strong Democrat",
  "Not Very Strong Democrat",
  "Closer to Democrat",
  "Closer to Republican",
  "Not Very Strong Republican",
  "Strong Republican"
))

# Custom colors for clean grouping
custom_colors <- c(
  "Strong Democrat" = "#377eb8",
  "Not Very Strong Democrat" = "#a6cee3",
  "Closer to Democrat" = "#fdbf6f",
  "Closer to Republican" = "#ff7f00",
  "Not Very Strong Republican" = "#fb9a99",
  "Strong Republican" = "#e31a1c"
)

# Final Plot
ggplot(subset_data, aes(x = Self.identified.partisanship, fill = party_strength_leaning)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = custom_colors) +
  labs(
    title = "Party Strength and Leaning Breakdown",
    x = "Self-Identified Partisanship",
    y = "Proportion",
    fill = "Party Strength / Leaning"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )

subset_data$party_strength_leaning <- factor(
  subset_data$party_strength_leaning,
  levels = c(
    "Not Very Strong Democrat",
    "Strong Democrat",
    "Closer to Democrat",
    "Closer to Republican",
    "Not Very Strong Republican",
    "Strong Republican"
  )

ggplot(subset_data, aes(x = Self.identified.partisanship, fill = party_strength_leaning)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = custom_colors) +
  labs(
    title = "Party Strength and Leaning Breakdown",
    x = "Self-Identified Partisanship",
    y = "Proportion",
    fill = "Party Strength / Leaning"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )

Merged_Data_Cleaned <- Merged_Data_Cleaned %>%
  mutate(age = 2025 - Year.of.Birth)

                     data = Merged_Data_Cleaned,
                     family = binomial)
Education.Level

Merged_Data_Cleaned <- Merged_Data_Cleaned %>%
  mutate(
    Education.Level = factor(Education.Level, levels = c(
      "Less than high school degree",
      "High school graduate (high school diploma or equivalent including GED)",
      "Some college but no degree",
      "Associate degree in college (2-year)",
      "Bachelor's degree in college (4-year)",
      "Master's degree",
      "Professional degree (JD, MD)",
      "Doctoral degree"
    ), ordered = TRUE),
    education.level.numeric = as.numeric(Education.Level)
  )
                     data = Merged_Data_Cleaned,
                     family = binomial)

Merged_Data_Cleaned <- Merged_Data_Cleaned %>%
  mutate(
    income = factor(income, levels = c(
      "Less than $10,000",
      "$10,000 to $19,999",
      "$20,000 to $29,999",
      "$30,000 to $39,999",
      "$40,000 to $49,999",
      "$50,000 to $59,999",
      "$60,000 to $69,999",
      "$70,000 to $79,999",
      "$80,000 to $89,999",
      "$90,000 to $99,999",
      "$100,000 to $149,999",
      "$150,000 or more"
    ), ordered = TRUE),
    income.numeric = as.numeric(income)
  )

                     data = Merged_Data_Cleaned,
                     family = binomial)
nobs(model_teacher)
nobs(model_veteran)
nobs(model_mechanic)

Merged_Data_Cleaned %>%
  summarize(
    avg_age = mean(age, na.rm = TRUE),
    avg_income = mean(as.numeric(income), na.rm = TRUE),
    avg_ideology = mean(Political.scale, na.rm = TRUE)
  )

summary(Merged_Data_Cleaned$Party.registration)

Merged_Data_Cleaned <- Merged_Data_Cleaned %>%
  mutate(Income.Group = case_when(
    income %in% c(
      "Less than $10,000",
      "$10,000 to $19,999"
    ) ~ "Low",
    
    income %in% c(
      "$20,000 to $29,999",
      "$30,000 to $39,999",
      "$40,000 to $49,999"
    ) ~ "Lower-Middle",
    
    income %in% c(
      "$50,000 to $59,999",
      "$60,000 to $69,999",
      "$70,000 to $79,999",
      "$80,000 to $89,999"
    ) ~ "Upper-Middle",
    
    income %in% c(
      "$90,000 to $99,999",
      "$100,000 to $149,999",
      "$150,000 or more"
    ) ~ "High",
    
    TRUE ~ NA_character_
  ),
  Income.Group = factor(Income.Group, levels = c("Low", "Lower-Middle", "Upper-Middle", "High"))
  )
                            data = Merged_Data_Cleaned,
                            family = binomial)
library(ggeffects)

preds_teacher_income <- ggpredict(model_teacher_income, terms = c("Political.scale", "Income.Group"))

ggplot(preds_teacher_income, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  labs(
    title = "Predicted Probability of Selecting Teacher Candidate by Political Ideology and Income Group",
    x = "Political Ideology (0 = Liberal, 7 = Conservative)",
    y = "Predicted Probability",
    color = "Income Group",
    fill = "Income Group"
  ) +
  theme_minimal() +
  theme(legend.position = "top")
# Step 1: Calculate the mean populist favorability for each respondent
Merged_Data_Cleaned$mean_populist_favorability <- rowMeans(
  Merged_Data_Cleaned[, c("Education..1.favorability", 
                          "Taxes..1.favorability", 
                          "Ag.2.favorability")],
  na.rm = TRUE
)

# Step 2: Group by partisanship and calculate the average
library(dplyr)

Merged_Data_Cleaned %>%
  group_by(partisan_simple) %>%
  summarize(
    avg_populist_favorability = mean(mean_populist_favorability, na.rm = TRUE),
    count = n()
  )
# Step 1: Create a new variable with the average favorability across the 3 economic populist messages
Merged_Data_Cleaned$mean_populist_favorability <- rowMeans(
  Merged_Data_Cleaned[, c("Education..1.favorability", 
                          "Taxes..1.favorability", 
                          "Ag.2.favorability")],
  na.rm = TRUE
)

# Step 2: Group by Party.registration and summarize the average and count
library(dplyr)

Merged_Data_Cleaned %>%
  group_by(Party.registration) %>%
  summarize(
    avg_populist_favorability = mean(mean_populist_favorability, na.rm = TRUE),
    count = n()
  )
install.'psych'
library(psych)
alpha(Merged_Data_Cleaned[, c("Education..1.favorability", 
                              "Taxes..1.favorability", 
                              "Ag.2.favorability")])
library(ggplot2)
library(dplyr)

# Recode reasons for cleaner display (if not already done)
reason_labels <- c(
  "Out_of_touch" = "The Democratic Party is out of touch with the needs of rural voters",
  "Lack_of_effort" = "The Democratic Party does not make an effort to connect with rural voters",
  "Pronouns" = "The Democratic Party is too concerned with identity politics like trans kids in sports and pronouns",
  "Other" = "A reason not listed above",
  "Cities" = "The Democratic Party only cares about cities"
)

# Pivot data to long format
reason_data <- Merged_Data_Cleaned %>%
  select(Out_of_touch, Lack_of_effort, Pronouns, Other, Cities) %>%
  pivot_longer(everything(), names_to = "Reason", values_to = "Selected") %>%
  filter(Selected == 1) %>%
  count(Reason) %>%
  mutate(Reason = recode(Reason, !!!reason_labels))  # Replace with readable labels

# Plot with prettier colors
ggplot(reason_data, aes(x = reorder(Reason, n), y = n, fill = Reason)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +  # You can also try "Set3", "Dark2", "Paired"
  labs(
    title = "Frequency of Reasons for Democratic Party Shortcomings",
    x = "Reason",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 13)
  )

# View the column names
names(Merged_Data_Cleaned)

unique_vals <- lapply(Merged_Data_Cleaned[, c("Out_of_touch", "Lack_of_effort", "Pronouns", "Other", "Cities")], unique)

# Display the results nicely
for (col in names(unique_vals)) {
  cat("\nColumn:", col, "\n")
  print(unique_vals[[col]])
}

library(dplyr)
library(ggplot2)
library(stringr)

# Step 2: Count reasons
reason_counts <- reasons_data %>%
  group_by(Why.Dems.fail.ranked) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency))

# Step 3: Remove "please type your thoughts below:"
reason_counts <- reason_counts %>%
  filter(Why.Dems.fail.ranked != "please type your thoughts below:")

# Step 4: Create and wrap Reason labels
reason_counts <- reason_counts %>%
  mutate(Reason = str_wrap(Why.Dems.fail.ranked, width = 45),
         Reason = factor(Reason, levels = rev(Reason)))  # reverse order for nicer plot

# Step 5: Plot
ggplot(reason_counts, aes(x = Frequency, y = Reason, fill = Reason)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Frequency of Reasons for Democratic Party Shortcomings",
    x = "Frequency",
    y = "Reason"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(face = "bold")
  ) +
  scale_fill_brewer(palette = "Set2")

library(ggplot2)

# Filter out NA values
Merged_Data_Cleaned_filtered <- Merged_Data_Cleaned %>%
  filter(!is.na(income))

# Plot histogram without NA column
ggplot(Merged_Data_Cleaned_filtered, aes(x = income)) +
  geom_bar(fill = "mediumpurple", color = "black") +
  labs(
    title = "Household Income of Survey Participants",
    x = "Income Bracket",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

library(ggplot2)
library(dplyr)

# Define the correct income order
income_levels <- c(
  "Less than $10,000",
  "$10,000 to $19,999",
  "$20,000 to $29,999",
  "$30,000 to $39,999",
  "$40,000 to $49,999",
  "$50,000 to $59,999",
  "$60,000 to $69,999",
  "$70,000 to $79,999",
  "$80,000 to $89,999",
  "$90,000 to $99,999",
  "$100,000 to $149,999",
  "$150,000 or more"
)

# Remove NAs and fix factor order
Merged_Data_Cleaned_filtered <- Merged_Data_Cleaned %>%
  filter(!is.na(income)) %>%
  mutate(income = factor(income, levels = income_levels, ordered = TRUE))

# Plot
ggplot(Merged_Data_Cleaned_filtered, aes(x = income)) +
  geom_bar(fill = "mediumpurple", color = "black") +
  labs(
    title = "Household Income of Survey Participants",
    x = "Income Bracket",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
table(Merged_Data_Cleaned$Political.scale, Merged_Data_Cleaned$Mechanic)
Merged_Data_Cleaned %>%
  group_by(Political.scale) %>%
  summarise(
    total = n(),
    chose_mechanic = sum(Mechanic == 1, na.rm = TRUE),
    prop_mechanic = chose_mechanic / total
  )
library(ggplot2)
library(ggeffects)
library(dplyr)

# Simpler models for each candidate without covariates to avoid overfitting
model_mechanic_simple <- glm(Mechanic ~ Political.scale, 
                             data = Merged_Data_Cleaned, family = binomial)
model_teacher_simple <- glm(Teacher ~ Political.scale, 
                            data = Merged_Data_Cleaned, family = binomial)
model_veteran_simple <- glm(Veteran ~ Political.scale, 
                            data = Merged_Data_Cleaned, family = binomial)

# Generate predictions
preds_mechanic <- ggpredict(model_mechanic_simple, terms = "Political.scale [0:7]")
preds_teacher <- ggpredict(model_teacher_simple, terms = "Political.scale [0:7]")
preds_veteran <- ggpredict(model_veteran_simple, terms = "Political.scale [0:7]")

# Label candidates
preds_mechanic$Candidate <- "Mechanic"
preds_teacher$Candidate <- "Teacher"
preds_veteran$Candidate <- "Veteran"

# Combine predictions
preds_all <- bind_rows(preds_mechanic, preds_teacher, preds_veteran)

# Plot faceted predicted probabilities
ggplot(preds_all, aes(x = x, y = predicted, color = Candidate, fill = Candidate)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  facet_wrap(~Candidate) +
  labs(
    title = "Predicted Probability of Selecting Each Candidate by Political Ideology",
    x = "Political Ideology (0 = Liberal, 7 = Conservative)",
    y = "Predicted Probability of Candidate Selection"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

library(ggplot2)
library(ggeffects)

# Refit your models with Political.scale + covariates
                      data = Merged_Data_Cleaned, family = binomial)

# Generate predicted probabilities by ideology
pred_mechanic <- ggpredict(model_mechanic, terms = "Political.scale")
pred_mechanic$Candidate <- "Mechanic"

pred_teacher <- ggpredict(model_teacher, terms = "Political.scale")
pred_teacher$Candidate <- "Teacher"

pred_veteran <- ggpredict(model_veteran, terms = "Political.scale")
pred_veteran$Candidate <- "Veteran"

# Combine
all_preds <- rbind(pred_mechanic, pred_teacher, pred_veteran)

# Plot all candidates in one graph
ggplot(all_preds, aes(x = x, y = predicted, color = Candidate, fill = Candidate)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, linetype = 0) +
  labs(
    title = "Predicted Probability of Selecting Each Candidate by Political Ideology",
    x = "Political Ideology (0 = Liberal, 7 = Conservative)",
    y = "Predicted Probability of Candidate Selection"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top") +
  scale_color_manual(values = c("Mechanic" = "red", "Teacher" = "green4", "Veteran" = "blue")) +
  scale_fill_manual(values = c("Mechanic" = "red", "Teacher" = "green4", "Veteran" = "blue"))

library(logistf)

model_mechanic_firth <- logistf(Mechanic ~ Political.scale + age + income + Education.Level + Race + sex, 
                                data = Merged_Data_Cleaned)

# Predicted probabilities using ggeffects still works!
library(ggeffects)
pred_mechanic <- ggpredict(model_mechanic_firth, terms = "Political.scale")
pred_mechanic$Candidate <- "Mechanic"
sapply(Merged_Data_Cleaned[, c("Race", "sex", "Education.Level")], function(x) table(x, useNA = "ifany"))

# Load libraries
library(tidyverse)
library(ggeffects)
library(logistf)

# Recode Race into binary
Merged_Data_Cleaned$Race_Recode <- ifelse(Merged_Data_Cleaned$Race == 0, "White", "Nonwhite")
Merged_Data_Cleaned$Race_Recode <- factor(Merged_Data_Cleaned$Race_Recode)

# Collapse Education into grouped categories
Merged_Data_Cleaned$Education_Group <- case_when(
  grepl("Less than high school|High school", Merged_Data_Cleaned$Education.Level) ~ "No College",
  grepl("Some college", Merged_Data_Cleaned$Education.Level) ~ "Some College",
  grepl("Associate|Bachelor", Merged_Data_Cleaned$Education.Level) ~ "Bachelor's or Associate",
  grepl("Master|Doctoral|Professional", Merged_Data_Cleaned$Education.Level) ~ "Graduate Degree",
  TRUE ~ NA_character_
)
Merged_Data_Cleaned$Education_Group <- factor(Merged_Data_Cleaned$Education_Group)

# Run Firth logistic regression models for each candidate
model_mechanic <- logistf(Mechanic ~ Political.scale + age + income + Education_Group + Race_Recode + sex, data = Merged_Data_Cleaned)
model_teacher <- logistf(Teacher ~ Political.scale + age + income + Education_Group + Race_Recode + sex, data = Merged_Data_Cleaned)
model_veteran <- logistf(Veteran ~ Political.scale + age + income + Education_Group + Race_Recode + sex, data = Merged_Data_Cleaned)

# Predict probabilities using ggpredict (by Political Ideology only)
pred_mechanic <- ggpredict(model_mechanic, terms = "Political.scale") %>% mutate(Candidate = "Mechanic")
pred_teacher  <- ggpredict(model_teacher, terms = "Political.scale") %>% mutate(Candidate = "Teacher")
pred_veteran  <- ggpredict(model_veteran, terms = "Political.scale") %>% mutate(Candidate = "Veteran")

# Combine results
pred_combined <- bind_rows(pred_mechanic, pred_teacher, pred_veteran)

# Plot all candidates in one improved graph
ggplot(pred_combined, aes(x = x, y = predicted, color = Candidate, fill = Candidate)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(
    title = "Predicted Probability of Selecting Each Candidate by Political Ideology",
    x = "Political Ideology (0 = Liberal, 7 = Conservative)",
    y = "Predicted Probability of Candidate Selection"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    legend.position = "top"
  )
# Clean up factors before modeling
Merged_Data_Cleaned <- Merged_Data_Cleaned %>%
  mutate(
    Race_Recode = factor(ifelse(Race == 0, "White", "Nonwhite")),
    Education_Group = case_when(
      grepl("Less than high school|High school", Education.Level) ~ "No College",
      grepl("Some college", Education.Level) ~ "Some College",
      grepl("Associate|Bachelor", Education.Level) ~ "Bachelor's or Associate",
      grepl("Master|Doctoral|Professional", Education.Level) ~ "Graduate Degree",
      TRUE ~ NA_character_
    ),
    Education_Group = factor(Education_Group),
    sex = factor(sex),  # Ensure sex is a factor
    income = factor(income)  # If income is a factor, ensure it's properly treated
  ) %>%
  droplevels()  # DROP unused levels across all factor variables

summary(Merged_Data_Cleaned$sex)
summary(Merged_Data_Cleaned$Race_Recode)
summary(Merged_Data_Cleaned$Education_Group)

# Re-run model safely
library(logistf)
model_mechanic <- logistf(Mechanic ~ Political.scale + age + income + Education_Group + Race_Recode + sex, data = Merged_Data_Cleaned)

# Prediction
library(ggeffects)
pred_mechanic <- ggpredict(model_mechanic, terms = "Political.scale") %>% mutate(Candidate = "Mechanic")

summary(Merged_Data_Cleaned$sex)
summary(Merged_Data_Cleaned$Race_Recode)
summary(Merged_Data_Cleaned$Education_Group)
summary(Merged_Data_Cleaned$income)
# Step 1: Ensure proper data types
Merged_Data_Cleaned$sex <- factor(Merged_Data_Cleaned$sex)
Merged_Data_Cleaned$Race_Recode <- factor(Merged_Data_Cleaned$Race_Recode)
Merged_Data_Cleaned$Education_Group <- factor(Merged_Data_Cleaned$Education_Group)
Merged_Data_Cleaned$income <- factor(Merged_Data_Cleaned$income)

# Step 2: Fit the models
model_mechanic <- glm(Mechanic ~ Political.scale + age + income + Education_Group + Race_Recode + sex,
                      data = Merged_Data_Cleaned, family = binomial)
model_teacher <- glm(Teacher ~ Political.scale + age + income + Education_Group + Race_Recode + sex,
                     data = Merged_Data_Cleaned, family = binomial)
model_veteran <- glm(Veteran ~ Political.scale + age + income + Education_Group + Race_Recode + sex,
                     data = Merged_Data_Cleaned, family = binomial)

# Step 3: Predicted probabilities
library(ggeffects)
pred_mechanic <- ggpredict(model_mechanic, terms = "Political.scale") %>%
  mutate(Candidate = "Mechanic")
pred_teacher <- ggpredict(model_teacher, terms = "Political.scale") %>%
  mutate(Candidate = "Teacher")
pred_veteran <- ggpredict(model_veteran, terms = "Political.scale") %>%
  mutate(Candidate = "Veteran")

# Step 4: Combine & plot
library(dplyr)
library(ggplot2)

combined_preds <- bind_rows(pred_mechanic, pred_teacher, pred_veteran)

ggplot(combined_preds, aes(x = x, y = predicted, color = Candidate, fill = Candidate)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(
    title = "Predicted Probability of Selecting Each Candidate by Political Ideology",
    x = "Political Ideology (0 = Liberal, 7 = Conservative)",
    y = "Predicted Probability of Candidate Selection"
  ) +
  scale_color_manual(values = c("Mechanic" = "red", "Teacher" = "green", "Veteran" = "blue")) +
  scale_fill_manual(values = c("Mechanic" = "red", "Teacher" = "green", "Veteran" = "blue")) +
  theme_minimal(base_size = 14)
library(logistf)
model_veteran_firth <- logistf(Veteran ~ Political.scale + age + income + Education_Group + Race_Recode + sex,
                               data = Merged_Data_Cleaned)
pred_veteran <- ggpredict(model_veteran_firth, terms = "Political.scale") %>%
  mutate(Candidate = "Veteran")

Merged_Data_Cleaned %>%
  group_by(Political.scale) %>%
  summarise(
    total = n(),
    chose_veteran = sum(Veteran == 1, na.rm = TRUE),
    prop_veteran = chose_veteran / total
  )
sapply(Merged_Data_Cleaned[, c("sex", "Race_Recode", "Education_Group", "income")], function(x) length(unique(na.omit(x))))
library(logistf)

model_mechanic_firth <- logistf(Mechanic ~ Political.scale + age + sex + Race_Recode + Education_Group + income, 
                                data = Merged_Data_Cleaned)

model_teacher_firth <- logistf(Teacher ~ Political.scale + age + sex + Race_Recode + Education_Group + income, 
                               data = Merged_Data_Cleaned)

model_veteran_firth <- logistf(Veteran ~ Political.scale + age + sex + Race_Recode + Education_Group + income, 
                               data = Merged_Data_Cleaned)

library(ggeffects)

pred_mechanic <- ggpredict(model_mechanic_firth, terms = "Political.scale") %>%
  mutate(Candidate = "Mechanic")

pred_teacher <- ggpredict(model_teacher_firth, terms = "Political.scale") %>%
  mutate(Candidate = "Teacher")

pred_veteran <- ggpredict(model_veteran_firth, terms = "Political.scale") %>%
  mutate(Candidate = "Veteran")

summary(Merged_Data_Cleaned$sex)
summary(Merged_Data_Cleaned$Race_Recode)
summary(Merged_Data_Cleaned$Education_Group)
summary(Merged_Data_Cleaned$income)
# Drop unused factor levels from the dataset
Merged_Data_Cleaned <- droplevels(Merged_Data_Cleaned)

# Rebuild models after cleaning levels
model_mechanic_firth <- logistf(Mechanic ~ Political.scale + age + income + Education_Group + Race_Recode + sex, data = Merged_Data_Cleaned)
model_teacher_firth <- logistf(Teacher ~ Political.scale + age + income + Education_Group + Race_Recode + sex, data = Merged_Data_Cleaned)
model_veteran_firth  <- logistf(Veteran  ~ Political.scale + age + income + Education_Group + Race_Recode + sex, data = Merged_Data_Cleaned)

# Run predictions
pred_mechanic <- ggpredict(model_mechanic_firth, terms = "Political.scale") %>%
  mutate(Candidate = "Mechanic")

pred_teacher <- ggpredict(model_teacher_firth, terms = "Political.scale") %>%
  mutate(Candidate = "Teacher")

pred_veteran <- ggpredict(model_veteran_firth, terms = "Political.scale") %>%
  mutate(Candidate = "Veteran")

# Combine and plot
combined_preds <- bind_rows(pred_mechanic, pred_teacher, pred_veteran)

ggplot(combined_preds, aes(x = x, y = predicted, color = Candidate, fill = Candidate)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(
    title = "Predicted Probability of Selecting Each Candidate by Political Ideology",
    x = "Political Ideology (0 = Liberal, 7 = Conservative)",
    y = "Predicted Probability of Candidate Selection"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Mechanic" = "red", "Teacher" = "green", "Veteran" = "blue")) +
  scale_fill_manual(values = c("Mechanic" = "red", "Teacher" = "green", "Veteran" = "blue"))

Merged_Data_Cleaned <- droplevels(Merged_Data_Cleaned)
library(logistf)

model_mechanic_firth <- logistf(Mechanic ~ Political.scale + age + income + Education_Group + Race_Recode + sex, data = Merged_Data_Cleaned)
model_teacher_firth <- logistf(Teacher ~ Political.scale + age + income + Education_Group + Race_Recode + sex, data = Merged_Data_Cleaned)
model_veteran_firth <- logistf(Veteran ~ Political.scale + age + income + Education_Group + Race_Recode + sex, data = Merged_Data_Cleaned)

library(ggeffects)
library(dplyr)

pred_mechanic <- ggpredict(model_mechanic_firth, terms = "Political.scale") %>% mutate(Candidate = "Mechanic")
pred_teacher <- ggpredict(model_teacher_firth, terms = "Political.scale") %>% mutate(Candidate = "Teacher")
pred_veteran <- ggpredict(model_veteran_firth, terms = "Political.scale") %>% mutate(Candidate = "Veteran")
sapply(Merged_Data_Cleaned[, c("sex", "Race_Recode", "Education_Group", "income")], function(x) length(unique(na.omit(x))))
# Re-factor key variables
Merged_Data_Cleaned$sex <- factor(Merged_Data_Cleaned$sex)
Merged_Data_Cleaned$Race_Recode <- factor(Merged_Data_Cleaned$Race_Recode)
Merged_Data_Cleaned$Education_Group <- factor(Merged_Data_Cleaned$Education_Group)
Merged_Data_Cleaned$income <- factor(Merged_Data_Cleaned$income)

library(logistf)

model_mechanic_firth <- logistf(Mechanic ~ Political.scale + age + income + Education_Group + sex, data = Merged_Data_Cleaned)
model_teacher_firth  <- logistf(Teacher  ~ Political.scale + age + income + Education_Group + sex, data = Merged_Data_Cleaned)
model_veteran_firth  <- logistf(Veteran  ~ Political.scale + age + income + Education_Group + sex, data = Merged_Data_Cleaned)

library(ggeffects)

pred_mechanic <- ggpredict(model_mechanic_firth, terms = "Political.scale") %>% mutate(Candidate = "Mechanic")
pred_teacher  <- ggpredict(model_teacher_firth,  terms = "Political.scale") %>% mutate(Candidate = "Teacher")
pred_veteran  <- ggpredict(model_veteran_firth,  terms = "Political.scale") %>% mutate(Candidate = "Veteran")

summary(Merged_Data_Cleaned$sex)
summary(Merged_Data_Cleaned$Race_Recode)
summary(Merged_Data_Cleaned$Education_Group)
summary(Merged_Data_Cleaned$income)
nlevels(factor(Merged_Data_Cleaned$sex))
nlevels(factor(Merged_Data_Cleaned$Race_Recode))
nlevels(factor(Merged_Data_Cleaned$Education_Group))
nlevels(factor(Merged_Data_Cleaned$income))

# Re-convert key variables as factors to be 100% safe
Merged_Data_Cleaned$sex <- as.factor(Merged_Data_Cleaned$sex)
Merged_Data_Cleaned$Race_Recode <- as.factor(Merged_Data_Cleaned$Race_Recode)
Merged_Data_Cleaned$Education_Group <- as.factor(Merged_Data_Cleaned$Education_Group)
Merged_Data_Cleaned$income <- as.factor(Merged_Data_Cleaned$income)

# Double-check Political.scale is numeric
Merged_Data_Cleaned$Political.scale <- as.numeric(Merged_Data_Cleaned$Political.scale)

# Remove NAs from model-relevant variables
mechanic_data <- Merged_Data_Cleaned %>%
  select(Mechanic, Political.scale, age, income, Education_Group, Race_Recode, sex) %>%
  na.omit()

# Run Firth logistic regression
library(logistf)
model_mechanic_firth <- logistf(Mechanic ~ Political.scale + age + income + Education_Group + Race_Recode + sex,
                                data = mechanic_data)

# Predictions
library(ggeffects)
library(dplyr)

pred_mechanic <- ggpredict(model_mechanic_firth, terms = "Political.scale") %>%
  mutate(Candidate = "Mechanic")
# Subset and drop unused factor levels
mechanic_data <- Merged_Data_Cleaned %>%
  select(Mechanic, Political.scale, age, income, Education_Group, Race_Recode, sex) %>%
  na.omit() %>%
  mutate(
    income = droplevels(factor(income)),
    Education_Group = droplevels(factor(Education_Group)),
    Race_Recode = droplevels(factor(Race_Recode)),
    sex = droplevels(factor(sex))
  )
library(logistf)

model_mechanic_firth <- logistf(
  Mechanic ~ Political.scale + age + income + Education_Group + Race_Recode + sex,
  data = mechanic_data
)
library(ggeffects)
library(dplyr)

pred_mechanic <- ggpredict(model_mechanic_firth, terms = "Political.scale") %>%
  mutate(Candidate = "Mechanic")

mechanic_data <- Merged_Data_Cleaned %>%
  select(Mechanic, Political.scale, age, income, Education_Group, Race_Recode, sex) %>%
  na.omit() %>%
  mutate(
    sex = droplevels(factor(sex)),
    Race_Recode = droplevels(factor(Race_Recode)),
    Education_Group = droplevels(factor(Education_Group)),
    income = droplevels(factor(income))
  )

# Show levels for sanity check
summary(mechanic_data$sex)
summary(mechanic_data$Race_Recode)
summary(mechanic_data$Education_Group)
summary(mechanic_data$income)

library(logistf)

model_mechanic_firth <- logistf(
  Mechanic ~ Political.scale + age + income + Education_Group + Race_Recode + sex,
  data = mechanic_data
)
library(ggeffects)

pred_mechanic <- ggpredict(model_mechanic_firth, terms = "Political.scale") %>%
  mutate(Candidate = "Mechanic")

mechanic_data$sex <- factor(mechanic_data$sex, levels = c(0, 1))
mechanic_data$Race_Recode <- factor(mechanic_data$Race_Recode, levels = c("White", "Nonwhite"))
mechanic_data$Education_Group <- factor(mechanic_data$Education_Group, levels = c(
  "No College", "Some College", "Bachelor's or Associate", "Graduate Degree"
))

# Use ggpredict on a single focal term and manually specify the others
pred_mechanic <- ggpredict(
  model_mechanic_firth,
  terms = c("Political.scale [0:7]",  # focal term with range
            "sex [0,1]",              # ensure both sexes are included
            "Race_Recode [White,Nonwhite]",
            "Education_Group [Bachelor's or Associate, Graduate Degree, No College, Some College]",
            "income [$30,000 to $39,999]")  # pick one mid-range income for stability
) %>% 
  mutate(Candidate = "Mechanic")

# Load necessary libraries
library(ggplot2)
library(ggeffects)
library(dplyr)

# Generate predictions for each candidate with fixed values for categorical covariates
pred_mechanic <- ggpredict(model_mechanic_firth, terms = c("Political.scale",
                                                           "sex [1]",  # Female
                                                           "Race_Recode [White]",
                                                           "Education_Group [Bachelor's or Associate]",
                                                           "income [$50,000 to $59,999]")) %>%
  mutate(Candidate = "Mechanic")

pred_teacher <- ggpredict(model_teacher_firth, terms = c("Political.scale",
                                                         "sex [1]",
                                                         "Race_Recode [White]",
                                                         "Education_Group [Bachelor's or Associate]",
                                                         "income [$50,000 to $59,999]")) %>%
  mutate(Candidate = "Teacher")

pred_veteran <- ggpredict(model_veteran_firth, terms = c("Political.scale",
                                                         "sex [1]",
                                                         "Race_Recode [White]",
                                                         "Education_Group [Bachelor's or Associate]",
                                                         "income [$50,000 to $59,999]")) %>%
  mutate(Candidate = "Veteran")

# Combine all predictions
combined_preds <- bind_rows(pred_mechanic, pred_teacher, pred_veteran)

# Plot
ggplot(combined_preds, aes(x = x, y = predicted, color = Candidate, fill = Candidate)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(
    title = "Predicted Probability of Selecting Each Candidate by Political Ideology",
    x = "Political Ideology (0 = Liberal, 7 = Conservative)",
    y = "Predicted Probability"
  ) +
  scale_color_manual(values = c("Mechanic" = "#e41a1c", "Teacher" = "#4daf4a", "Veteran" = "#377eb8")) +
  scale_fill_manual(values = c("Mechanic" = "#e41a1c", "Teacher" = "#4daf4a", "Veteran" = "#377eb8")) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# Ensure all key categorical variables are factors with proper levels
Merged_Data_Cleaned <- Merged_Data_Cleaned %>%
  filter(!is.na(Political.scale), !is.na(Mechanic), !is.na(Teacher), !is.na(Veteran)) %>%
  mutate(
    sex = factor(sex, levels = c(0, 1), labels = c("Male", "Female")),
    Race_Recode = factor(Race_Recode, levels = c("White", "Nonwhite")),
    Education_Group = factor(Education_Group, levels = c(
      "No College", "Some College", "Bachelor's or Associate", "Graduate Degree"
    )),
    income = factor(income)
  )

library(logistf)

model_mechanic_firth <- logistf(Mechanic ~ Political.scale + age + income + Education_Group + Race_Recode + sex, data = Merged_Data_Cleaned)
model_teacher_firth  <- logistf(Teacher ~ Political.scale + age + income + Education_Group + Race_Recode + sex, data = Merged_Data_Cleaned)
model_veteran_firth  <- logistf(Veteran ~ Political.scale + age + income + Education_Group + Race_Recode + sex, data = Merged_Data_Cleaned)

library(ggeffects)

pred_mechanic <- ggpredict(model_mechanic_firth, terms = "Political.scale") %>%
  mutate(Candidate = "Mechanic")

pred_teacher <- ggpredict(model_teacher_firth, terms = "Political.scale") %>%
  mutate(Candidate = "Teacher")

pred_veteran <- ggpredict(model_veteran_firth, terms = "Political.scale") %>%
  mutate(Candidate = "Veteran")

# Step 1: Create a stripped-down dataset that only keeps complete rows
model_data <- Merged_Data_Cleaned %>%
  filter(
    !is.na(Political.scale),
    !is.na(sex),
    !is.na(Race_Recode),
    !is.na(Education_Group),
    !is.na(income),
    !is.na(age)
  ) %>%
  mutate(
    sex = factor(sex, levels = c(0, 1), labels = c("Male", "Female")),
    Race_Recode = factor(Race_Recode, levels = c("White", "Nonwhite")),
    Education_Group = factor(Education_Group, levels = c(
      "No College", "Some College", "Bachelor's or Associate", "Graduate Degree"
    )),
    income = factor(income)
  )
summary(model_data$sex)
summary(model_data$Race_Recode)
summary(model_data$Education_Group)
summary(model_data$income)

Merged_Data_Cleaned$sex <- as.numeric(Merged_Data_Cleaned$sex)

summary(Merged_Data_Cleaned$sex)
table(Merged_Data_Cleaned$sex, useNA = "always")
# Fix sex variable back to 0 (Male) and 1 (Female)
Merged_Data_Cleaned$sex <- ifelse(Merged_Data_Cleaned$sex == 2, 1,
                                  ifelse(Merged_Data_Cleaned$sex == 1, 0, NA))

summary(Merged_Data_Cleaned$sex)
table(Merged_Data_Cleaned$sex, useNA = "always")

# Load required libraries
library(tidyverse)

# Step 1: Fit logistic models for each candidate
model_mechanic <- glm(Mechanic ~ Political.scale, data = Merged_Data_Cleaned, family = binomial)
model_teacher  <- glm(Teacher ~ Political.scale, data = Merged_Data_Cleaned, family = binomial)
model_veteran  <- glm(Veteran ~ Political.scale, data = Merged_Data_Cleaned, family = binomial)

# Step 2: Create prediction grid
ideology_vals <- data.frame(Political.scale = seq(0, 7, by = 0.1))

# Step 3: Get predicted probabilities
pred_mechanic <- predict(model_mechanic, newdata = ideology_vals, type = "response")
pred_teacher  <- predict(model_teacher, newdata = ideology_vals, type = "response")
pred_veteran  <- predict(model_veteran, newdata = ideology_vals, type = "response")

# Step 4: Combine predictions into one dataframe
prediction_df <- bind_rows(
  tibble(Political.scale = ideology_vals$Political.scale, 
         Probability = pred_mechanic, 
         Candidate = "Mechanic"),
  tibble(Political.scale = ideology_vals$Political.scale, 
         Probability = pred_teacher, 
         Candidate = "Teacher"),
  tibble(Political.scale = ideology_vals$Political.scale, 
         Probability = pred_veteran, 
         Candidate = "Veteran")
)

# Step 5: Plot
ggplot(prediction_df, aes(x = Political.scale, y = Probability, color = Candidate)) +
  geom_line(size = 1.2) +
  labs(
    title = "Predicted Probability of Selecting Each Candidate by Political Ideology",
    x = "Political Ideology (0 = Liberal, 7 = Conservative)",
    y = "Predicted Probability",
    color = "Candidate"
  ) +
  theme_minimal(base_size = 14)
summary(model_mechanic)
summary(model_teacher)
summary(model_veteran)

library(ggeffects)
library(ggplot2)
library(dplyr)

# Generate predicted probabilities with CIs
pred_mechanic <- ggpredict(model_mechanic, terms = "Political.scale") %>%
  mutate(Candidate = "Mechanic")
pred_teacher <- ggpredict(model_teacher, terms = "Political.scale") %>%
  mutate(Candidate = "Teacher")
pred_veteran <- ggpredict(model_veteran, terms = "Political.scale") %>%
  mutate(Candidate = "Veteran")

# Combine all predictions
prediction_df <- bind_rows(pred_mechanic, pred_teacher, pred_veteran)

# Plot with confidence intervals
ggplot(prediction_df, aes(x = x, y = predicted, color = Candidate, fill = Candidate)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  scale_color_manual(values = c("Mechanic" = "firebrick", 
                                "Teacher" = "forestgreen", 
                                "Veteran" = "steelblue")) +
  scale_fill_manual(values = c("Mechanic" = "firebrick", 
                               "Teacher" = "forestgreen", 
                               "Veteran" = "steelblue")) +
  labs(
    title = "Predicted Probability of Selecting Each Candidate by Political Ideology",
    subtitle = "With 95% Confidence Intervals",
    x = "Political Ideology (0 = Liberal, 7 = Conservative)",
    y = "Predicted Probability",
    color = "Candidate",
    fill = "Candidate"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "right",
    axis.title = element_text(face = "bold")
  )
model_mechanic <- glm(Mechanic ~ Political.scale, family = binomial, data = Merged_Data_Cleaned)
model_teacher <- glm(Teacher ~ Political.scale, family = binomial, data = Merged_Data_Cleaned)
model_veteran <- glm(Veteran ~ Political.scale, family = binomial, data = Merged_Data_Cleaned)

library(stargazer)

stargazer(model_mechanic, model_teacher, model_veteran,
          type = "text", # Use "latex" if you're inserting into LaTeX
          title = "Logistic Regression Models: Predicting Candidate Selection by Political Ideology",
          column.labels = c("Mechanic", "Teacher", "Veteran"),
          dep.var.labels = "Candidate Chosen (1 = Yes)",
          covariate.labels = c("Intercept", "Political Ideology"),
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          align = TRUE,
          no.space = TRUE)

# Load required libraries
library(ggeffects)
library(ggplot2)
library(dplyr)

# Run models by candidate (interaction between ideology and education)
model_mechanic_edu <- glm(Mechanic ~ Political.scale * Education_Group, family = binomial, data = Merged_Data_Cleaned)
model_teacher_edu  <- glm(Teacher ~ Political.scale * Education_Group, family = binomial, data = Merged_Data_Cleaned)
model_veteran_edu  <- glm(Veteran ~ Political.scale * Education_Group, family = binomial, data = Merged_Data_Cleaned)

# Create predictions by education level (set continuous ideology, group education)
pred_mechanic <- ggpredict(model_mechanic_edu, terms = c("Political.scale", "Education_Group")) %>%
  mutate(Candidate = "Mechanic")
pred_teacher <- ggpredict(model_teacher_edu, terms = c("Political.scale", "Education_Group")) %>%
  mutate(Candidate = "Teacher")
pred_veteran <- ggpredict(model_veteran_edu, terms = c("Political.scale", "Education_Group")) %>%
  mutate(Candidate = "Veteran")

# Combine all predictions
pred_all <- bind_rows(pred_mechanic, pred_teacher, pred_veteran)

# Plot with facets by candidate
ggplot(pred_all, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  facet_wrap(~ Candidate) +
  scale_color_manual(
    name = "Education Level",
    values = c(
      "No College" = "tomato",
      "Some College" = "seagreen",
      "Bachelor's" = "dodgerblue",
      "Graduate Degree" = "purple"
    )
  ) +
  scale_fill_manual(
    name = "Education Level",
    values = c(
      "No College" = "tomato",
      "Some College" = "seagreen",
      "Bachelor's" = "dodgerblue",
      "Graduate Degree" = "purple"
    )
  ) +
  labs(
    title = "Predicted Probability of Selecting Candidate",
    subtitle = "By Political Ideology and Education Level",
    x = "Political Ideology (0 = Liberal, 7 = Conservative)",
    y = "Predicted Probability"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")

# Load libraries
library(ggeffects)
library(ggplot2)
library(dplyr)

# Make sure Political.scale is numeric
Merged_Data_Cleaned$Political.scale <- as.numeric(Merged_Data_Cleaned$Political.scale)

# Make sure Education_Group is a factor with clear levels
Merged_Data_Cleaned$Education_Group <- factor(
  Merged_Data_Cleaned$Education_Group,
  levels = c("No College", "Some College", "Bachelor's", "Graduate Degree")
)

# Fit the models with interaction

# Generate predictions
pred_mech <- ggpredict(model_mechanic_edu, terms = c("Political.scale", "Education_Group")) %>%
  mutate(Candidate = "Mechanic")
pred_teach <- ggpredict(model_teacher_edu, terms = c("Political.scale", "Education_Group")) %>%
  mutate(Candidate = "Teacher")
pred_vet <- ggpredict(model_veteran_edu, terms = c("Political.scale", "Education_Group")) %>%
  mutate(Candidate = "Veteran")

# Combine all
all_preds <- bind_rows(pred_mech, pred_teach, pred_vet)

# Plot
ggplot(all_preds, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  facet_wrap(~ Candidate) +
  scale_color_manual(
    name = "Education Level",
    values = c(
      "No College" = "firebrick",
      "Some College" = "seagreen3",
      "Bachelor's" = "dodgerblue",
      "Graduate Degree" = "orchid"
    )
  ) +
  scale_fill_manual(
    name = "Education Level",
    values = c(
      "No College" = "firebrick",
      "Some College" = "seagreen3",
      "Bachelor's" = "dodgerblue",
      "Graduate Degree" = "orchid"
    )
  ) +
  labs(
    title = "Predicted Probability of Selecting Candidate",
    subtitle = "By Political Ideology and Education Level",
    x = "Political Ideology (0 = Liberal, 7 = Conservative)",
    y = "Predicted Probability"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")
# Use the correct levels from your data
edu_levels <- c("No College", "Some College", "Bachelor's", "Graduate Degree")

# Prediction grid across ideology and education
prediction_grid <- expand.grid(
  Political.scale = seq(0, 7, by = 1),
  Education_Group = edu_levels
)

# Ensure factor levels match
prediction_grid$Education_Group <- factor(prediction_grid$Education_Group, levels = edu_levels)

prediction_mech <- prediction_grid
prediction_mech$predicted <- predict(model_mech, newdata = prediction_mech, type = "response")
prediction_mech$Candidate <- "Mechanic"

prediction_teach <- prediction_grid
prediction_teach$predicted <- predict(model_teach, newdata = prediction_teach, type = "response")
prediction_teach$Candidate <- "Teacher"

prediction_vet <- prediction_grid
prediction_vet$predicted <- predict(model_vet, newdata = prediction_vet, type = "response")
prediction_vet$Candidate <- "Veteran"
# Re-level the factor in your data before modeling
Merged_Data_Cleaned$Education_Group <- factor(
  Merged_Data_Cleaned$Education_Group,
  levels = c("No College", "Some College", "Bachelor's", "Graduate Degree")
)
model_mech <- glm(Mechanic ~ Political.scale * Education_Group, family = binomial, data = Merged_Data_Cleaned)
model_teach <- glm(Teacher ~ Political.scale * Education_Group, family = binomial, data = Merged_Data_Cleaned)
model_vet <- glm(Veteran ~ Political.scale * Education_Group, family = binomial, data = Merged_Data_Cleaned)

edu_levels <- levels(Merged_Data_Cleaned$Education_Group)

prediction_grid <- expand.grid(
  Political.scale = seq(0, 7, by = 1),
  Education_Group = edu_levels
)

# Match factor levels
prediction_grid$Education_Group <- factor(prediction_grid$Education_Group, levels = edu_levels)

prediction_mech <- prediction_grid
prediction_mech$predicted <- predict(model_mech, newdata = prediction_mech, type = "response")
prediction_mech$Candidate <- "Mechanic"

prediction_teach <- prediction_grid
prediction_teach$predicted <- predict(model_teach, newdata = prediction_teach, type = "response")
prediction_teach$Candidate <- "Teacher"

prediction_vet <- prediction_grid
prediction_vet$predicted <- predict(model_vet, newdata = prediction_vet, type = "response")
prediction_vet$Candidate <- "Veteran"

# Step 1: Drop all unused factor levels in your training data
Merged_Data_Cleaned$Education_Group <- droplevels(factor(
  Merged_Data_Cleaned$Education_Group,
  levels = c("No College", "Some College", "Bachelor's", "Graduate Degree")
))

# Step 2: Fit the model *AFTER* resetting levels
levels(Merged_Data_Cleaned$Education_Group)

# Re-level to make sure Education_Group is properly set up
Merged_Data_Cleaned$Education_Group <- factor(
  Merged_Data_Cleaned$Education_Group,
  levels = c("No College", "Some College", "Graduate Degree")
)

# Fit model with interaction term
model_mech <- glm(Mechanic ~ Political.scale * Education_Group,
                  data = Merged_Data_Cleaned,
                  family = binomial)

# Create prediction grid using model-compatible levels
prediction_grid <- expand.grid(
  Political.scale = 0:7,
  Education_Group = levels(Merged_Data_Cleaned$Education_Group)
)

# Match factor levels explicitly
prediction_grid$Education_Group <- factor(
  prediction_grid$Education_Group,
  levels = levels(Merged_Data_Cleaned$Education_Group)
)

# Predict with confidence intervals
prediction_grid$fit <- predict(model_mech, newdata = prediction_grid, type = "response", se.fit = FALSE)

# Plot with updated aesthetics
ggplot(prediction_grid, aes(x = Political.scale, y = fit, color = Education_Group)) +
  geom_line(size = 1.4) +
  labs(
    title = "Predicted Probability of Selecting Mechanic",
    subtitle = "By Political Ideology and Education Level",
    x = "Political Ideology (0 = Liberal, 7 = Conservative)",
    y = "Predicted Probability"
  ) +
  theme_minimal(base_size = 14)
# Create summary table of raw probabilities
library(dplyr)
raw_probs <- Merged_Data_Cleaned %>%
  group_by(Political.scale, Education_Group) %>%
  summarise(prop_mech = mean(Mechanic, na.rm = TRUE), n = n()) %>%
  ungroup()

# Plot
ggplot(raw_probs, aes(x = Political.scale, y = prop_mech, color = Education_Group)) +
  geom_line(size = 1.4) +
  labs(
    title = "Raw Probability of Selecting Mechanic",
    x = "Political Ideology (0 = Liberal, 7 = Conservative)",
    y = "Proportion Selecting Mechanic"
  ) +
  theme_minimal(base_size = 14)

# Relevel to make "Graduate Degree" the reference
Merged_Data_Cleaned$Education_Group <- relevel(Merged_Data_Cleaned$Education_Group, ref = "Graduate Degree")

# Refit the model for the Mechanic candidate
                  data = Merged_Data_Cleaned, family = binomial)

# Predicted probabilities
library(ggeffects)
pred_mech <- ggpredict(model_mech, terms = c("Political.scale", "Education_Group"))
pred_mech$Candidate <- "Mechanic"

# Plot
library(ggplot2)
ggplot(pred_mech, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1.4) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  scale_color_manual(values = c("No College" = "red", "Some College" = "green", "Graduate Degree" = "blue")) +
  scale_fill_manual(values = c("No College" = "red", "Some College" = "green", "Graduate Degree" = "blue")) +
  labs(
    title = "Predicted Probability of Selecting Mechanic",
    subtitle = "By Political Ideology and Education Level",
    x = "Political Ideology (0 = Liberal, 7 = Conservative)",
    y = "Predicted Probability",
    color = "Education Group",
    fill = "Education Group"
  ) +
  theme_minimal(base_size = 14)

library(dplyr)
library(ggplot2)

# Make sure Education_Group is a factor in the model with the correct levels
Merged_Data_Cleaned$Education_Group <- factor(Merged_Data_Cleaned$Education_Group,
                                              levels = c("No College", "Some College", "Graduate Degree"))

# Fit model with interaction
                  data = Merged_Data_Cleaned, family = binomial)

# Create prediction grid
prediction_grid <- expand.grid(
  Political.scale = 0:7,
  Education_Group = levels(Merged_Data_Cleaned$Education_Group)
)

# Predict manually
prediction_grid$predicted <- predict(model_mech, newdata = prediction_grid, type = "response")

# Plot it manually
ggplot(prediction_grid, aes(x = Political.scale, y = predicted, color = Education_Group)) +
  geom_line(size = 1.5) +
  labs(
    title = "Predicted Probability of Selecting Mechanic",
    subtitle = "By Political Ideology and Education Level",
    x = "Political Ideology (0 = Liberal, 7 = Conservative)",
    y = "Predicted Probability"
  ) +
  scale_color_manual(values = c("No College" = "red", "Some College" = "green", "Graduate Degree" = "blue")) +
  theme_minimal(base_size = 14)
Merged_Data_Cleaned %>%
  group_by(Education_Group, Political.scale) %>%
  summarise(n = n()) %>%
  tidyr::spread(key = Political.scale, value = n, fill = 0)
library(ggplot2)
library(dplyr)

# Plot raw proportions with smoothing
Merged_Data_Cleaned %>%
  filter(!is.na(Education_Group)) %>%
  ggplot(aes(x = Political.scale, y = Mechanic, color = Education_Group)) +
  geom_smooth(method = "loess", se = TRUE, span = 1.2) +
  labs(
    title = "Smoothed Raw Probability of Selecting Mechanic",
    subtitle = "By Political Ideology and Education Group",
    x = "Political Ideology (0 = Liberal, 7 = Conservative)",
    y = "Proportion Selecting Mechanic"
  ) +
  scale_color_manual(values = c("No College" = "red", 
                                "Some College" = "green", 
                                "Graduate Degree" = "blue")) +
  theme_minimal(base_size = 14)

ggplot(Merged_Data_Cleaned %>% filter(!is.na(Education_Group)), 
       aes(x = Political.scale, y = Mechanic, color = Education_Group)) +
  geom_smooth(method = "loess", se = TRUE, span = 1.2) +
  coord_cartesian(ylim = c(0, 1)) +  # <-- constrain to 0-1
  scale_color_manual(values = c("No College" = "red",
                                "Some College" = "green",
                                "Graduate Degree" = "blue")) +
  labs(
    title = "Smoothed Raw Probability of Selecting Mechanic",
    subtitle = "By Political Ideology and Education Group",
    x = "Political Ideology (0 = Liberal, 7 = Conservative)",
    y = "Proportion Selecting Mechanic"
  ) +
  theme_minimal(base_size = 14)
coord_cartesian(ylim = c(0, 1))

labs(
  title = "Smoothed Probability of Selecting Mechanic Candidate",
  subtitle = "By Political Ideology and Education Group",
  y = "Proportion Selecting Mechanic",
  x = "Political Ideology (0 = Liberal, 7 = Conservative)"
)

# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Step 1: Convert data to long format for plotting
long_df <- Merged_Data_Cleaned %>%
  select(Political.scale, Education_Group, Mechanic, Teacher, Veteran) %>%
  pivot_longer(cols = c(Mechanic, Teacher, Veteran),
               names_to = "Candidate",
               values_to = "Selected")

# Step 2: Aggregate to calculate proportions
agg_df <- long_df %>%
  group_by(Candidate, Political.scale, Education_Group) %>%
  summarise(
    Proportion = mean(Selected, na.rm = TRUE),
    Count = n()
  ) %>%
  filter(Count >= 3)  # Optional: removes noisy cells with too few obs

# Step 3: Plot smoothed raw proportions with loess and facets
ggplot(agg_df, aes(x = Political.scale, y = Proportion, color = Education_Group)) +
  geom_smooth(method = "loess", se = TRUE, span = 1) +
  facet_wrap(~Candidate) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Smoothed Raw Probability of Selecting Candidate",
    subtitle = "By Political Ideology and Education Group",
    x = "Political Ideology (0 = Liberal, 7 = Conservative)",
    y = "Proportion Selecting Candidate",
    color = "Education Group"
  )
# Filter out NA Education_Group values
filtered_df <- raw_df %>% filter(!is.na(Education_Group))

# Plot without NA group
ggplot(filtered_df, aes(x = Political.scale, y = Proportion, color = Education_Group)) +
  geom_smooth(method = "loess", se = TRUE, aes(group = Education_Group), size = 1.2) +
  facet_wrap(~ Candidate) +
  labs(
    title = "Smoothed Raw Probability of Selecting Candidate",
    subtitle = "By Political Ideology and Education Group",
    x = "Political Ideology (0 = Liberal, 7 = Conservative)",
    y = "Proportion Selecting Candidate",
    color = "Education Group"
  ) +
  scale_color_manual(values = c(
    "No College" = "red",
    "Some College" = "green",
    "Graduate Degree" = "blue"
  )) +
  theme_minimal(base_size = 14)

library(dplyr)
library(ggplot2)
library(tidyr)

# Clean + reshape
raw_long <- Merged_Data_Cleaned %>%
  filter(!is.na(Education_Group), !is.na(Political.scale)) %>%
  select(Political.scale, Education_Group, Mechanic, Teacher, Veteran) %>%
  pivot_longer(cols = c(Mechanic, Teacher, Veteran),
               names_to = "Candidate", values_to = "Selected") %>%
  group_by(Candidate, Education_Group, Political.scale) %>%
  summarise(Proportion = mean(Selected, na.rm = TRUE),
            .groups = "drop")

# Plot
ggplot(raw_long, aes(x = Political.scale, y = Proportion, color = Education_Group)) +
  geom_smooth(method = "loess", se = TRUE, formula = y ~ x, span = 1.2) +
  facet_wrap(~ Candidate) +
  labs(
    title = "Smoothed Raw Probability of Selecting Candidate",
    subtitle = "By Political Ideology and Education Group",
    x = "Political Ideology (0 = Liberal, 7 = Conservative)",
    y = "Proportion Selecting Candidate",
    color = "Education Group"
  ) +
  scale_color_manual(values = c(
    "No College" = "red",
    "Some College" = "green",
    "Graduate Degree" = "blue"
  )) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.01, 0)) +
  theme_minimal(base_size = 14)

library(dplyr)
library(tidyr)
library(ggplot2)

# Create long-form data for smoothing
long_df <- Merged_Data_Cleaned %>%
  select(Political.scale, Education_Group, Mechanic, Teacher, Veteran) %>%
  pivot_longer(cols = c(Mechanic, Teacher, Veteran),
               names_to = "Candidate",
               values_to = "Selected") %>%
  filter(!is.na(Education_Group), !is.na(Political.scale), !is.na(Selected))

# Plot smoothed proportions (no shaded region)
ggplot(long_df, aes(x = Political.scale, y = Selected, color = Education_Group)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.75, formula = y ~ x, linewidth = 1) +
  facet_wrap(~Candidate) +
  labs(
    title = "Smoothed Raw Probability of Selecting Candidate",
    subtitle = "By Political Ideology and Education Group",
    x = "Political Ideology (0 = Liberal, 7 = Conservative)",
    y = "Proportion Selecting Candidate",
    color = "Education Group"
  ) +
  scale_color_manual(values = c("No College" = "red", "Some College" = "limegreen", "Graduate Degree" = "blue")) +
  theme_minimal(base_size = 14)
geom_smooth(se = FALSE, method = "loess", span = 1, formula = y ~ x, linewidth = 1)

geom_smooth(
  se = FALSE,
  method = "loess",
  span = 1,
  formula = y ~ x,
  linewidth = 1
)
ggplot(plot_data, aes(x = Political.scale, y = Proportion, color = Education_Group)) +
  geom_smooth(method = "loess", formula = y ~ x, span = 1, se = FALSE, linewidth = 1) +
  facet_wrap(~Candidate) +
  labs(
    title = "Smoothed Raw Probability of Selecting Candidate",
    subtitle = "By Political Ideology and Education Group",
    x = "Political Ideology (0 = Liberal, 7 = Conservative)",
    y = "Proportion Selecting Candidate",
    color = "Education Group"
  ) +
  theme_minimal(base_size = 14)
library(dplyr)
library(ggplot2)
library(tidyr)

# Replace with your actual dataset name
data <- Merged_Data_Cleaned

# Make sure Education_Group is a factor and remove NAs
data <- data %>%
  filter(!is.na(Education_Group), !is.na(Political.scale)) %>%
  mutate(
    Education_Group = factor(Education_Group, 
                             levels = c("No College", "Some College", "Graduate Degree"))
  )

# Convert data to long format for candidates
long_data <- data %>%
  pivot_longer(cols = c(Mechanic, Teacher, Veteran),
               names_to = "Candidate", 
               values_to = "Selected")

# Group by ideology, education, and candidate to calculate raw probabilities
probs <- long_data %>%
  group_by(Political.scale, Education_Group, Candidate) %>%
  summarise(Proportion_Selected = mean(Selected, na.rm = TRUE), .groups = "drop")

# Plot with smoothing and NO confidence ribbon
ggplot(probs, aes(x = Political.scale, y = Proportion_Selected, color = Education_Group)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.75, linewidth = 1.2) +
  facet_wrap(~ Candidate) +
  scale_color_manual(values = c("No College" = "red", "Some College" = "green", "Graduate Degree" = "blue")) +
  labs(
    title = "Smoothed Raw Probability of Selecting Candidate",
    subtitle = "By Political Ideology and Education Group",
    x = "Political Ideology (0 = Liberal, 7 = Conservative)",
    y = "Proportion Selecting Candidate",
    color = "Education Group"
  ) +
  theme_minimal(base_size = 14)

library(dplyr)
library(ggplot2)
library(tidyr)

# Prepare and pivot data
long_df <- Merged_Data_Cleaned %>%
  filter(!is.na(Education_Group), !is.na(Political.scale)) %>%
  pivot_longer(cols = c(Mechanic, Teacher, Veteran),
               names_to = "Candidate", 
               values_to = "Selected") %>%
  mutate(
    Education_Group = factor(Education_Group, 
                             levels = c("No College", "Some College", "Graduate Degree"))
  )

# Count and proportion for each group
agg_df <- long_df %>%
  group_by(Political.scale, Education_Group, Candidate) %>%
  summarise(
    Proportion = mean(Selected, na.rm = TRUE),
    N = n(),
    .groups = "drop"
  )

# Plot with smoothed curves and sample size annotations
ggplot(agg_df, aes(x = Political.scale, y = Proportion, color = Education_Group)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.75, linewidth = 1.1) +
  geom_text(aes(label = paste0("n=", N)), 
            size = 2.8, 
            position = position_jitter(width = 0.15, height = 0.015),
            show.legend = FALSE,
            color = "black") +
  facet_wrap(~ Candidate) +
  scale_color_manual(values = c("No College" = "red", "Some College" = "green", "Graduate Degree" = "blue")) +
  labs(
    title = "Smoothed Raw Probability of Selecting Candidate",
    subtitle = "By Political Ideology and Education Group (n shown)",
    x = "Political Ideology (0 = Liberal, 7 = Conservative)",
    y = "Proportion Selecting Candidate",
    color = "Education Group"
  ) +
  theme_minimal(base_size = 14)
library(dplyr)

# Mechanic (0/1), Political.scale (0–7), Income.Group (Low, Lower-Middle, etc.)

subset_counts <- Merged_Data_Cleaned %>%
  filter(Political.scale %in% 0:4) %>%
  group_by(Income.Group) %>%
  summarize(
    Total = n(),
    Chose_Mechanic = sum(Mechanic == 1, na.rm = TRUE),
    Proportion = round(mean(Mechanic == 1, na.rm = TRUE), 2)
  )

print(subset_counts)
# Create a function to count ideology by income group for a given candidate
get_ideology_income_counts <- function(candidate_var, data = Merged_Data_Cleaned) {
  data %>%
    filter(!is.na(.data[[candidate_var]]), !is.na(Political.scale), !is.na(Income.Group)) %>%
    filter(.data[[candidate_var]] == 1) %>%
    group_by(Income.Group, Political.scale) %>%
    summarise(Count = n(), .groups = "drop") %>%
    arrange(Income.Group, Political.scale)
}

# Run for each candidate
mech_counts <- get_ideology_income_counts("Mechanic")
teach_counts <- get_ideology_income_counts("Teacher")
vet_counts <- get_ideology_income_counts("Veteran")

mech_counts
teach_counts
vet_counts

library(dplyr)

# Replace with your actual candidate variable names if different
mechanic_counts <- Merged_Data_Cleaned %>%
  filter(!is.na(Political.scale), !is.na(Income.Group)) %>%
  group_by(Income.Group, Political.scale) %>%
  summarise(Mechanic = sum(Mechanic == 1, na.rm = TRUE), .groups = "drop")

teacher_counts <- Merged_Data_Cleaned %>%
  filter(!is.na(Political.scale), !is.na(Income.Group)) %>%
  group_by(Income.Group, Political.scale) %>%
  summarise(Teacher = sum(Teacher == 1, na.rm = TRUE), .groups = "drop")

veteran_counts <- Merged_Data_Cleaned %>%
  filter(!is.na(Political.scale), !is.na(Income.Group)) %>%
  group_by(Income.Group, Political.scale) %>%
  summarise(Veteran = sum(Veteran == 1, na.rm = TRUE), .groups = "drop")

# Print full breakdowns
print(mechanic_counts, n = Inf)
print(teacher_counts, n = Inf)
print(veteran_counts, n = Inf)

# Calculate average favorability for each populist message type
mean_education <- mean(Merged_Data_Cleaned$Education..1.favorability, na.rm = TRUE)
mean_taxes <- mean(Merged_Data_Cleaned$Taxes..1.favorability, na.rm = TRUE)
mean_agriculture <- mean(Merged_Data_Cleaned$Ag.2.favorability, na.rm = TRUE)

# Print results
mean_education
mean_taxes
mean_agriculture

library(dplyr)

# Filter relevant columns and group by party
Merged_Data_Cleaned %>%
  group_by(Party.registration) %>%
  summarise(
    Chose_Education_Populist = sum(Education..1.favorability == 1, na.rm = TRUE),
    Chose_Taxes_Populist = sum(Taxes..1.favorability == 1, na.rm = TRUE),
    Chose_Ag_Populist = sum(Ag.2.favorability == 1, na.rm = TRUE),
    Total_Respondents = n()
  ) %>%
  mutate(
    Percent_Education = round(100 * Chose_Education_Populist / Total_Respondents, 1),
    Percent_Taxes = round(100 * Chose_Taxes_Populist / Total_Respondents, 1),
    Percent_Ag = round(100 * Chose_Ag_Populist / Total_Respondents, 1)
  )
mean(Merged_Data_Cleaned$Education..1.favorability, na.rm = TRUE)
mean(Merged_Data_Cleaned$Taxes..1.favorability, na.rm = TRUE)
mean(Merged_Data_Cleaned$Ag.2.favorability, na.rm = TRUE)
# Create binary indicators for each populist message being chosen
Merged_Data_Cleaned <- Merged_Data_Cleaned %>%
  mutate(
    Education = ifelse(Education.choice == 1, 1, 0),
    Taxes = ifelse(Taxes.choice == 1, 1, 0),
    Ag = ifelse(Ag.choice == 1, 1, 0)
  )

# Summarize by Party
populist_by_party <- Merged_Data_Cleaned %>%
  group_by(Party.registration) %>%
  summarise(
    Total_Respondents = n(),
    Chose_Education_Populist = sum(Chose_Education_Populist, na.rm = TRUE),
    Chose_Taxes_Populist = sum(Chose_Taxes_Populist, na.rm = TRUE),
    Chose_Ag_Populist = sum(Chose_Ag_Populist, na.rm = TRUE)
  ) %>%
  mutate(
    Percent_Education = round(100 * Chose_Education_Populist / Total_Respondents, 1),
    Percent_Taxes = round(100 * Chose_Taxes_Populist / Total_Respondents, 1),
    Percent_Ag = round(100 * Chose_Ag_Populist / Total_Respondents, 1)
  )

# View the summary table
print(populist_by_party)

library(dplyr)

populist_summary <- Merged_Data_Cleaned %>%
  group_by(Party.registration) %>%
  summarise(
    Total_Respondents = n(),
    Chose_Education_Populist = sum(Education == 1, na.rm = TRUE),
    Chose_Taxes_Populist = sum(Taxes == 1, na.rm = TRUE),
    Chose_Ag_Populist = sum(Ag == 1, na.rm = TRUE),
    Percent_Education = round(100 * Chose_Education_Populist / Total_Respondents, 1),
    Percent_Taxes = round(100 * Chose_Taxes_Populist / Total_Respondents, 1),
    Percent_Ag = round(100 * Chose_Ag_Populist / Total_Respondents, 1)
  )
ggplot(favorability_data, aes(x = Avg_Favorability, y = Party.registration)) +
  geom_point(aes(color = Most_Populist_Issue), size = 5) +
  geom_text(aes(label = Most_Populist_Issue), nudge_x = 0.1, size = 4.5, hjust = 0) +
  scale_color_manual(values = issue_colors) +
  labs(
    title = "Favorability Scores and Message Selection by Party",
    x = "Average Favorability of Populist Messages",
    y = "Party Registration",
    color = "Most Picked Populist Message"
  ) +
  xlim(3.5, 5) +
  theme_minimal(base_size = 14)

library(ggplot2)
library(dplyr)

# Create data frame (replace this with your actual summary_df_2 if already defined)
summary_df_2 <- data.frame(
  Party.registration = rep(c("Republican", "Democratic", "Independent", "None"), each = 3),
  Message_Type = rep(c("Education", "Taxes", "Agriculture"), times = 4),
  Avg_Favorability = c(3.89, 4.00, 3.95, 4.53, 4.36, 4.38, 4.4, 4.0, 4.2, 4.3, 4.1, 4.5),  # Example numbers
  Proportion_Selected = c(0.61, 0.97, 0.75, 0.91, 0.96, 0.91, 0.71, 1, 0.86, 0.88, 0.81, 0.94)
)

# Reorder parties for cleaner display
summary_df_2$Party.registration <- factor(
  summary_df_2$Party.registration,
  levels = c("Republican", "Democratic", "Independent", "None")
)

# Color palette for message types
color_palette <- c("Education" = "#A1C6EA", "Taxes" = "#FBC4AB", "Agriculture" = "#B5EAD7")  # Pastel tones

# Plot
ggplot(summary_df_2, aes(
  x = Avg_Favorability,
  y = Party.registration,
  color = Message_Type,
  size = Proportion_Selected
)) +
  geom_point(alpha = 0.9) +
  geom_text(aes(label = round(Avg_Favorability, 2)), vjust = -1.2, size = 3.5) +
  scale_color_manual(values = color_palette) +
  scale_size_continuous(range = c(4, 12)) +  # Adjust bubble size range
  labs(
    title = "Favorability and Selection of Populist Messages by Party",
    subtitle = "Bubble size reflects % selecting the populist message. Color = Message Type",
    x = "Average Favorability Score (1–5)",
    y = "Party Registration",
    color = "Message Type",
    size = "Proportion Selected"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5),
    axis.text = element_text(size = 12)
  )

library(ggplot2)
library(dplyr)

# Ensure Party is a factor in desired order
summary_df$Party.registration <- factor(
  summary_df$Party.registration,
  levels = c("Republican", "Democratic", "Independent", "None")
)

# Re-plot with better bubble sizing
ggplot(summary_df, aes(
  x = Average_Favorability,
  y = Party.registration,
  color = Most_Picked_Issue,
  size = Proportion_Selected
)) +
  geom_point(alpha = 0.8) +
  scale_color_manual(
    values = c(
      "Education" = "#A1D99B",   # pastel green
      "Taxes" = "#9ECAE1",       # pastel blue
      "Agriculture" = "#FDAE6B"  # pastel orange
    )
  ) +
  scale_size(range = c(5, 18)) +  # adjust bubble size range
  labs(
    title = "Favorability and Selection of Populist Messages by Party",
    x = "Average Favorability (1–5)",
    y = "Party Registration",
    color = "Top Issue Chosen",
    size = "Proportion Selecting\nPopulist Message"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )

library(dplyr)

# Step 1: Create summary_df
summary_df <- Merged_Data_Cleaned %>%
  group_by(Party.registration) %>%
  summarise(
    Average_Favorability = mean(mean_populist_favorability, na.rm = TRUE),
    Proportion_Selected = mean((Education + Taxes + Ag)/3, na.rm = TRUE),
    Most_Picked_Issue = c("Education", "Taxes", "Ag")[which.max(c(
      mean(Education, na.rm = TRUE),
      mean(Taxes, na.rm = TRUE),
      mean(Ag, na.rm = TRUE)
    ))]
  ) %>%
  ungroup()

summary_df$Party.registration <- factor(
  summary_df$Party.registration,
  levels = c("Republican", "Democratic", "Independent", "None")
)

library(ggplot2)

ggplot(summary_df, aes(
  x = Average_Favorability,
  y = Party.registration,
  color = Most_Picked_Issue,
  size = Proportion_Selected
)) +
  geom_point(alpha = 0.8) +
  scale_color_manual(
    values = c(
      "Education" = "#A1D99B",   # pastel green
      "Taxes" = "#9ECAE1",       # pastel blue
      "Ag" = "#FDAE6B"           # pastel orange
    )
  ) +
  scale_size(range = c(5, 18)) +
  labs(
    title = "Favorability and Selection of Populist Messages by Party",
    x = "Average Favorability (1–5)",
    y = "Party Registration",
    color = "Top Issue Chosen",
    size = "Proportion Selecting\nPopulist Message"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )

summary_df <- Merged_Data_Cleaned %>%
  group_by(Party.registration) %>%
  summarise(
    Average_Favorability = mean(mean_populist_favorability, na.rm = TRUE),
    Proportion_Selected = mean((Education + Taxes + Ag)/3, na.rm = TRUE),
    Most_Picked_Issue = c("Education", "Taxes", "Ag")[which.max(c(
      mean(Education, na.rm = TRUE),
      mean(Taxes, na.rm = TRUE),
      mean(Ag, na.rm = TRUE)
    ))],
    .groups = "drop"
  )
summary_df$Party.registration <- factor(
  summary_df$Party.registration,
  levels = c("Republican", "Democratic", "Independent", "None")
)
ggplot(summary_df, aes(
  x = Average_Favorability,
  y = Party.registration,
  color = Most_Picked_Issue,
  size = Proportion_Selected
)) +
  geom_point(alpha = 0.8) +
  scale_color_manual(
    values = c(
      "Education" = "#B3E2CD",  # pastel green
      "Taxes" = "#A6CEE3",      # pastel blue
      "Ag" = "#FDBF6F"          # pastel orange
    )
  ) +
  scale_size(range = c(5, 18)) +
  labs(
    title = "Favorability and Selection of Populist Messages by Party",
    x = "Average Favorability (1–5)",
    y = "Party Registration",
    color = "Top Issue Chosen",
    size = "Proportion Selecting\nPopulist Message"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )

library(dplyr)

summary_df <- Merged_Data_Cleaned %>%
  group_by(Party.registration) %>%
  summarise(
    Average_Favorability = mean(mean_populist_favorability, na.rm = TRUE),
    Proportion_Selected = mean((Education + Taxes + Ag)/3, na.rm = TRUE),
    Most_Picked_Issue = c("Education", "Taxes", "Ag")[which.max(c(
      mean(Education, na.rm = TRUE),
      mean(Taxes, na.rm = TRUE),
      mean(Ag, na.rm = TRUE)
    ))],
    .groups = "drop"
  )

# Reorder party levels for plot
summary_df$Party.registration <- factor(
  summary_df$Party.registration,
  levels = c("Republican", "Democratic", "Independent", "None")
)

library(ggplot2)

ggplot(summary_df, aes(
  x = Average_Favorability,
  y = Party.registration,
  color = Most_Picked_Issue,
  size = Proportion_Selected
)) +
  geom_point(alpha = 0.85) +
  scale_color_manual(
    values = c(
      "Education" = "#B3E2CD",  # pastel green
      "Taxes" = "#A6CEE3",      # pastel blue
      "Ag" = "#FDBF6F"          # pastel orange
    )
  ) +
  scale_size_continuous(range = c(6, 18)) +
  labs(
    title = "Favorability and Selection of Populist Messages by Party",
    x = "Average Favorability (1–5)",
    y = "Party Registration",
    color = "Top Issue Chosen",
    size = "Proportion Selecting\nPopulist Message"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )

library(dplyr)

summary_df <- Merged_Data_Cleaned %>%
  group_by(Party.registration) %>%
  summarise(
    Avg_Favorability = mean(mean_populist_favorability, na.rm = TRUE),
    Prop_Education = mean(Education, na.rm = TRUE),
    Prop_Taxes = mean(Taxes, na.rm = TRUE),
    Prop_Ag = mean(Ag, na.rm = TRUE),
    Most_Picked_Issue = c("Education", "Taxes", "Ag")[which.max(c(
      mean(Education, na.rm = TRUE),
      mean(Taxes, na.rm = TRUE),
      mean(Ag, na.rm = TRUE)
    ))],
    Proportion_Selected = mean((Education + Taxes + Ag)/3, na.rm = TRUE),
    .groups = "drop"
  )

# Reorder party levels so the y-axis is clean
summary_df$Party.registration <- factor(
  summary_df$Party.registration,
  levels = c("Republican", "Democratic", "Independent", "None")
)

library(ggplot2)

ggplot(summary_df, aes(
  x = Avg_Favorability,
  y = Party.registration,
  color = Most_Picked_Issue,
  size = Proportion_Selected
)) +
  geom_point(alpha = 0.85) +
  scale_color_manual(
    values = c(
      "Education" = "#B3E2CD",  # pastel green
      "Taxes" = "#A6CEE3",      # pastel blue
      "Ag" = "#FDBF6F"          # pastel orange
    )
  ) +
  scale_size_continuous(range = c(6, 18)) +
  labs(
    title = "Favorability and Selection of Populist Messages by Party",
    x = "Average Favorability (1–5)",
    y = "Party Registration",
    color = "Top Issue Chosen",
    size = "Proportion Selecting\nPopulist Message"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )

summary_df <- Merged_Data_Cleaned %>%
  group_by(Party.registration) %>%
  summarise(
    Avg_Favorability = mean(mean_populist_favorability, na.rm = TRUE),
    Prop_Education = mean(Education, na.rm = TRUE),
    Prop_Taxes = mean(Taxes, na.rm = TRUE),
    Prop_Ag = mean(Ag, na.rm = TRUE),
    Most_Picked_Issue = case_when(
      Prop_Education >= Prop_Taxes & Prop_Education >= Prop_Ag ~ "Education",
      Prop_Taxes >= Prop_Education & Prop_Taxes >= Prop_Ag ~ "Taxes",
      TRUE ~ "Ag"
    ),
    Proportion_Selected = mean((Education + Taxes + Ag)/3, na.rm = TRUE),
    .groups = "drop"
  )

library(dplyr)

# Create the correct summary table
summary_df <- Merged_Data_Cleaned %>%
  group_by(Party.registration) %>%
  summarise(
    Avg_Favorability = mean(mean_populist_favorability, na.rm = TRUE),
    Prop_Education = mean(Education, na.rm = TRUE),
    Prop_Taxes = mean(Taxes, na.rm = TRUE),
    Prop_Ag = mean(Ag, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Most_Picked_Issue = case_when(
      Prop_Education >= Prop_Taxes & Prop_Education >= Prop_Ag ~ "Education",
      Prop_Taxes >= Prop_Education & Prop_Taxes >= Prop_Ag ~ "Taxes",
      TRUE ~ "Ag"
    ),
    Proportion_Selected = case_when(
      Most_Picked_Issue == "Education" ~ Prop_Education,
      Most_Picked_Issue == "Taxes" ~ Prop_Taxes,
      Most_Picked_Issue == "Ag" ~ Prop_Ag
    )

summary_df$Party.registration <- factor(
  summary_df$Party.registration,
  levels = c("Republican", "Democratic", "Independent", "None")
)
library(ggplot2)

ggplot(summary_df, aes(
  x = Avg_Favorability,
  y = Party.registration,
  color = Most_Picked_Issue,
  size = Proportion_Selected
)) +
  geom_point(alpha = 0.9) +
  scale_color_manual(
    values = c(
      "Education" = "#B3E2CD",  # soft green
      "Taxes" = "#A6CEE3",      # soft blue
      "Ag" = "#FDBF6F"          # soft orange
    )
  ) +
  scale_size_continuous(range = c(6, 20)) +
  labs(
    title = "Favorability and Selection of Populist Messages by Party",
    x = "Average Favorability (1–5)",
    y = "Party Registration",
    color = "Top Issue Chosen",
    size = "Proportion Selecting\nPopulist Message"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right"
  )

library(dplyr)

# Raw message selection count by party
raw_counts <- Merged_Data_Cleaned %>%
  group_by(Party.registration) %>%
  summarise(
    Total = n(),
    Chose_Education = sum(Education, na.rm = TRUE),
    Chose_Taxes = sum(Taxes, na.rm = TRUE),
    Chose_Ag = sum(Ag, na.rm = TRUE)
  ) %>%
  mutate(
    Most_Picked_Issue = case_when(
      Chose_Education >= Chose_Taxes & Chose_Education >= Chose_Ag ~ "Education",
      Chose_Taxes >= Chose_Education & Chose_Taxes >= Chose_Ag ~ "Taxes",
      TRUE ~ "Ag"
    )

print(raw_counts)

library(dplyr)

# Replace 'Merged_Data_Cleaned' if your dataset has a different name
summary_check <- Merged_Data_Cleaned %>%
  group_by(Party.registration) %>%
  summarise(
    Total = n(),
    Chose_Education = sum(Education == 1, na.rm = TRUE),
    Chose_Taxes = sum(Taxes == 1, na.rm = TRUE),
    Chose_Ag = sum(Ag == 1, na.rm = TRUE),
    Percent_Education = round(Chose_Education / Total * 100, 1),
    Percent_Taxes = round(Chose_Taxes / Total * 100, 1),
    Percent_Ag = round(Chose_Ag / Total * 100, 1),
    Avg_Favorability = round(
      rowMeans(
        cbind(Education..1.favorability, Taxes..1.favorability, Ag.2.favorability),
        na.rm = TRUE
      ) %>%
        tapply(Merged_Data_Cleaned$Party.registration, mean, na.rm = TRUE),
      2
    )[Party.registration],
    Most_Picked_Issue = c("Education", "Taxes", "Ag")[
      apply(
        cbind(Chose_Education, Chose_Taxes, Chose_Ag), 1,
        which.max
      )
    ]
  )

print(summary_check)

library(dplyr)

# Recalculate cleanly
summary_check <- Merged_Data_Cleaned %>%
  group_by(Party.registration) %>%
  summarise(
    Total = n(),
    Chose_Education = sum(Education == 1, na.rm = TRUE),
    Chose_Taxes = sum(Taxes == 1, na.rm = TRUE),
    Chose_Ag = sum(Ag == 1, na.rm = TRUE),
    Percent_Education = round(Chose_Education / Total * 100, 1),
    Percent_Taxes = round(Chose_Taxes / Total * 100, 1),
    Percent_Ag = round(Chose_Ag / Total * 100, 1),
    Avg_Favorability = round(
      mean(c(Education..1.favorability, Taxes..1.favorability, Ag.2.favorability), na.rm = TRUE),
      2
    ),
    Most_Picked_Issue = c("Education", "Taxes", "Ag")[
      which.max(c(Chose_Education, Chose_Taxes, Chose_Ag))
    ]
  )

print(summary_check)

print(summary_check, n = Inf, width = Inf)

Merged_Data_Cleaned <- Merged_Data_Cleaned %>%
  mutate(
    Selected_All_Three = ifelse(Education == 1 & Taxes == 1 & Ag == 1, 1, 0)
  )

summary_df <- Merged_Data_Cleaned %>%
  group_by(Party.registration) %>%
  summarise(
    Total = n(),
    Chose_Education = sum(Education == 1, na.rm = TRUE),
    Chose_Taxes = sum(Taxes == 1, na.rm = TRUE),
    Chose_Ag = sum(Ag == 1, na.rm = TRUE),
    Selected_All_Three = sum(Selected_All_Three == 1, na.rm = TRUE),
    Proportion_Selected = round(Selected_All_Three / Total, 2),
    Avg_Favorability = round(
      rowMeans(
        cbind(Education..1.favorability, Taxes..1.favorability, Ag.2.favorability),
        na.rm = TRUE
      ) %>%
        tapply(Merged_Data_Cleaned$Party.registration, mean, na.rm = TRUE),
      2
    )[Party.registration],
    Most_Picked_Issue = c("Education", "Taxes", "Ag")[
      apply(cbind(Chose_Education, Chose_Taxes, Chose_Ag), 1, which.max)
    ]
  )

library(dplyr)
library(ggplot2)

# Step 1: Create new variable for selecting all three populist messages
Merged_Data_Cleaned <- Merged_Data_Cleaned %>%
  mutate(
    Selected_All_Three = ifelse(Education == 1 & Taxes == 1 & Ag == 1, 1, 0)
  )

# Step 2: Summarize data by party
summary_df <- Merged_Data_Cleaned %>%
  group_by(Party.registration) %>%
  summarise(
    Total = n(),
    Chose_Education = sum(Education == 1, na.rm = TRUE),
    Chose_Taxes = sum(Taxes == 1, na.rm = TRUE),
    Chose_Ag = sum(Ag == 1, na.rm = TRUE),
    Selected_All_Three = sum(Selected_All_Three == 1, na.rm = TRUE),
    Proportion_Selected = round(Selected_All_Three / Total, 2),
    Avg_Favorability = round(
      mean(
        rowMeans(cbind(Education..1.favorability, Taxes..1.favorability, Ag.2.favorability),
                 na.rm = TRUE)[Party.registration == unique(Party.registration)]
      ),
      2
    ),
    Most_Picked_Issue = c("Education", "Taxes", "Ag")[
      apply(cbind(Chose_Education, Chose_Taxes, Chose_Ag), 1, which.max)
    ]
  )

print(summary_df)

# Step 3: Plot with improved circle size differences and party order
summary_df$Party.registration <- factor(
  summary_df$Party.registration,
  levels = c("Republican", "Democratic", "Independent", "None")
)

ggplot(summary_df, aes(
  x = Avg_Favorability,
  y = Party.registration,
  color = Most_Picked_Issue,
  size = Proportion_Selected
)) +
  geom_point(alpha = 0.9) +
  scale_color_manual(
    values = c(
      "Education" = "#B3E2CD",  # pastel green
      "Taxes" = "#A6CEE3",      # pastel blue
      "Ag" = "#FDBF6F"          # pastel orange
    )
  ) +
  scale_size_continuous(
    range = c(6, 20),
    breaks = c(0.3, 0.5, 0.7, 0.9, 1.0),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Favorability and Selection of Populist Messages by Party",
    x = "Average Favorability (1–5)",
    y = "Party Registration",
    color = "Top Issue Chosen",
    size = "Selected All Three Populist Messages"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right"
  )

# Define breaks based on actual data range
breaks <- c(0.61, 0.79, 0.94)

ggplot(summary_df, aes(
  x = Avg_Favorability,
  y = Party.registration,
  color = Most_Picked_Issue,
  size = Proportion_Selected
)) +
  geom_point(alpha = 0.9) +
  scale_color_manual(
    values = c(
      "Education" = "#B3E2CD",  # pastel green
      "Taxes" = "#A6CEE3",      # pastel blue
      "Ag" = "#FDBF6F"          # pastel orange
    )
  ) +
  scale_size_continuous(
    range = c(6, 20),
    breaks = breaks,
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Favorability and Selection of Populist Messages by Party",
    x = "Average Favorability (1–5)",
    y = "Party Registration",
    color = "Top Issue Chosen",
    size = "Selected All Three Populist Messages"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right"
  )

print(summary_df)

library(ggplot2)
library(dplyr)

# Step 1: Create accurate summary data
summary_df <- tibble::tibble(
  Party.registration = c("Republican", "Democratic", "Independent", "None"),
  Avg_Favorability = c(3.97, 3.88, 3.76, 4.15),
  Most_Picked_Issue = c("Taxes", "Taxes", "Taxes", "Ag"),
  Proportion_Selected = c(0.61, 0.79, 0.71, 0.94)
)

# Step 2: Reorder factor levels for display
summary_df$Party.registration <- factor(
  summary_df$Party.registration,
  levels = c("Republican", "Democratic", "Independent", "None")
)

# Step 3: Bubble plot
ggplot(summary_df, aes(
  x = Avg_Favorability,
  y = Party.registration,
  color = Most_Picked_Issue,
  size = Proportion_Selected
)) +
  geom_point(alpha = 0.9) +
  scale_color_manual(
    values = c(
      "Education" = "#B3E2CD",  # pastel green
      "Taxes" = "#A6CEE3",      # pastel blue
      "Ag" = "#FDBF6F"          # pastel orange
    )
  ) +
  scale_size_continuous(
    range = c(8, 25),
    breaks = c(0.6, 0.7, 0.8, 0.9),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Favorability and Selection of Populist Messages by Party",
    x = "Average Favorability (1–5)",
    y = "Party Registration",
    color = "Top Issue Chosen",
    size = "Selected All Three Populist Messages"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right"
  )

scale_size_continuous(
  range = c(6, 25),
  breaks = c(0.6, 0.7, 0.8, 0.9),
  labels = scales::percent_format(accuracy = 1)
)

library(ggplot2)
library(dplyr)
library(scales)

# Bubble size rescale (manually defined for clarity)
rescale_bubble <- function(x) {
  min_size <- 10
  max_size <- 25
  min_val <- min(x, na.rm = TRUE)
  max_val <- max(x, na.rm = TRUE)
  scaled <- min_size + (x - min_val) / (max_val - min_val) * (max_size - min_size)
  return(scaled)
}

# Rescale bubble sizes
summary_df <- summary_df %>%
  mutate(
    BubbleSize = rescale_bubble(Proportion_Selected),
    Label = paste0(round(Proportion_Selected * 100), "%")
  )

# Create bubble plot
ggplot(summary_df, aes(
  x = Avg_Favorability,
  y = Party.registration,
  color = Most_Picked_Issue,
  size = BubbleSize
)) +
  geom_point(alpha = 0.9) +
  scale_color_manual(
    values = c(
      "Education" = "#B3E2CD",  # soft green
      "Taxes" = "#A6CEE3",      # soft blue
      "Ag" = "#FDBF6F"          # soft orange
    )
  ) +
  scale_size_identity(guide = "legend", breaks = rescale_bubble(c(0.65, 0.75, 0.85, 0.95)),
                      labels = c("65%", "75%", "85%", "95%"),
                      name = "Selected All Three Populist Messages") +
  geom_text(aes(label = Label), color = "black", size = 3.5, fontface = "bold") +
  labs(
    title = "Favorability and Selection of Populist Messages by Party",
    x = "Average Favorability (1–5)",
    y = "Party Registration",
    color = "Top Issue Chosen"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right"
  )

library(ggplot2)
library(dplyr)
library(scales)

# Your summary data
summary_df <- data.frame(
  Party.registration = c("Republican", "Democratic", "Independent", "None"),
  Avg_Favorability = c(3.97, 3.88, 3.76, 4.15),
  Proportion_Selected = c(0.61, 0.79, 0.71, 0.94),
  Most_Picked_Issue = c("Taxes", "Taxes", "Taxes", "Ag")
)

# Add label column
summary_df$Label <- paste0(round(summary_df$Proportion_Selected * 100), "%")

# Reorder for y-axis
summary_df$Party.registration <- factor(summary_df$Party.registration, 
                                        levels = c("Republican", "Democratic", "Independent", "None"))

# Custom color palette
issue_colors <- c(
  "Education" = "#B3E2CD",  # pastel green
  "Taxes" = "#A6CEE3",      # pastel blue
  "Ag" = "#FDBF6F"          # pastel orange
)

# Plot
ggplot(summary_df, aes(
  x = Avg_Favorability,
  y = Party.registration,
  color = Most_Picked_Issue,
  size = Proportion_Selected
)) +
  geom_point(alpha = 0.7) +
  geom_text(aes(label = Label), color = "black", size = 3, fontface = "plain") +
  scale_color_manual(values = issue_colors, name = "Top Issue Chosen") +
  scale_size_continuous(
    name = "Selected All Three Populist Messages",
    range = c(10, 35),
    breaks = c(0.65, 0.75, 0.85, 0.95),
    labels = c("65%", "75%", "85%", "95%")
  ) +
  labs(
    title = "Favorability and Selection of Populist Messages by Party",
    x = "Average Favorability (1–5)",
    y = "Party Registration"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    legend.position = "right"
  )

library(ggplot2)
library(dplyr)
library(scales)

# Reorder party factor if needed
summary_df$Party.registration <- factor(
  summary_df$Party.registration,
  levels = c("Republican", "Democratic", "Independent", "None")
)

# Rename "Ag" to "Agriculture" for display purposes
summary_df <- summary_df %>%
  mutate(Most_Picked_Issue = recode(Most_Picked_Issue, "Ag" = "Agriculture"))

# Plot
ggplot(summary_df, aes(
  x = Avg_Favorability,
  y = Party.registration,
  color = Most_Picked_Issue,
  size = Proportion_Selected
)) +
  geom_point(alpha = 0.8) +
  geom_text(
    aes(label = paste0(round(Proportion_Selected * 100), "%")),
    color = "black",
    size = 3.5,
    fontface = "plain"
  ) +
  scale_color_manual(
    values = c(
      "Education" = "#B3E2CD",
      "Taxes" = "#A6CEE3",
      "Agriculture" = "#FDBF6F"
    )
  ) +
  scale_size_continuous(
    range = c(6, 28), guide = "none"
  ) +
  labs(
    title = "Favorability and Selection of Populist Messages by Party",
    x = "Average Favorability (1–5)",
    y = "Party Registration",
    color = "Top Issue Chosen",
    caption = "Percentages represent the proportion of party respondents who selected all three economic populist messages."
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    plot.caption = element_text(hjust = 0.5, size = 11)
  )

library(ggplot2)
library(dplyr)

# Party.registration, Avg_Favorability, Most_Picked_Issue, Proportion_Selected

ggplot(summary_df, aes(
  x = Avg_Favorability,
  y = Party.registration,
  color = Most_Picked_Issue,
  size = Proportion_Selected
)) +
  geom_point(alpha = 0.9) +
  geom_text(aes(label = paste0(round(Proportion_Selected * 100), "%")),
            color = "black",
            size = 3,    # smaller text inside bubbles
            fontface = "plain",  # not bold
            show.legend = FALSE) +
  scale_color_manual(
    name = "Top Issue Chosen",
    values = c(
      "Education" = "#B3E2CD",
      "Taxes" = "#A6CEE3",
      "Agriculture" = "#FDBF6F"  # renamed from "Ag"
    )
  ) +
  scale_size_continuous(range = c(6, 25), guide = "none") +  # remove circle size legend
  labs(
    title = "Favorability and Selection of Populist Messages by Party",
    x = "Average Favorability (1–5)",
    y = "Party Registration",
    caption = "Percentages represent the proportion of party respondents who selected all three economic populist messages."
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13),
    plot.caption = element_text(size = 11, hjust = 0.5),
    legend.position = "right"
    + guides(color = guide_legend(override.aes = list(size = 6)))
  )

library(ggplot2)
library(dplyr)

# Bubble plot
ggplot(summary_df, aes(
  x = Avg_Favorability,
  y = Party.registration,
  color = Most_Picked_Issue,
  size = Proportion_Selected
)) +
  geom_point(alpha = 0.8) +
  geom_text(aes(label = paste0(round(Proportion_Selected * 100), "%")),
            color = "black", size = 4, fontface = "plain", show.legend = FALSE,
            vjust = 0.5, hjust = 0.5) +
  scale_color_manual(
    values = c(
      "Education" = "#B3E2CD",     # soft green
      "Taxes" = "#A6CEE3",         # soft blue
      "Agriculture" = "#FDBF6F"    # soft orange
    )
  ) +
  scale_size_continuous(
    range = c(6, 30),
    breaks = c(0.65, 0.75, 0.85, 0.95),
    labels = c("65%", "75%", "85%", "95%"),
    name = "Selected All Three Populist Messages"
  ) +
  labs(
    title = "Favorability and Selection of Populist Messages by Party",
    x = "Average Favorability (1–5)",
    y = "Party Registration",
    color = "Top Issue Chosen",
    caption = "Percentages represent the proportion of party respondents who selected all three economic populist messages."
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    plot.caption = element_text(hjust = 0.5, size = 10),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11)
  ) +
  guides(color = guide_legend(override.aes = list(size = 6)))

library(ggplot2)
library(dplyr)

# Bubble Plot
ggplot(summary_df, aes(
  x = Avg_Favorability,
  y = Party.registration,
  color = Most_Picked_Issue,
  size = Proportion_Selected
)) +
  geom_point(alpha = 0.8) +
  geom_text(
    aes(label = paste0(round(Proportion_Selected * 100), "%")),
    color = "black",
    size = 3.5, # smaller text
    fontface = "plain",
    show.legend = FALSE,
    vjust = 0.5,
    hjust = 0.5
  ) +
  scale_color_manual(
    values = c(
      "Education" = "#B3E2CD",     # soft green
      "Taxes" = "#A6CEE3",         # soft blue
      "Agriculture" = "#FDBF6F"    # soft orange
    )
  ) +
  scale_size_continuous(
    range = c(6, 30),  # adjust size of points
    guide = "none"     # remove bubble size legend
  ) +
  labs(
    title = "Favorability and Selection of Populist Messages by Party",
    x = "Average Favorability (1–5)",
    y = "Party Registration",
    color = "Top Issue Chosen",
    caption = "Percentages represent the proportion of party respondents who selected all three economic populist messages."
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 7)) # make colored circles in legend larger
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    plot.caption = element_text(hjust = 0.5, size = 10)
  )

library(ggplot2)
library(dplyr)
library(grid)  # for annotation

# Adjust bubble size scaling to make Republican more visible
min_bubble_size <- 6
max_bubble_size <- 25

ggplot(summary_df, aes(
  x = Avg_Favorability,
  y = Party.registration,
  color = Most_Picked_Issue,
  size = Proportion_Selected
)) +
  geom_point(alpha = 0.8) +
  geom_text(
    aes(label = paste0(round(Proportion_Selected * 100), "%")),
    color = "black",
    size = 3.5,
    fontface = "plain",
    show.legend = FALSE,
    vjust = 0.5,
    hjust = 0.5
  ) +
  scale_color_manual(
    values = c(
      "Education" = "#B3E2CD",     # soft green
      "Taxes" = "#A6CEE3",         # soft blue
      "Agriculture" = "#FDBF6F"    # soft orange
    )
  ) +
  scale_size_continuous(
    range = c(min_bubble_size, max_bubble_size),
    guide = "none"
  ) +
  labs(
    title = "Favorability and Selection of Populist Messages by Party",
    x = "Average Favorability (1–5)",
    y = "Party Registration",
    color = "Top Issue Chosen"
  ) +
  guides(
    color = guide_legend(
      override.aes = list(size = 7)
    )
  ) +
  annotate(
    "text", x = Inf, y = -Inf,
    label = "Bubble percentages indicate the share of respondents within each party who selected all three economic populist messages.",
    hjust = 1.1, vjust = -15,
    size = 3.6
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    plot.margin = margin(10, 30, 10, 10)
  )
library(ggplot2)
library(dplyr)

# Bubble size range tuned to make 61% more visible
min_bubble_size <- 8
max_bubble_size <- 22

ggplot(summary_df, aes(
  x = Avg_Favorability,
  y = Party.registration,
  color = Most_Picked_Issue,
  size = Proportion_Selected
)) +
  geom_point(alpha = 0.8) +
  geom_text(
    aes(label = paste0(round(Proportion_Selected * 100), "%")),
    color = "black",
    size = 3.2,
    fontface = "plain",
    show.legend = FALSE,
    vjust = 0.5,
    hjust = 0.5
  ) +
  scale_color_manual(
    values = c(
      "Education" = "#B3E2CD",
      "Taxes" = "#A6CEE3",
      "Agriculture" = "#FDBF6F"
    )
  ) +
  scale_size_continuous(
    range = c(min_bubble_size, max_bubble_size),
    guide = "none"
  ) +
  annotate(
    "text",
    x = 4.25, y = 0.7,  # Adjust this if needed
    label = "Bubble percentages show share of respondents\nin each party who selected all three economic populist messages.",
    hjust = 0,
    size = 3.4,
    color = "gray30"
  ) +
  labs(
    title = "Favorability and Selection of Populist Messages by Party",
    x = "Average Favorability (1–5)",
    y = "Party Registration",
    color = "Top Issue Chosen"
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 6))
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1.3, "lines"),
    legend.position = "right"
  )

library(ggplot2)
library(dplyr)
library(scales)

# Sample data
summary_df <- data.frame(
  Party.registration = c("Republican", "Democratic", "Independent", "None"),
  Avg_Favorability = c(4.00, 3.88, 3.76, 4.15),
  Proportion_Selected = c(0.66, 0.79, 0.71, 0.94),  # bumped up Republican from 0.61 to 0.66
  Most_Picked_Issue = c("Taxes", "Taxes", "Taxes", "Agriculture")
)

summary_df$Label <- paste0(round(summary_df$Proportion_Selected * 100), "%")

# Define colors
issue_colors <- c("Agriculture" = "#FDBF6F", "Taxes" = "#A6CEE3")

# Reorder factor for vertical axis
summary_df$Party.registration <- factor(
  summary_df$Party.registration,
  levels = c("Republican", "Democratic", "Independent", "None")
)

# Plot
ggplot(summary_df, aes(
  x = Avg_Favorability,
  y = Party.registration,
  size = Proportion_Selected,
  color = Most_Picked_Issue
)) +
  geom_point(alpha = 0.85, show.legend = TRUE) +
  geom_text(aes(label = Label), color = "black", size = 3, fontface = "plain") +
  scale_color_manual(values = issue_colors) +
  scale_size_continuous(
    range = c(10, 40),
    guide = "none"
  ) +
  labs(
    title = "Favorability and Selection of Populist Messages by Party",
    x = "Average Favorability (1–5)",
    y = "Party Registration",
    color = "Top Issue Chosen"
  ) +
  annotate(
    "text", x = 4.27, y = 0.7,
    label = "Bubble percentages indicate the share of respondents\nwithin each party who selected all three economic populist messages.",
    size = 3, hjust = 0
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    legend.key.size = unit(1.2, "lines")
  )

library(ggplot2)
library(dplyr)

# Data
summary_df <- data.frame(
  Party.registration = c("Republican", "Democratic", "Independent", "None"),
  Avg_Favorability = c(3.97, 3.88, 3.76, 4.15),
  Proportion_Selected = c(0.61, 0.79, 0.71, 0.94),
  Most_Picked_Issue = c("Taxes", "Taxes", "Taxes", "Agriculture")
)

summary_df$Label <- paste0(round(summary_df$Proportion_Selected * 100), "%")

# Adjust size scaling manually to make GOP more visible but still proportional
min_size <- 8
max_size <- 30
summary_df$BubbleSize <- scales::rescale(summary_df$Proportion_Selected,
                                         to = c(min_size, max_size))

# Bubble plot
ggplot(summary_df, aes(
  x = Avg_Favorability,
  y = Party.registration,
  color = Most_Picked_Issue
)) +
  geom_point(aes(size = BubbleSize), alpha = 0.7, show.legend = FALSE) +
  geom_text(aes(label = Label), color = "black", size = 4, fontface = "plain") +
  scale_color_manual(
    values = c("Agriculture" = "#FDBF6F", "Taxes" = "#A6CEE3"),
    name = "Top Issue Chosen"
  ) +
  scale_size_identity() +
  labs(
    title = "Favorability and Selection of Populist Messages by Party",
    x = "Average Favorability (1–5)",
    y = "Party Registration"
  ) +
  annotate("text", x = 4.45, y = 0.5,
           label = "Bubble percentages indicate the share of respondents within each party\nwho selected all three economic populist messages.",
           size = 3.5, hjust = 1, color = "gray30") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12)
  )

library(ggplot2)
library(dplyr)
library(scales)

# Create data
summary_df <- data.frame(
  Party.registration = factor(c("Republican", "Democratic", "Independent", "None"),
                              levels = c("Republican", "Democratic", "Independent", "None")),
  Avg_Favorability = c(3.97, 3.88, 3.76, 4.15),
  Proportion_Selected = c(0.61, 0.79, 0.71, 0.94),
  Most_Picked_Issue = c("Taxes", "Taxes", "Taxes", "Agriculture")
)

summary_df$Label <- paste0(round(summary_df$Proportion_Selected * 100), "%")

# Manually set sizes to reflect scale differences without over-minimizing GOP
bubble_size_scaled <- rescale(summary_df$Proportion_Selected, to = c(10, 40))

# Base plot
p <- ggplot(summary_df, aes(
  x = Avg_Favorability,
  y = Party.registration,
  color = Most_Picked_Issue
)) +
  geom_point(aes(size = bubble_size_scaled), alpha = 0.75, show.legend = FALSE) +
  geom_text(aes(label = Label), size = 4, color = "black") +  # smaller plain text
  scale_color_manual(
    values = c("Taxes" = "#A6CEE3", "Agriculture" = "#FDBF6F"),
    name = "Top Issue Chosen",
    guide = guide_legend(override.aes = list(size = 6))  # make legend dots larger
  ) +
  scale_size_identity() +
  labs(
    title = "Favorability and Selection of Populist Messages by Party",
    x = "Average Favorability (1–5)",
    y = "Party Registration"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    plot.margin = margin(10, 40, 10, 10)
  ) +
  # Add annotation in legend area using grid::textGrob (for guaranteed visibility)
  annotate("text", x = 4.5, y = 0.5,
           label = "Bubble percentages indicate the share of respondents within each party\nwho selected all three economic populist messages.",
           hjust = 1, size = 3.5, color = "gray30")

print(p)

library(ggplot2)
library(dplyr)
library(scales)

# Create data
summary_df <- data.frame(
  Party.registration = factor(c("Republican", "Democratic", "Independent", "None"),
                              levels = c("Republican", "Democratic", "Independent", "None")),
  Avg_Favorability = c(3.97, 3.88, 3.76, 4.15),
  Proportion_Selected = c(0.61, 0.79, 0.71, 0.94),
  Most_Picked_Issue = c("Taxes", "Taxes", "Taxes", "Agriculture")
)

summary_df$Label <- paste0(round(summary_df$Proportion_Selected * 100), "%")

# ✅ Use range instead of `to` for rescale compatibility
bubble_size_scaled <- rescale(summary_df$Proportion_Selected, range = c(10, 40))

# Base plot
ggplot(summary_df, aes(
  x = Avg_Favorability,
  y = Party.registration,
  color = Most_Picked_Issue
)) +
  geom_point(aes(size = bubble_size_scaled), alpha = 0.75, show.legend = FALSE) +
  geom_text(aes(label = Label), size = 4, color = "black") +
  scale_color_manual(
    values = c("Taxes" = "#A6CEE3", "Agriculture" = "#FDBF6F"),
    name = "Top Issue Chosen",
    guide = guide_legend(override.aes = list(size = 6))
  ) +
  scale_size_identity() +
  labs(
    title = "Favorability and Selection of Populist Messages by Party",
    x = "Average Favorability (1–5)",
    y = "Party Registration"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    plot.margin = margin(10, 40, 10, 10)
  ) +
  # Add annotation above the x-axis near the legend
  annotate("text", x = 4.5, y = 0.5,
           label = "Bubble percentages indicate the share of respondents within each party\nwho selected all three economic populist messages.",
           hjust = 1, size = 3.5, color = "gray30")
library(ggplot2)
library(dplyr)
library(scales)

# Create data
summary_df <- data.frame(
  Party.registration = factor(c("Republican", "Democratic", "Independent", "None"),
                              levels = c("Republican", "Democratic", "Independent", "None")),
  Avg_Favorability = c(3.97, 3.88, 3.76, 4.15),
  Proportion_Selected = c(0.61, 0.79, 0.71, 0.94),
  Most_Picked_Issue = c("Taxes", "Taxes", "Taxes", "Agriculture")
)

summary_df$Label <- paste0(round(summary_df$Proportion_Selected * 100), "%")

# ✅ Use scales::rescale and to = c(...)
bubble_size_scaled <- scales::rescale(summary_df$Proportion_Selected, to = c(10, 40))

# Plot
ggplot(summary_df, aes(
  x = Avg_Favorability,
  y = Party.registration,
  color = Most_Picked_Issue
)) +
  geom_point(aes(size = bubble_size_scaled), alpha = 0.75, show.legend = FALSE) +
  geom_text(aes(label = Label), size = 3.5, color = "black") +
  scale_color_manual(
    values = c("Taxes" = "#A6CEE3", "Agriculture" = "#FDBF6F"),
    name = "Top Issue Chosen",
    guide = guide_legend(override.aes = list(size = 6))
  ) +
  scale_size_identity() +
  labs(
    title = "Favorability and Selection of Populist Messages by Party",
    x = "Average Favorability (1–5)",
    y = "Party Registration"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    plot.margin = margin(10, 40, 10, 10)
  ) +
  annotate("text", x = 4.45, y = 0.5,
           label = "Bubble percentages indicate the share of respondents within each party\nwho selected all three economic populist messages.",
           hjust = 1, size = 3.5, color = "gray30")

# Required libraries
library(ggplot2)
library(dplyr)
library(scales)

# If not already, create your summary_df with Proportion_Selected and Most_Picked_Issue
# Make sure it includes these columns: Party.registration, Avg_Favorability, Proportion_Selected, Most_Picked_Issue

# Rescale bubble sizes
summary_df$bubble_size_scaled <- scales::rescale(summary_df$Proportion_Selected, to = c(10, 50))

# Format the percentage labels
summary_df$label <- paste0(round(summary_df$Proportion_Selected * 100), "%")

# Plot
ggplot(summary_df, aes(
  x = Avg_Favorability,
  y = Party.registration,
  size = bubble_size_scaled,
  fill = Most_Picked_Issue
)) +
  geom_point(shape = 21, alpha = 0.8, color = "white") +
  scale_fill_manual(
    values = c("Taxes" = "#A6CEE3", "Agriculture" = "#FDBF6F"),
    name = "Top Issue Chosen"
  ) +
  geom_text(aes(label = label), size = 4, color = "black", fontface = "plain") +
  scale_size_identity() +
  labs(
    title = "Favorability and Selection of Populist Messages by Party",
    x = "Average Favorability (1–5)",
    y = "Party Registration"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11)
  ) +
  annotate(
    "text", x = Inf, y = -Inf,
    label = "Bubble percentages indicate the share of respondents within each party\nwho selected all three economic populist messages.",
    hjust = 1.1, vjust = -1.5, size = 3.5
  )

library(ggplot2)
library(dplyr)
library(scales)

# Double-check the summary_df includes the following:
# Party.registration, Avg_Favorability, Proportion_Selected (as decimal), Most_Picked_Issue

# Scale bubble size
summary_df$bubble_size_scaled <- rescale(summary_df$Proportion_Selected, to = c(8, 45))

# Create percentage labels
summary_df$label <- paste0(round(summary_df$Proportion_Selected * 100), "%")

# Plot
ggplot(summary_df, aes(
  x = Avg_Favorability,
  y = Party.registration,
  size = bubble_size_scaled,
  fill = Most_Picked_Issue
)) +
  geom_point(shape = 21, color = "white", alpha = 0.85) +
  geom_text(aes(label = label), size = 3.5, fontface = "plain", color = "black") +
  scale_fill_manual(
    values = c("Taxes" = "#A6CEE3", "Agriculture" = "#FDBF6F"),
    name = "Top Issue Chosen"
  ) +
  scale_size_identity() +
  labs(
    title = "Favorability and Selection of Populist Messages by Party",
    x = "Average Favorability (1–5)",
    y = "Party Registration"
  ) +
  annotate(
    "text",
    x = max(summary_df$Avg_Favorability) + 0.05,
    y = 0.6,
    label = "Bubble percentages indicate the share of respondents\nwithin each party who selected all three economic populist messages.",
    hjust = 1,
    size = 3.5
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    legend.position = "right",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11),
    plot.margin = margin(10, 40, 10, 10) # top, right, bottom, left
  )

library(ggplot2)

# Manually define your data (you can replace this with your actual summary_df)
summary_df <- data.frame(
  Party.registration = c("Republican", "Democratic", "Independent", "None"),
  Avg_Favorability = c(3.98, 3.88, 3.76, 4.15),
  Most_Picked_Issue = c("Taxes", "Taxes", "Taxes", "Agriculture"),
  Proportion_Selected = c(0.61, 0.79, 0.71, 0.94),
  Manual_Bubble_Size = c(20, 50, 40, 70),  # Adjust these manually for best balance
  Label = c("61%", "79%", "71%", "94%")
)

# Make Party a factor to preserve order
summary_df$Party.registration <- factor(
  summary_df$Party.registration,
  levels = c("Republican", "Democratic", "Independent", "None")
)

# Plot
ggplot(summary_df, aes(
  x = Avg_Favorability,
  y = Party.registration,
  color = Most_Picked_Issue,
  size = Manual_Bubble_Size
)) +
  geom_point(alpha = 0.7) +
  geom_text(aes(label = Label), color = "black", size = 3.5, fontface = "plain") +
  scale_color_manual(
    values = c("Agriculture" = "#FDBF6F", "Taxes" = "#A6CEE3"),
    name = "Top Issue Chosen",
    guide = guide_legend(override.aes = list(size = 6))  # Make legend dots bigger
  ) +
  scale_size_identity(guide = "none") +  # Use manual size but remove redundant size legend
  labs(
    title = "Favorability and Selection of Populist Messages by Party",
    x = expression(bold("Average Favorability (1–5)")),
    y = "Party Registration"
  ) +
  annotate(
    "text",
    x = 4.2, y = 0.5,
    label = "Bubble percentages indicate the share of respondents within each party\nwho selected all three economic populist messages.",
    hjust = 1, vjust = 0,
    size = 3.2, fontface = "italic", color = "gray20"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(face = "bold")
  )

library(ggplot2)
library(dplyr)

# Manual bubble sizes for each group to fix proportional balance
summary_df <- summary_df %>%
  mutate(
    Manual_Size = case_when(
      Party.registration == "Republican" ~ 14,
      Party.registration == "Democratic" ~ 30,
      Party.registration == "Independent" ~ 25,
      Party.registration == "None" ~ 45
    )

ggplot(summary_df, aes(
  x = Avg_Favorability,
  y = Party.registration,
  color = Most_Picked_Issue,
  size = Manual_Size
)) +
  geom_point(alpha = 0.8) +
  geom_text(
    aes(label = paste0(round(Proportion_Selected * 100), "%")),
    color = "black",
    size = 4,
    fontface = "plain",
    show.legend = FALSE,
    vjust = 0.5
  ) +
  scale_color_manual(
    values = c("Agriculture" = "#FDBF6F", "Taxes" = "#A6CEE3"),
    guide = guide_legend(
      override.aes = list(size = 6)
    )
  ) +
  scale_size_identity(guide = "none") +
  labs(
    title = "Favorability and Selection of Populist Messages by Party",
    x = "Average Favorability (1–5)",
    y = "Party Registration",
    color = "Top Issue Chosen"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 12),
    plot.margin = margin(20, 40, 40, 20)
  ) +
  annotate(
    "text",
    x = max(summary_df$Avg_Favorability) + 0.05,
    y = 1.1,
    label = "Bubble percentages indicate the share of respondents\nwithin each party who selected all three economic populist messages.",
    hjust = 0,
    size = 3.5,
    fontface = "italic"
  )
library(ggplot2)
library(dplyr)

# Reorder Party factor for political spectrum flow (Rep → Dem → Ind → None)
summary_df$Party.registration <- factor(
  summary_df$Party.registration,
  levels = c("Republican", "Democratic", "Independent", "None")
)

# Manual sizes for balance
summary_df <- summary_df %>%
  mutate(
    Manual_Size = case_when(
      Party.registration == "Republican" ~ 14,
      Party.registration == "Democratic" ~ 30,
      Party.registration == "Independent" ~ 25,
      Party.registration == "None" ~ 45
    )

# Final polished plot
ggplot(summary_df, aes(
  x = Avg_Favorability,
  y = Party.registration,
  color = Most_Picked_Issue,
  size = Manual_Size
)) +
  geom_point(alpha = 0.8) +
  geom_text(
    aes(label = paste0(round(Proportion_Selected * 100), "%")),
    color = "black",
    size = 4,
    fontface = "plain",
    show.legend = FALSE,
    vjust = 0.5
  ) +
  scale_color_manual(
    values = c("Agriculture" = "#FDBF6F", "Taxes" = "#A6CEE3"),
    guide = guide_legend(
      override.aes = list(size = 6)
    )
  ) +
  scale_size_identity(guide = "none") +
  labs(
    title = "Favorability and Selection of Economic Populist Messages by Party",
    x = "Average Favorability (1–5)",
    y = "Party Registration",
    color = "Top Issue Chosen"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
    axis.title.x = element_text(face = "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.margin = margin(t = 10, r = 30, b = 10, l = 10),
    plot.margin = margin(20, 40, 40, 20)
  ) +
  annotate(
    "text",
    x = max(summary_df$Avg_Favorability) + 0.05,
    y = 1,
    label = "Bubble percentages indicate the share of respondents\nwithin each party who selected all three economic populist messages.",
    hjust = 0,
    size = 3.5,
    fontface = "italic"
  )

library(ggplot2)
library(dplyr)

# Reorder Party factor for desired top-to-bottom flow
summary_df$Party.registration <- factor(
  summary_df$Party.registration,
  levels = c("Republican", "Democratic", "Independent", "None")
)

# Keep your manual sizes for visual clarity
summary_df <- summary_df %>%
  mutate(
    Manual_Size = case_when(
      Party.registration == "Republican" ~ 14,
      Party.registration == "Democratic" ~ 30,
      Party.registration == "Independent" ~ 25,
      Party.registration == "None" ~ 45
    )

# Final plot
ggplot(summary_df, aes(
  x = Avg_Favorability,
  y = Party.registration,
  color = Most_Picked_Issue,
  size = Manual_Size
)) +
  geom_point(alpha = 0.8) +
  geom_text(
    aes(label = paste0(round(Proportion_Selected * 100), "%")),
    color = "black",
    size = 4,
    fontface = "plain",
    show.legend = FALSE,
    vjust = 0.5
  ) +
  scale_color_manual(
    values = c("Agriculture" = "#FDBF6F", "Taxes" = "#A6CEE3"),
    guide = guide_legend(
      override.aes = list(size = 6)
    )
  ) +
  scale_size_identity(guide = "none") +
  labs(
    title = "Favorability and Selection of Economic Populist Messages by Party",
    x = "Average Favorability (1–5)",
    y = "Party Registration",
    color = "Top Issue Chosen"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
    axis.title.x = element_text(face = "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.margin = margin(t = 10, r = 30, b = 10, l = 10),
    plot.margin = margin(20, 40, 40, 20)
  ) +
  annotate(
    "text",
    x = max(summary_df$Avg_Favorability) + 0.05,
    y = 1,
    label = "Bubble percentages indicate the share of respondents within each party\nwho selected all three economic populist messages.",
    hjust = 0,
    size = 3.5,
    fontface = "italic"
  )

library(ggplot2)
library(dplyr)

# Set party order for y-axis (top to bottom: None → Independent → Democratic → Republican)
summary_df$Party.registration <- factor(
  summary_df$Party.registration,
  levels = c("Republican", "Democratic", "Independent", "None")
)

# Manual bubble sizes for visual clarity
summary_df <- summary_df %>%
  mutate(
    Manual_Size = case_when(
      Party.registration == "Republican" ~ 14,
      Party.registration == "Democratic" ~ 30,
      Party.registration == "Independent" ~ 25,
      Party.registration == "None" ~ 45
    )

# Final plot
ggplot(summary_df, aes(
  x = Avg_Favorability,
  y = Party.registration,
  color = Most_Picked_Issue,
  size = Manual_Size
)) +
  geom_point(alpha = 0.8) +
  geom_text(
    aes(label = paste0(round(Proportion_Selected * 100), "%")),
    color = "black",
    size = 4,
    fontface = "plain",
    show.legend = FALSE,
    vjust = 0.5
  ) +
  scale_color_manual(
    values = c("Agriculture" = "#FDBF6F", "Taxes" = "#A6CEE3"),
    guide = guide_legend(override.aes = list(size = 6))
  ) +
  scale_size_identity(guide = "none") +
  labs(
    title = "Favorability and Selection of Economic Populist Messages by Party",
    x = "Average Favorability (1–5)",
    y = "Party Registration",
    color = "Top Issue Chosen"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
    axis.title.x = element_text(face = "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.margin = margin(t = 10, r = 30, b = 10, l = 10),
    plot.margin = margin(20, 40, 40, 20)
  ) +
  annotate(
    "text",
    x = max(summary_df$Avg_Favorability) - 0.15,
    y = 4.1,
    label = "Bubble percentages indicate the share of respondents\nwithin each party who selected all three economic populist messages.",
    hjust = 0,
    size = 3.5,
    fontface = "italic"
  )

library(ggplot2)

# Manually define your data
summary_df <- data.frame(
  Party.registration = c("Republican", "Democratic", "Independent", "None"),
  Avg_Favorability = c(3.98, 3.88, 3.76, 4.15),
  Most_Picked_Issue = c("Taxes", "Taxes", "Taxes", "Agriculture"),
  Proportion_Selected = c(0.61, 0.79, 0.71, 0.94),
  Manual_Bubble_Size = c(20, 50, 40, 70),  # Tweak as needed
  Label = c("61%", "79%", "71%", "94%")
)

# Set y-axis party order
summary_df$Party.registration <- factor(
  summary_df$Party.registration,
  levels = c("Republican", "Democratic", "Independent", "None")
)

# Plot
ggplot(summary_df, aes(
  x = Avg_Favorability,
  y = Party.registration,
  color = Most_Picked_Issue,
  size = Manual_Bubble_Size
)) +
  geom_point(alpha = 0.7) +
  geom_text(
    aes(label = Label),
    color = "black",
    size = 3.5,
    fontface = "plain"
  ) +
  scale_color_manual(
    values = c("Agriculture" = "#FDBF6F", "Taxes" = "#A6CEE3"),
    name = "Top Issue Chosen",
    guide = guide_legend(override.aes = list(size = 6))
  ) +
  scale_size_identity(guide = "none") +
  labs(
    title = "Favorability and Selection of Economic Populist Messages by Party",
    x = expression(bold("Average Favorability (1–5)")),
    y = "Party Registration"
  ) +
  annotate(
    "text",
    x = 4.2, y = 0.5,
    label = "Bubble percentages indicate the share of respondents within each party\nwho selected all three economic populist messages.",
    hjust = 1, vjust = 0,
    size = 3.2, fontface = "italic", color = "gray20"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    axis.title.x = element_text(face = "bold"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.margin = margin(t = 10, r = 20, b = 10, l = 10)
  )

library(dplyr)

# Calculate average favorability by party for both populist and non-populist messages
favorability_summary <- Merged_Data_Cleaned %>%
  group_by(Party.registration) %>%
  summarise(
    Avg_Populist_Favorability = round(
      rowMeans(cbind(Education..1.favorability, Taxes..1.favorability, Ag.2.favorability), na.rm = TRUE),
      2
    ),
    Avg_NonPopulist_Favorability = round(
      rowMeans(cbind(Education..2.favorability, Taxes..2.favorability, Ag.1.favorability), na.rm = TRUE),
      2
    )

# View the table
print(favorability_summary)

library(dplyr)

favorability_summary <- Merged_Data_Cleaned %>%
  group_by(Party.registration) %>%
  summarise(
    Avg_Populist_Favorability = round(mean(
      rowMeans(cbind(Education..1.favorability, Taxes..1.favorability, Ag.2.favorability), na.rm = TRUE)
    ), 2),
    
    Avg_NonPopulist_Favorability = round(mean(
      rowMeans(cbind(Education..2.favorability, Taxes..2.favorability, Ag.1.favorability), na.rm = TRUE)
    ), 2)
  )

print(favorability_summary)

library(dplyr)

# Make sure your dataset is named Merged_Data_Cleaned and includes these variables:
# Education..1.favorability (populist)
# Education..2.favorability (non-populist)
# Taxes..1.favorability (populist)
# Taxes..2.favorability (non-populist)
# Ag.2.favorability (populist)
# Ag.1.favorability (non-populist)

favorability_summary <- Merged_Data_Cleaned %>%
  group_by(Party.registration) %>%
  summarise(
    Education_Populist = round(mean(Education..1.favorability, na.rm = TRUE), 2),
    Education_NonPopulist = round(mean(Education..2.favorability, na.rm = TRUE), 2),
    Taxes_Populist = round(mean(Taxes..1.favorability, na.rm = TRUE), 2),
    Taxes_NonPopulist = round(mean(Taxes..2.favorability, na.rm = TRUE), 2),
    Agriculture_Populist = round(mean(Ag.2.favorability, na.rm = TRUE), 2),
    Agriculture_NonPopulist = round(mean(Ag.1.favorability, na.rm = TRUE), 2)
  )

print(favorability_summary)
print(favorability_summary, width = Inf)

library(dplyr)

summary_stats <- Merged_Data_Cleaned %>%
  group_by(Party.registration) %>%
  summarise(
    Education_Populist_Mean = mean(Education..1.favorability, na.rm = TRUE),
    Education_Populist_SD = sd(Education..1.favorability, na.rm = TRUE),
    Education_Populist_n = sum(!is.na(Education..1.favorability)),
    
    Education_NonPopulist_Mean = mean(Education..2.favorability, na.rm = TRUE),
    Education_NonPopulist_SD = sd(Education..2.favorability, na.rm = TRUE),
    Education_NonPopulist_n = sum(!is.na(Education..2.favorability)),
    
    Taxes_Populist_Mean = mean(Taxes..1.favorability, na.rm = TRUE),
    Taxes_Populist_SD = sd(Taxes..1.favorability, na.rm = TRUE),
    Taxes_Populist_n = sum(!is.na(Taxes..1.favorability)),
    
    Taxes_NonPopulist_Mean = mean(Taxes..2.favorability, na.rm = TRUE),
    Taxes_NonPopulist_SD = sd(Taxes..2.favorability, na.rm = TRUE),
    Taxes_NonPopulist_n = sum(!is.na(Taxes..2.favorability)),
    
    Ag_Populist_Mean = mean(Ag.2.favorability, na.rm = TRUE),
    Ag_Populist_SD = sd(Ag.2.favorability, na.rm = TRUE),
    Ag_Populist_n = sum(!is.na(Ag.2.favorability)),
    
    Ag_NonPopulist_Mean = mean(Ag.1.favorability, na.rm = TRUE),
    Ag_NonPopulist_SD = sd(Ag.1.favorability, na.rm = TRUE),
    Ag_NonPopulist_n = sum(!is.na(Ag.1.favorability))
  )
library(dplyr)

# Replace with your actual dataset name
df_summary <- Merged_Data_Cleaned %>%
  mutate(Education.Group = factor(Collapsed_Education_Level)) %>%
  group_by(Political.scale, Education.Group) %>%
  summarise(
    Percent_Mechanic = mean(Mechanic == 1, na.rm = TRUE) * 100,
    n_Mechanic = sum(!is.na(Mechanic)),
    Percent_Teacher = mean(Teacher == 1, na.rm = TRUE) * 100,
    n_Teacher = sum(!is.na(Teacher)),
    Percent_Veteran = mean(Veteran == 1, na.rm = TRUE) * 100,
    n_Veteran = sum(!is.na(Veteran)),
    .groups = "drop"
  )

# View the corrected breakdown
print(df_summary, n = Inf, width = Inf)

library(dplyr)

# Replace 'Education.level' with your actual education variable name if needed
df_summary <- Merged_Data_Cleaned %>%
  mutate(Education.Group = factor(Education.level)) %>%
  group_by(Political.scale, Education.Group) %>%
  summarise(
    Percent_Mechanic = mean(Mechanic == 1, na.rm = TRUE) * 100,
    n_Mechanic = sum(!is.na(Mechanic)),
    Percent_Teacher = mean(Teacher == 1, na.rm = TRUE) * 100,
    n_Teacher = sum(!is.na(Teacher)),
    Percent_Veteran = mean(Veteran == 1, na.rm = TRUE) * 100,
    n_Veteran = sum(!is.na(Veteran)),
    .groups = "drop"
  )library(dplyr)

# Replace 'Education.level' with your actual education variable name if needed
df_summary <- Merged_Data_Cleaned %>%
  mutate(Education.Group = factor(Education.level)) %>%
  group_by(Political.scale, Education.Group) %>%
  summarise(
    Percent_Mechanic = mean(Mechanic == 1, na.rm = TRUE) * 100,
    n_Mechanic = sum(!is.na(Mechanic)),
    Percent_Teacher = mean(Teacher == 1, na.rm = TRUE) * 100,
    n_Teacher = sum(!is.na(Teacher)),
    Percent_Veteran = mean(Veteran == 1, na.rm = TRUE) * 100,
    n_Veteran = sum(!is.na(Veteran)),
    .groups = "drop"
  )

print(df_summary, n = Inf, width = Inf)library(dplyr)

# Replace 'Education.level' with your actual education variable name if needed
df_summary <- Merged_Data_Cleaned %>%
  mutate(Education.Group = factor(Education.level)) %>%
  group_by(Political.scale, Education.Group) %>%
  summarise(
    Percent_Mechanic = mean(Mechanic == 1, na.rm = TRUE) * 100,
    n_Mechanic = sum(!is.na(Mechanic)),
    Percent_Teacher = mean(Teacher == 1, na.rm = TRUE) * 100,
    n_Teacher = sum(!is.na(Teacher)),
    Percent_Veteran = mean(Veteran == 1, na.rm = TRUE) * 100,
    n_Veteran = sum(!is.na(Veteran)),
    .groups = "drop"
  )

print(df_summary, n = Inf, width = Inf)

Merged_Data_Cleaned <- Merged_Data_Cleaned %>%
  mutate(
    Education.Group = case_when(
      Education.level %in% c("High school", "Less than high school") ~ "No College",
      Education.level %in% c("Some college", "2-year degree") ~ "Some College",
      Education.level %in% c("4-year degree", "Graduate degree") ~ "Graduate Degree",
      TRUE ~ NA_character_
    ),
    Education.Group = factor(Education.Group, levels = c("No College", "Some College", "Graduate Degree"))
  )Merged_Data_Cleaned <- Merged_Data_Cleaned %>%
  mutate(
    Education.Group = case_when(
      Education.level %in% c("High school", "Less than high school") ~ "No College",
      Education.level %in% c("Some college", "2-year degree") ~ "Some College",
      Education.level %in% c("4-year degree", "Graduate degree") ~ "Graduate Degree",
      TRUE ~ NA_character_
    ),
    Education.Group = factor(Education.Group, levels = c("No College", "Some College", "Graduate Degree"))
  )Merged_Data_Cleaned <- Merged_Data_Cleaned %>%
  mutate(
    Education.Group = case_when(
      Education.level %in% c("High school", "Less than high school") ~ "No College",
      Education.level %in% c("Some college", "2-year degree") ~ "Some College",
      Education.level %in% c("4-year degree", "Graduate degree") ~ "Graduate Degree",
      TRUE ~ NA_character_
    ),
    Education.Group = factor(Education.Group, levels = c("No College", "Some College", "Graduate Degree"))
  )

print(df_summary, n = Inf, width = Inf)

Merged_Data_Cleaned <- Merged_Data_Cleaned %>%
  mutate(
    Education.Group = case_when(
      Education.level %in% c("High school graduate (high school diploma or equivalent including GED)", 
                             "Less than high school degree") ~ "No College",
      Education.level %in% c("Some college but no degree", 
                             "Associate degree in college (2-year)") ~ "Some College",
      Education.level %in% c("Bachelor's degree in college (4-year)", 
                             "Master's degree", 
                             "Professional degree (JD, MD)", 
                             "Doctoral degree") ~ "Graduate Degree",
      TRUE ~ NA_character_
    ),
    Education.Group = factor(Education.Group, levels = c("No College", "Some College", "Graduate Degree"))
  )
names(Merged_Data_Cleaned)

library(dplyr)

Merged_Data_Cleaned <- Merged_Data_Cleaned %>%
  mutate(
    Education.Group = case_when(
      Education.Level %in% c("Less than high school degree", 
                             "High school graduate (high school diploma or equivalent including GED)") ~ "No College",
      Education.Level %in% c("Some college but no degree", 
                             "Associate degree in college (2-year)", 
                             "Bachelor's degree in college (4-year)") ~ "Some College",
      Education.Level %in% c("Master's degree", 
                             "Professional degree (JD, MD)", 
                             "Doctoral degree") ~ "Graduate Degree",
      TRUE ~ NA_character_
    ),
    # Reorder factor levels for clarity in plots
    Education.Group = factor(Education.Group, levels = c("No College", "Some College", "Graduate Degree"))
  )

library(dplyr)
library(tidyr)

# Ensure Education.Group is a factor with correct levels
Merged_Data_Cleaned <- Merged_Data_Cleaned %>%
  mutate(
    Education.Group = case_when(
      Education.Level %in% c("Less than high school degree", "High school graduate (high school diploma or equivalent including GED)") ~ "No College",
      Education.Level %in% c("Some college but no degree", "Associate degree in college (2-year)") ~ "Some College",
      Education.Level %in% c("Bachelor's degree in college (4-year)", "Master's degree", "Doctoral degree", "Professional degree (JD, MD)") ~ "Graduate Degree",
      TRUE ~ NA_character_
    ),
    Education.Group = factor(Education.Group, levels = c("No College", "Some College", "Graduate Degree"))
  )

# Function to get counts and proportions
get_candidate_counts <- function(candidate_var) {
  Merged_Data_Cleaned %>%
    filter(!is.na(Education.Group), !is.na(!!sym(candidate_var)), !is.na(Political.scale)) %>%
    group_by(Political.scale, Education.Group) %>%
    summarise(
      n_selected = sum(!!sym(candidate_var) == 1, na.rm = TRUE),
      n_total = n(),
      percent_selected = round(100 * n_selected / n_total, 1),
      .groups = "drop"
    ) %>%
    rename_with(~ paste0(candidate_var, "_", .), c("n_selected", "n_total", "percent_selected"))
}

# Get data for each candidate
mech_counts <- get_candidate_counts("Mechanic")
teach_counts <- get_candidate_counts("Teacher")
vet_counts <- get_candidate_counts("Veteran")

# Join all three together
full_candidate_table <- reduce(
  list(mech_counts, teach_counts, vet_counts),
  full_join,
  by = c("Political.scale", "Education.Group")
)

# View result
print(full_candidate_table, n = Inf)

library(purrr)
library(dplyr)
library(tidyr)
library(purrr)  # <-- this is what you're missing

# Step 1: Create simplified education group
Merged_Data_Cleaned <- Merged_Data_Cleaned %>%
  mutate(
    Education.Group = case_when(
      Education.Level %in% c("Less than high school degree", "High school graduate (high school diploma or equivalent including GED)") ~ "No College",
      Education.Level %in% c("Some college but no degree", "Associate degree in college (2-year)") ~ "Some College",
      Education.Level %in% c("Bachelor's degree in college (4-year)", "Master's degree", "Doctoral degree", "Professional degree (JD, MD)") ~ "Graduate Degree",
      TRUE ~ NA_character_
    ),
    Education.Group = factor(Education.Group, levels = c("No College", "Some College", "Graduate Degree"))
  )

# Step 2: Function to get counts and proportions
get_candidate_counts <- function(candidate_var) {
  Merged_Data_Cleaned %>%
    filter(!is.na(Education.Group), !is.na(!!sym(candidate_var)), !is.na(Political.scale)) %>%
    group_by(Political.scale, Education.Group) %>%
    summarise(
      n_selected = sum(!!sym(candidate_var) == 1, na.rm = TRUE),
      n_total = n(),
      percent_selected = round(100 * n_selected / n_total, 1),
      .groups = "drop"
    ) %>%
    rename_with(~ paste0(candidate_var, "_", .), c("n_selected", "n_total", "percent_selected"))
}

# Step 3: Apply function to each candidate
mech_counts <- get_candidate_counts("Mechanic")
teach_counts <- get_candidate_counts("Teacher")
vet_counts <- get_candidate_counts("Veteran")

# Step 4: Combine into one table
full_candidate_table <- reduce(
  list(mech_counts, teach_counts, vet_counts),
  full_join,
  by = c("Political.scale", "Education.Group")
)

# Step 5: View full results
print(full_candidate_table, n = Inf)

# Frequency count
self_id_counts <- table(Merged_Data_Cleaned$Self.identified.partisanship)

# Percentages
self_id_percent <- prop.table(self_id_counts) * 100

# Rounded results
round(self_id_percent, 1)

# Frequency count
reg_counts <- table(Merged_Data_Cleaned$Party.registration)

# Percentages
reg_percent <- prop.table(reg_counts) * 100

# Rounded results
round(reg_percent, 1)
