#Clearing environment and loading necessary libraries
rm(list = ls())
library(margins)
library(tidyverse)
library(haven)
library(dplyr)
library(lme4)
library(stargazer)

#Some Notes on the Data Set:
#We download British Household Panel Survey: Waves 1-18, 1991-2009 from the UK Data Service (SN 5151)
#Contains data from all waves except wave 1 (wave 1 does not contain person number data and is therefore unable to merge), so waves 2:18. 
#The final cleaned data frame of working age individuals (which the paper explicitly limits all analysis to) contains 184,169 observations and 27,015 unique individuals. For reference, when doing analysis, the paper indicates 189,820 observations and 28,042 unique individuals)
#However, it is important to note that lottery data does not begin until 1998 (wave 7). When I limited my analysis to only waves with lottery data, I only had about half the indicated observations (90,000) 
#cross wave person identifier information is in aegoalt.csv
#look into consolidating loops into one loop for waves 2-18
#add a column that indicates combined variable instead of merging variables

# Initialize lists to store merged data frames and selected relevant column names
mdfs_list <- list()
columns <- c("hid", "pno", "sex", "jbstat", "doby", "windfg", "windfgy", "mastat", "pid", "pidp", "fihhyr", "hlstat", "hlsf1", "hospd", "nkids", "qfedhi", "qfvoc", "region.x")

path <- "/Users/cherynryoo/Desktop/BHPS/data/raw/bhps_w"

#egoalt: sex, hid, pno
#indresp: hid, pno, sex, jbstat, doby, windfgy, windfgy, mastat, pid, pidp, fimn(total income last month), fihhyr (annual labor income)
#hhresp: fihhyr, number of children in household




#Note that wave 1 is missing aindsamp.dta and the ahhresp.dta does not contain apno, just the household number.
# Loop through letters 'b' to 'f' (waves 1991 to 1996)
for (i in 1:18) {
  letter <- letters[i]
  
  #getting the relevant variable name for each wave for the id variables
  v1 <- paste0(letter, "hid")
  v2 <- paste0(letter, "pno")
  
  # Read individual data files for indsamp, indresp, and hhresp
  # Read individual data files for indsamp, indresp, and hhresp
  df_indresp <- read_csv(file.path(paste0(path, i, "/", letter, "indresp.csv")))
  df_hhresp <- read_csv(file.path(paste0(path, i, "/", letter, "hhresp.csv")))
  
  
  # Perform inner joins to merge data frames
  merged_df <- inner_join(df_indresp, df_hhresp, by = v1)
  
  # Add a column indicating wave number
  merged_df <- merged_df %>%
    mutate(wave = i)  
  
  # Find the common columns to select
  available_columns <- intersect(paste0(letter, columns), colnames(merged_df))
  
  final_df <- NULL
  
  if (length(available_columns) > 0) {
    final_df <- select(merged_df, all_of(available_columns), pid, pidp, wave)
  } else {
    message(paste("Columns not found in data frames for letter:", letter))
    next
  }
  
  # Rename columns to remove the prefix letters and keep variable names consistent
  colnames(final_df) <- gsub(paste0("^", letter), "", colnames(final_df))
  
  # Store the final dataframe in mdfs_list
  mdfs_list[[paste0(letter, "_mdfs")]] <- final_df
}
finaldf <- bind_rows(mdfs_list)

# Merge the extra columns to handle NA values
finaldf <- finaldf %>%
  mutate(
    hlstat = coalesce(hlstat, hlsf1),
    pid = coalesce(pid, id),
    pidp = coalesce(pidp, idp),
    region = region.x
  ) %>%
  select(pid, pidp, hid, pno, sex, jbstat, doby, windfg, windfgy, mastat, fihhyr, hlstat, hospd, nkids, qfedhi, qfvoc, wave, region)
View(finaldf)

#cleaning up environment so there is just the finaldf and df
alldf <- ls()
keep <- setdiff(alldf, "finaldf")
rm(list = keep)
df <- finaldf
View(df)

#Continue cleaning data:
df <- df %>%
  mutate(fihhyr = as.numeric(fihhyr),
         mastat = as.numeric(mastat))

# Recode specific values to NA
df <- df %>%
  mutate(fihhyr = recode(fihhyr, `-8` = NA_real_, `-7` = NA_real_, `-2` = NA_real_, `-1` = NA_real_, `-9` = NA_real_))

#Recoding Variables
df <- df %>%
  mutate(mastat = recode(mastat, `-8` = NA_real_, `-7` = NA_real_, `-2` = NA_real_, `-1` = NA_real_, `-9` = NA_real_),
         mastat = recode(mastat,
                         `0` = "Child under 16",
                         `1` = "Married",
                         `2` = "Living as couple",
                         `3` = "Widowed",
                         `4` = "Divorced",
                         `5` = "Separated",
                         `6` = "Never married"),
         .default = NA_real_)

# First, recode the numeric values to NA where applicable
df <- df %>%
  mutate(hlstat = recode(as.numeric(hlstat), 
                         `-8` = NA_real_, `-7` = NA_real_, `-2` = NA_real_, `-1` = NA_real_, `-9` = NA_real_),
         hospd = recode(as.numeric(hospd), 
                        `-8` = NA_real_, `-7` = NA_real_, `-2` = NA_real_, `-1` = NA_real_, `-9` = NA_real_),
         # hsowrp = recode(as.numeric(hsowrp), 
         #                 `-8` = NA_real_, `-7` = NA_real_, `-2` = NA_real_, `-1` = NA_real_, `-9` = NA_real_, 
         #                 `2` = 0),
         qfedhi = recode(as.numeric(qfedhi), 
                         `-8` = NA_real_, `-7` = NA_real_, `-2` = NA_real_, `-1` = NA_real_, `-9` = NA_real_),
         qfvoc = recode(as.numeric(qfvoc), 
                        `-8` = NA_real_, `-7` = NA_real_, `-2` = NA_real_, `-1` = NA_real_, `-9` = NA_real_),
         nkids = recode(as.numeric(nkids),
                        `-8` = NA_real_, `-7` = NA_real_, `-2` = NA_real_, `-1` = NA_real_, `-9` = NA_real_),
         sex = recode(as.numeric(sex),
                      `-8` = NA_real_, `-7` = NA_real_, `-2` = NA_real_, `-1` = NA_real_, `-9` = NA_real_),
         region = recode(as.numeric(region),
                      `-8` = NA_real_, `-7` = NA_real_, `-2` = NA_real_, `-1` = NA_real_, `-9` = NA_real_))


# Then, convert to factors with the appropriate labels

df <- df %>%
  mutate(hlstat = factor(hlstat, levels = c(1, 2, 3, 4, 5), 
                         labels = c("Excellent", "Good", "Fair", "Poor", "Very Poor")),
         qfedhi = factor(qfedhi, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), 
                         labels = c("Higher Degree", "First Degree", "Teaching QF", "Other Higher QF", 
                                    "Nursing QF", "GCE A Levels", "GCE O Levels or Equiv", 
                                    "Commercial QF, No O Levels", "CSE Grade 2-5,Scot Grade 4-5", 
                                    "Apprenticeship", "Other QF", "No QF", "Still At School No QF")),
         region = factor(region, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19),
                         labels = c(
                           "Inner London", "Outer London", "R. of South East", "South West",
                           "East Anglia", "East Midlands", "West Midlands Conurbation", 
                           "R. of West Midlands", "Greater Manchester", "Merseyside", 
                           "R. of North West", "South Yorkshire", "West Yorkshire", 
                           "R. of Yorks & Humberside", "Tyne & Wear", "R. of North", 
                           "Wales", "Scotland", "Northern Ireland"
                         )))
df <- df %>%
  select(-.default)
#Now adding relevant columns for Fleche Paper
#filtering for working age individuals
df <- df %>%
  mutate(year = 1991 + wave,
         age = year - doby) %>%
  filter(age >= 16 & age <= 65)


#Now adjust windfgy using CPI using data from Office of National Statistics
#This is the correct CPI DATA: https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/d7bt/mm23
cpi_data_2015_base <- tibble(
  year = c(1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2015),
  cpi = c(71.2, 72.1, 72.7, 73.6, 74.5, 75.5, 76.5, 78.1, 79.9, 81.8, 84.7, 86.6, 100)  
)

# Create CPI data with 2000 as the base year
cpi_data_2000_base <- cpi_data_2015_base %>%
  mutate(cpi_2000_base = cpi / cpi[year == 2000] * 100)

wave_year_mapping <- tibble(
  wave = 7:18,
  year = 1998:2009
)

# Merge finaldf with wave_year_mapping to get the corresponding year for each wave
df <- df %>%
  left_join(wave_year_mapping, by = c("wave", "year"))

# Merge finaldf with cpi_data_2000_base to get the corresponding CPI values
df <- df %>%
  left_join(cpi_data_2000_base, by = "year")

df <- df %>%
  mutate(
    windfgy_adjusted = ifelse(windfgy > 0, windfgy, NA),
    windfgy_adjusted = windfgy_adjusted / cpi_2000_base * 100
  )


#Adding a dummy column for top 25% wins, selfemployed, female, and winsize
# t25 <- quantile(df$windfgy_adjusted, probs = 0.75, na.rm = TRUE)
t25 <- quantile(df$windfgy_adjusted, probs = 0.75, na.rm = TRUE)
df <- df %>%
  mutate(t25_win = ifelse(windfgy_adjusted >= t25, 1, 0),
         selfemployed = ifelse(jbstat == 1, 1, 0),
         female = ifelse(sex == 2, 1, 0)) %>%
  mutate(winsize = case_when(
    windfgy_adjusted < 100 ~ "small win",
    windfgy_adjusted >= 100 & windfgy_adjusted < 500 ~ "medium win",
    windfgy_adjusted >= 500 ~ "large win"
  ))

#Creating variable for log winnings:
df <- df %>%
  mutate(log_windfgy = log(windfgy_adjusted))

#Calculating Share of Self Employed in Working Age Sample (Paper finds 8.1%, I find 8.18%)
selfemployed <- df %>%
  filter(jbstat == 1)

workingage <- df %>%
  filter(!is.na(jbstat)) %>%
  filter(jbstat > 0)

selfemployed_count <- workingage %>%
  count(jbstat == 1) %>%
  mutate(percentage = n / sum(n) * 100)
print(selfemployed_count)

#Calculating percentage of individuals who report at least one lottery win: (paper reports 36%, I find 25.34%)


filter_df <- df %>%
  filter(windfg >= 0)

individuals_with_win <- n_distinct(winners$pidp)

total_individuals <- n_distinct(filter_df$pidp)

percentage_with_win <- (individuals_with_win / total_individuals) * 100
cat("Percentage of people that report at least one lottery win across waves:", percentage_with_win, "%\n")

#Calculating the percentage of self employed people who report at least one lottery win (17.56% compared to paper's 36%)
SE_with_win <- selfemployed %>%
  group_by(pidp) %>%
  summarize(any_win = any(windfg == 1)) %>%
  ungroup()

t_individual <- n_distinct(selfemployed$pidp)
individuals_with_win_count <- SE_with_win %>%
  filter(any_win) %>%
  nrow()

percentage_with_win <- (individuals_with_win_count / t_individual) * 100
print(percentage_with_win)



#Calculating percentiles of lottery wins: Matches With Paper closely 
percentiles <- quantile(winners$windfgy_adjusted, probs = seq(0, 1, 0.25), na.rm = TRUE)
percentiles <- format(percentiles, scientific = FALSE)
print(percentiles)

# Calculate the average win values for top 25th percentile and bottom 75th percentile: NOTE, these are quite consistent with the paper
top_25th_avg <- winners %>%
  filter(t25_win == 1) %>%
  summarise(avg_win = mean(windfgy_adjusted, na.rm = TRUE))

bottom_75th_avg <- winners %>%
  filter(t25_win == 0) %>%
  summarise(avg_win = mean(windfgy_adjusted, na.rm = TRUE))

top_25th_avg
bottom_75th_avg

##Calculating the percentage of small, medium, and big wins (quite consistent with paper)
win_size_counts <- winners %>%
  count(winsize) %>%
  mutate(percentage = n / sum(n) * 100)
print(win_size_counts)  

#Calculating mean wins for self employed and non-self employed (very close to paper; paper finds 595.50 average win for self employed and 192.79 for non self employed)
result <- winners %>%
  mutate(self_employed = ifelse(is.na(selfemployed), 0, selfemployed)) %>%
  group_by(self_employed) %>%
  summarize(Average_win = mean(windfgy_adjusted, na.rm = TRUE)) %>%
  ungroup()
print(result)


#Calculating mean wins for self employed and non-self employed by gender (paper finds 211.99, 696.96, 168.91, 197.57, respectively) 
mean_wins <- winners %>%
  mutate(self_employed = ifelse(is.na(selfemployed), 0, selfemployed)) %>%
  group_by(female, selfemployed) %>%
  summarize(Average_win = mean(windfgy_adjusted, na.rm = TRUE)) %>%
  ungroup()
print(mean_wins)




#Figure1 1: Log lottery wins (self employed vs non-self employed by gender)
#Log Lottery Gains for Self Employed vs Non-self Employed Females
female_SE <- df %>%
  filter(female == 1 & selfemployed == 1 & !is.na(log_windfgy))
female_not_SE <- df %>%
  filter(female == 1 & selfemployed == 0 & !is.na(log_windfgy))

h1 = hist(female_SE$log_windfgy, breaks = 25)
h1$density = h1$counts/sum(h1$counts)*100

h2 = hist(female_not_SE$log_windfgy, breaks = 25)
h2$density = h2$counts/sum(h2$counts) * 100

xrange <- range(h1$breaks, h2$breaks)
yrange <- range(0, h1$density, h2$density)

# Plot the first histogram
plot(h1, freq = FALSE, col = rgb(1, 0, 0, 0.5), xlim = xrange, ylim = yrange,
     main = "Histogram of Log Lottery Gains for Self-Employed and Non-Self-Employed Females", 
     xlab = "Log Lottery Gains", ylab = "Percentage", border = "black")

# Add the second histogram
plot(h2, freq = FALSE, col = rgb(0, 0, 1, 0.5), add = TRUE, border = "black")

# Add a legend
legend("topright", legend = c("Self-Employed Females", "Non-Self-Employed Females"),
       fill = c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5)))


#Log Lottery Gains for Self Employed vs Non-self Employed Males
male_SE <- df %>%
  filter(female == 0 & selfemployed == 1 & !is.na(log_windfgy))
male_not_SE <- df %>%
  filter(female == 0 & selfemployed == 0 & !is.na(log_windfgy))

h3 = hist(male_SE$log_windfgy, breaks = 30)
h3$density = h3$counts/sum(h3$counts)*100

h4 = hist(male_not_SE$log_windfgy, breaks = 30)
h4$density = h4$counts/sum(h4$counts) * 100

xrange <- range(h3$breaks, h4$breaks)
yrange <- range(0, h3$density, h4$density)

# Plot the first histogram
plot(h3, freq = FALSE, col = rgb(1, 0, 0, 0.5), xlim = xrange, ylim = yrange,
     main = "Histogram of Log Lottery Gains for Self-Employed and Non-Self-Employed Males", 
     xlab = "Log Lottery Gains", ylab = "Percentage", border = "black")

# Add the second histogram
plot(h4, freq = FALSE, col = rgb(0, 0, 1, 0.5), add = TRUE, border = "black")

# Add a legend
legend("topright", legend = c("Self-Employed Males", "Non-Self-Employed Males"),
       fill = c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5)))


#Table 1:
df <- df %>%
  mutate(fihhyr = recode(as.numeric(fihhyr), `0` = NA_real_),
         log_fihhyr = log(fihhyr)) %>%
  filter(!is.infinite(log_fihhyr) & !is.infinite(age) & !is.infinite(selfemployed) & !is.infinite(female) & !is.infinite(t25_win))

#Table 1:
#creating lag variables
df <- df %>%
  group_by(pidp) %>%
  arrange(wave) %>%
  mutate(t25_win_t_minus3 = ifelse(row_number() >= 4, lag(t25_win, 3), NA),
         t25_win_t_minus2 = ifelse(row_number() >= 3, lag(t25_win, 2), NA),
         t25_win_t_minus1 = ifelse(row_number() >= 2, lag(t25_win, 1), NA),
         t25_win_t_plus1 = ifelse(row_number() <= n() - 1, lead(t25_win, 1), NA),
         t25_win_t_plus2 = ifelse(row_number() <= n() - 2, lead(t25_win, 2), NA),
         t25_win_t_plus3 = ifelse(row_number() <= n() - 3, lead(t25_win, 3), NA)) %>%
  ungroup()


model1 <- glm(selfemployed ~ female + age + I(age^2) + log_fihhyr + mastat + nkids + hlstat + region, data = df, family = binomial())
model2 <- glm(selfemployed ~ female + t25_win + age + I(age^2) + log_fihhyr + mastat + nkids + hlstat + region, data = df, family = binomial())
model3 <- glm(selfemployed ~ female * t25_win + age + I(age^2) + log_fihhyr + mastat + nkids + hlstat + region, data = df, family = binomial())
model4 <- glm(selfemployed ~ female + t25_win_t_minus1 + age + I(age^2) + log_fihhyr + mastat + nkids + hlstat + region, data = df, family = binomial())
model5 <- glm(selfemployed ~ female * t25_win_t_minus1 + age + I(age^2) + log_fihhyr + mastat + nkids + hlstat + region, data = df, family = binomial())



#Now calculating the number of unique individuals:
# Calculate the number of unique individuals for each model
unique_individuals_model1 <- length(unique(df$pidp[!is.na(df$selfemployed) & 
                                                     !is.na(df$female) & 
                                                     !is.na(df$age) & 
                                                     !is.na(df$log_fihhyr) & 
                                                     !is.na(df$mastat) & 
                                                     !is.na(df$nkids) & 
                                                     !is.na(df$hlstat) &
                                                     !is.na(df$region)]))

unique_individuals_model2 <- length(unique(df$pidp[!is.na(df$selfemployed) & 
                                                     !is.na(df$female) & 
                                                     !is.na(df$t25_win) & 
                                                     !is.na(df$age) & 
                                                     !is.na(df$log_fihhyr) & 
                                                     !is.na(df$mastat) & 
                                                     !is.na(df$nkids) & 
                                                     !is.na(df$hlstat) &
                                                     !is.na(df$region)]))

unique_individuals_model3 <- length(unique(df$pidp[!is.na(df$selfemployed) & 
                                                     !is.na(df$female) & 
                                                     !is.na(df$t25_win) & 
                                                     !is.na(df$age) & 
                                                     !is.na(df$log_fihhyr) & 
                                                     !is.na(df$mastat) & 
                                                     !is.na(df$nkids) & 
                                                     !is.na(df$hlstat) &
                                                     !is.na(df$region)]))

unique_individuals_model4 <- length(unique(df$pidp[!is.na(df$selfemployed) & 
                                                     !is.na(df$female) & 
                                                     !is.na(df$t25_win_t_minus1) & 
                                                     !is.na(df$age) & 
                                                     !is.na(df$log_fihhyr) & 
                                                     !is.na(df$mastat) & 
                                                     !is.na(df$nkids) & 
                                                     !is.na(df$hlstat) &
                                                     !is.na(df$region)]))

unique_individuals_model5 <- length(unique(df$pidp[!is.na(df$selfemployed) & 
                                                     !is.na(df$female) & 
                                                     !is.na(df$t25_win_t_minus1) & 
                                                     !is.na(df$age) & 
                                                     !is.na(df$log_fihhyr) & 
                                                     !is.na(df$mastat) & 
                                                     !is.na(df$nkids) & 
                                                     !is.na(df$hlstat) &
                                                     !is.na(df$region)]))



# Summarize the models using stargazer with correct labels. NOTE: MODEL1, MODEL2, MODEL3 correspond to (1), (2), (4), in table 1
stargazer(model1, model2, model3, model4, model5, type = "text", title = "Logistic Regression Results", 
          dep.var.labels = c("Self-employed Probability"),
          omit.stat = c("f", "ser"), 
          omit = c("age", "I(age^2)", "log_fihhyr", "mastat", "nkids", "hlstat", "region"),
          add.lines = list(c("Unique individuals", 
                             unique_individuals_model1, 
                             unique_individuals_model2, 
                             unique_individuals_model3, 
                             unique_individuals_model4, 
                             unique_individuals_model5)))

# Calculate Marginal Effects at the Mean
mem1 <- summary(margins(model1))
mem2 <- summary(margins(model2))
mem3 <- summary(margins(model3))
mem4 <- summary(margins(model4))
mem5 <- summary(margins(model5))

# Extract coefficients and standard errors for the table
extract_mem <- function(mem) {
  data.frame(
    Variable = mem$factor,
    MEM = mem$AME,
    Std_Error = mem$SE
  )
}
mem1_table <- extract_mem(mem1)
mem2_table <- extract_mem(mem2)
mem3_table <- extract_mem(mem3)
mem4_table <- extract_mem(mem4)
mem5_table <- extract_mem(mem5)

#Figure 2:
# Create lead and lag variables conditionally
df <- df %>%
  group_by(pidp) %>%
  arrange(wave) %>%
  mutate(SE_t_minus3 = ifelse(row_number() >= 4, lag(selfemployed, 3), NA),
         SE_t_minus2 = ifelse(row_number() >= 3, lag(selfemployed, 2), NA),
         SE_t_minus1 = ifelse(row_number() >= 2, lag(selfemployed, 1), NA),
         SE_t_plus1 = ifelse(row_number() <= n() - 1, lead(selfemployed, 1), NA),
         SE_t_plus2 = ifelse(row_number() <= n() - 2, lead(selfemployed, 2), NA),
         SE_t_plus3 = ifelse(row_number() <= n() - 3, lead(selfemployed, 3), NA)) %>%
  ungroup()
top_25th_percentile = quantile(df$windfgy_adjusted, 0.75, na.rm = TRUE)


df <- df %>%
  mutate(
    t25_win = ifelse(windfgy_adjusted >= top_25th_percentile, 1, 0)
  )
  
#Running Regression for each period
model_t_minus3 <- lm(SE_t_minus3 ~ t25_win + (1 | pidp) + age + I(age^2) + log_fihhyr + mastat + nkids + hlstat + region, data = df)
model_t_minus2 <- lm(SE_t_minus2 ~ t25_win + (1 | pidp) + age + I(age^2) + log_fihhyr + mastat + nkids + hlstat + region, data = df)
model_t_minus1 <- lm(SE_t_minus1 ~ t25_win + (1 | pidp) + age + I(age^2) + log_fihhyr + mastat + nkids + hlstat + region, data = df)
model_t <- lm(selfemployed ~ t25_win + (1 | pidp) + age + I(age^2) + log_fihhyr + mastat + nkids + hlstat + region, data = df)
model_t_plus1 <- lm(SE_t_plus1 ~ t25_win + (1 | pidp) + age + I(age^2) + log_fihhyr + mastat + nkids + hlstat + region, data = df)
model_t_plus2 <- lm(SE_t_plus2 ~ t25_win + (1 | pidp) + age + I(age^2) + log_fihhyr + mastat + nkids + hlstat + region, data = df)
model_t_plus3 <- lm(SE_t_plus3 ~ t25_win + (1 | pidp) + age + I(age^2) + log_fihhyr + mastat + nkids + hlstat + region, data = df)



#Extracting Coefficients:
extract_coefs <- function(model) {
  coef_summary <- summary(model)$coefficients
  return(data.frame(Estimate = coef_summary[2, 1], Std_Error = coef_summary[2, 2]))
}

coef_t_minus3 <- extract_coefs(model_t_minus3)
coef_t_minus2 <- extract_coefs(model_t_minus2)
coef_t_minus1 <- extract_coefs(model_t_minus1)
coef_t <- extract_coefs(model_t)
coef_t_plus1 <- extract_coefs(model_t_plus1)
coef_t_plus2 <- extract_coefs(model_t_plus2)
coef_t_plus3 <- extract_coefs(model_t_plus3)


#Creating Table and Plotting Results: Note that these values are currently inconsistent with findings of the paper
# Create the summary table
summary_table <- data.frame(
  Time = factor(c("t-3", "t-2", "t-1", "t", "t+1", "t+2", "t+3"), 
                levels = c("t-3", "t-2", "t-1", "t", "t+1", "t+2", "t+3")),
  Estimate = c(coef_t_minus3$Estimate, coef_t_minus2$Estimate, coef_t_minus1$Estimate,
               coef_t$Estimate, coef_t_plus1$Estimate, coef_t_plus2$Estimate, coef_t_plus3$Estimate),
  Std_Error = c(coef_t_minus3$Std_Error, coef_t_minus2$Std_Error, coef_t_minus1$Std_Error,
                coef_t$Std_Error, coef_t_plus1$Std_Error, coef_t_plus2$Std_Error, coef_t_plus3$Std_Error))

print(summary_table)

ggplot(summary_table, aes(x = Time, y = Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = Estimate - 2 * Std_Error, ymax = Estimate + 2 * Std_Error), width = 0.2) +
  geom_line(aes(group = 1)) +
  labs(title = "Predicted Probability of Self-Employment",
       y = "Predicted Probability of Self-Employment",
       x = "") +
  theme_minimal()

#Checking Number of Observations Against The Results of the Paper. The number of observations is quite close to what is outlined in the paper. Still having some issues with the coefficients.
t3_obs <- sum(!is.na(df$t25_win_t_minus3))
t2_obs <- sum(!is.na(df$t25_win_t_minus2))
t1_obs <- sum(!is.na(df$t25_win_t_minus1))
t_obs <- sum(!is.na(df$t25_win))


#Table 2: Transitions into self-employment and lottery wins in t-1
table2df <- df %>%
  filter(SE_t_minus1 == 0 & !is.na(SE_t_minus1))
# Define the models
m1 <- glm(selfemployed ~ female + t25_win_t_minus1 + age + I(age^2) + log_fihhyr + mastat + nkids + hlstat + wave + region, 
          data = table2df, family = binomial())
m2 <- glm(selfemployed ~ female * t25_win_t_minus1 + age + I(age^2) + log_fihhyr + mastat + nkids + hlstat + wave + region, data = table2df, family = binomial())

# Calculate the number of unique individuals used in each model
unique_individuals_m1 <- length(unique(table2df$pidp[!is.na(table2df$selfemployed) & 
                                                       !is.na(table2df$female) & 
                                                       !is.na(table2df$t25_win_t_minus1) &
                                                       !is.na(table2df$age) & 
                                                       !is.na(table2df$log_fihhyr) & 
                                                       !is.na(table2df$mastat) & 
                                                       !is.na(table2df$nkids) & 
                                                       !is.na(table2df$hlstat)]))

unique_individuals_m2 <- length(unique(table2df$pidp[!is.na(table2df$selfemployed) & 
                                                       !is.na(table2df$female) & 
                                                       !is.na(table2df$t25_win_t_minus1) & 
                                                       !is.na(table2df$age) & 
                                                       !is.na(table2df$log_fihhyr) & 
                                                       !is.na(table2df$mastat) & 
                                                       !is.na(table2df$nkids) & 
                                                       !is.na(table2df$hlstat)]))

# Present the regression results using stargazer
stargazer(m1, m2, type = "text", title = "Logistic Regression Results", 
          dep.var.labels = c("Self-employed Probability"),
          omit.stat = c("f", "ser"),
          omit = c("age", "I(age^2)", "log_fihhyr", "mastat", "nkids", "hlstat", "region", "wave"),
          add.lines = list(c("Unique individuals", 
                             unique_individuals_m1, 
                             unique_individuals_m2)))
#Calculating Marginal Effects at Mean:
mem_t2_m1 <- summary(margins(m1))
mem_t2_m2 <- summary(margins(m2))

mem1_t2 <- extract_mem(mem_t2_m1)
mem2_t2 <- extract_mem(mem_t2_m2)


#Table 3
table2df <- table2df %>%
  mutate(
    partnered = ifelse(mastat == "Married" | mastat == "Living as Couple", 1, 0)
  )


youngdf <- subset(table2df, age < 40)
t3_m1 <- glm(selfemployed ~ female * t25_win_t_minus1 + age + I(age^2) + log_fihhyr + mastat + nkids + hlstat + wave + region, data = youngdf, family = binomial())

olddf <- subset(table2df, age >= 40)
t3_m2 <- glm(selfemployed ~ female * t25_win_t_minus1 + age + I(age^2) + log_fihhyr + mastat + nkids + hlstat + wave + region, data = olddf, family = binomial())

incomedf <- table2df %>%
  filter(!is.na(log_fihhyr))
median_log_income <- median(incomedf$log_fihhyr)

high_inc <- subset(table2df, log_fihhyr > median_log_income)
t3_m5 <- glm(selfemployed ~ female * t25_win_t_minus1 + age + I(age^2) + log_fihhyr + mastat + nkids + hlstat + wave + region, data = high_inc, family = binomial())

low_inc <- subset(table2df, log_fihhyr <= median_log_income)
t3_m6 <- glm(selfemployed ~ female * t25_win_t_minus1 + age + I(age^2) + log_fihhyr + mastat + nkids + hlstat + wave + region, data = low_inc, family = binomial())


table2df <- table2df %>%
  group_by(pidp) %>%
  mutate(log_windfgy_t_minus1 = lag(log_windfgy, 1, NA))

partnered <- subset(table2df, partnered == 1)
#t3_m7 <- glm(selfemployed ~ female * log_windfgy_t_minus1 + age + I(age^2) + log_fihhyr + mastat + nkids + hlstat + region, data = partnered, family = binomial())

not_partnered <- subset(table2df, partnered == 0)
t3_m8 <- glm(selfemployed ~ female * log_windfgy_t_minus1 + age + I(age^2) + log_fihhyr + mastat + nkids + hlstat + wave + region, data = not_partnered, family = binomial())

no_children <- subset(table2df, nkids == 0)
t3_m9 <- glm(selfemployed ~ female * log_windfgy_t_minus1 + age + I(age^2) + log_fihhyr + mastat + nkids + hlstat + wave + region, data = no_children, family = binomial())

children <- subset(table2df, nkids > 0)
t3_m10 <- glm(selfemployed ~ female * log_windfgy_t_minus1 + age + I(age^2) + log_fihhyr + mastat + nkids + hlstat + wave + region, data = children, family = binomial())




stargazer(t3_m1, t3_m2, t3_m5, t3_m6, t3_m8, t3_m9, t3_m10, type = "text", title = "Logistic Regression Results", 
          dep.var.labels = c("Self-employed Probability"),
          omit.stat = c("f", "ser"),
          omit = c("age", "I(age^2)", "log_fihhyr", "mastat", "nkids", "hlstat", "wave", "region"))

#Calculating Marginal Effects at Mean:
mem_t3_m1 <- summary(margins(t3_m1))
mem_t3_m2 <- summary(margins(t3_m2))

mem1_t3 <- extract_mem(mem_t3_m1)
mem2_t3 <- extract_mem(mem_t3_m2)

print(mem_t3_m2)


