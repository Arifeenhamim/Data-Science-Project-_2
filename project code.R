library(readxl)
Dataset_MIdterm_sectoinB <- read_excel("Dataset_MIdterm_sectoinB.xlsx")
View(Dataset_MIdterm_sectoinB)


dataset2<-Dataset_MIdterm_sectoinB



dataset2$age[is.na(dataset2$age)] <- mean(dataset2$age, na.rm = TRUE)  


if (anyNA(dataset2$gender)) {
  
  mode_gender <- as.character(names(sort(table(dataset2$gender), decreasing = TRUE)[1]))
  
  
  dataset2$gender[is.na(dataset2$gender)] <- mode_gender}

if (anyNA(dataset2$smoking_history)) {
  mode_smoking_history <- as.character(names(sort(table(dataset2$smoking_history), decreasing = TRUE)[1]))
  
  
  dataset2$smoking_history[is.na(dataset2$smoking_history)] <- mode_smoking_history
}




dataset2$hypertension[is.na(dataset2$hypertension)] <- mean(dataset2$hypertension, na.rm = TRUE) 


dataset2$age<-ceiling(dataset2$age)
dataset2$hypertension<-ceiling(dataset2$hypertension)



mean_age <- mean(dataset2$age, na.rm = TRUE)

dataset2$age[dataset2$age %in% c(280, 290)] <- mean_age

dataset2$bmi <- abs(dataset2$bmi)


install.packages("dplyr") 
library(dplyr) 




dataset2 <- dataset2 %>%
  mutate(bgl_type = case_when(
    blood_glucose_level < 100 ~ "low",
    blood_glucose_level < 200 ~ "medium",
    blood_glucose_level < 300 ~ "high",
    blood_glucose_level >= 300 ~ "extra-high"
  ))



dataset2$age = as.numeric(format(round(dataset2$age,0)))
dataset2$hypertension = as.numeric(format(round(dataset2$hypertension,0)))


dataset2$HbA1c_level<-ceiling(dataset2$HbA1c_level)
dataset2$bmi<-ceiling(dataset2$bmi)


install.packages("ggplot2")
library(ggplot2)


ggplot(dataset2, aes(x = age)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Age", x = "Age", y = "Frequency")


ggplot(dataset2, aes(x = bgl_type, fill = bgl_type)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Histogram of Blood Glucose Level Types", x = "Blood Glucose Level Type", y = "Frequency")

ggplot(dataset2, aes(x = gender)) +
  geom_bar(fill = "red", color = "black") +
  labs(title = "Distribution of Gender", x = "Gender", y = "Count") +
  theme_minimal()

ggplot(dataset2, aes(x = bmi)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black") +
  labs(title = "Histogram of BMI", x = "BMI", y = "Frequency") +
  theme_minimal()


ggplot(dataset2, aes(x = hypertension)) +
  geom_histogram(binwidth = 1, fill = "yellow", color = "black") +
  labs(title = "Histogram of Hypertension", x = "Hypertension", y = "Frequency") +
  theme_minimal()


ggplot(dataset2, aes(x = HbA1c_level)) +
  geom_histogram(binwidth = 0.5, fill = "orange", color = "black") + 
  labs(title = "Histogram of HbA1c Levels", x = "HbA1c Level", y = "Frequency") +
  theme_minimal()


ggplot(dataset2, aes(x = blood_glucose_level)) +
  geom_histogram(binwidth = 10, fill = "purple", color = "black") + 
  labs(title = "Histogram of Blood Glucose Levels", x = "Blood Glucose Level", y = "Frequency") +
  theme_minimal()


ggplot(dataset2, aes(x = heart_disease)) +
  geom_bar(fill = "green", color = "black") +
  labs(title = "Distribution of Heart Disease", x = "Heart Disease", y = "Count") +
  theme_minimal()


ggplot(dataset2, aes(x = smoking_history)) +
  geom_bar(fill = "red", color = "black") +
  labs(title = "Distribution of Smoking History", x = "Smoking History", y = "Count") +
  theme_minimal()

ggplot(dataset2, aes(x = diabetes)) +
  geom_bar(fill = "orange", color = "black") +
  labs(title = "Distribution of Diabetes", x = "Diabetes", y = "Count") +
  theme_minimal()
