# MST 202 - Assignment Term 4
# Student number: 23G4435
# Name: Master Ganyali

# QUESTION 1

degree <- c("BA","BSc","BCom","BCom","BA","BA","BA",
            "BCom","BA","BCom","BCom","BA","BA","BSc")

save(degree, file = "23G4435_question1.Rdata")

# Numerical summary
table(degree)
prop.table(table(degree))

# Graphical summary
barplot(table(degree),
        main = "Degree of Students Using the Library",
        xlab = "Degree",
        ylab = "Frequency")

# QUESTION 2
q2 <- read.csv("23G4435/23G4435_question2.csv")
q2$Location <- factor(q2$Location)
q2$Response <- factor(q2$Response,
                      levels = c("Strongly Disagree",
                                 "Disagree",
                                 "Uncertain",
                                 "Agree",
                                 "Strongly Agree"))

# Numerical summaries
table(q2$Location)
table(q2$Response)
q2_table <- table(q2$Location, q2$Response)
q2_table
prop.table(q2_table, margin = 1)

# Graphical summary
barplot(q2_table,
        beside = TRUE,
        legend.text = TRUE,
        main = "Rhodes Status and Response",
        xlab = "Response",
        ylab = "Frequency")

# Chi-square test of independence
q2_chisq <- chisq.test(q2_table)
q2_chisq
q2_chisq$expected

# QUESTION 3
q3 <- read.csv("23G4435/23G4435_question3.csv")
q3

q3$Zone <- factor(q3$Zone)

# Numerical summaries
by(q3$Fecundity, q3$Zone, summary)
aggregate(Fecundity ~ Zone, data = q3,
          FUN = function(x) c(n = length(x),
                              mean = mean(x),
                              sd = sd(x),
                              median = median(x)))

# Graphical summary
boxplot(Fecundity ~ Zone, data = q3,
        main = "Fecundity by Tidal Zone",
        xlab = "Tidal zone",
        ylab = "Egg production")

# Check equality of variances
fligner.test(Fecundity ~ Zone, data = q3)

# Two-sample t-test with equal variances
q3_test <- t.test(Fecundity ~ Zone, data = q3,
                  var.equal = TRUE)
q3_test

# QUESTION 4
q4 <- read.csv("23G4435/23G4435_question4.csv")
q4$Sex <- factor(q4$Sex)

# Numerical summaries
by(q4$MetabolicRate, q4$Sex, summary)
aggregate(MetabolicRate ~ Sex, data = q4,
          FUN = function(x) c(n = length(x),
                              mean = mean(x),
                              sd = sd(x),
                              median = median(x)))

# Graphical summary
boxplot(MetabolicRate ~ Sex, data = q4,
        main = "Metabolic Rate by Sex",
        xlab = "Sex",
        ylab = "Metabolic rate (kJ)")

# Check equality of variances
fligner.test(MetabolicRate ~ Sex, data = q4)
# Welch two-sample t-test
q4_test <- t.test(MetabolicRate ~ Sex, data = q4,
                  var.equal = FALSE)
q4_test

# QUESTION 5
q5 <- read.csv("23G4435/23G4435_question5.csv")
q5$Dose <- factor(q5$Dose)

# Numerical summaries
by(q5$P24Level, q5$Dose, summary)
aggregate(P24Level ~ Dose, data = q5,
          FUN = function(x) c(n = length(x),
                              mean = mean(x),
                              sd = sd(x),
                              median = median(x)))

# Graphical summary
boxplot(P24Level ~ Dose, data = q5,
        main = "p24 Level by AZT Dose",
        xlab = "Dose (mg)",
        ylab = "p24 level")

# Check equality of variances
fligner.test(P24Level ~ Dose, data = q5)

# Welch two-sample t-test
q5_test <- t.test(P24Level ~ Dose, data = q5,
                  var.equal = FALSE)
q5_test
# QUESTION 6

q6 <- read.csv("23G4435/23G4435_question6.csv")
q6
q6_complete <- q6[complete.cases(q6$Lab1, q6$Lab2), ]

# Numerical summaries
summary(q6_complete$Lab1)
summary(q6_complete$Lab2)
mean(q6_complete$Lab1)
sd(q6_complete$Lab1)
mean(q6_complete$Lab2)
sd(q6_complete$Lab2)

# Paired graphical summary
plot(q6_complete$Lab1, q6_complete$Lab2,
     xlab = "Lab 1",
     ylab = "Lab 2",
     main = "Lab 1 vs Lab 2")
abline(0, 1)

# Differences
difference <- q6_complete$Lab1 - q6_complete$Lab2
summary(difference)
mean(difference)
sd(difference)

# Paired t-test
q6_test <- t.test(q6_complete$Lab1,
                  q6_complete$Lab2,
                  paired = TRUE)
q6_test

# QUESTION 7

q7 <- read.csv("23G4435/23G4435_question7.csv")
q7$Treatment <- factor(q7$Treatment,
                       levels = c("Control",
                                  "Treatment 1",
                                  "Treatment 2"))

# Numerical summaries
by(q7$Yield, q7$Treatment, summary)
aggregate(Yield ~ Treatment, data = q7,
          FUN = function(x) c(n = length(x),
                              mean = mean(x),
                              sd = sd(x),
                              median = median(x)))

# Graphical summary
boxplot(Yield ~ Treatment, data = q7,
        main = "Fruit Yield by Treatment",
        xlab = "Treatment",
        ylab = "Yield (kg)")

# One-way ANOVA
q7_model <- aov(Yield ~ Treatment, data = q7)
summary(q7_model)

# Check ANOVA assumptions
fligner.test(Yield ~ Treatment, data = q7)
shapiro.test(residuals(q7_model))

