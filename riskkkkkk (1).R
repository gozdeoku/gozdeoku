library(readxl)
riskkk <- read_excel("riskkk.xlsx")
View(riskkk)
attach(riskkk)
library(aod)
library(ggplot2)
str(riskkk)

riskkk$`System 1 or 2`<-factor(riskkk$`System 1 or 2`)
summary(riskkk)
riskkk$Frequency<-factor(riskkk$Frequency)
summary(riskkk)
riskkk$`Risk Tendency` <- factor(riskkk$`Risk Tendency`)
xtabs(~Frequency + `System 1 or 2`, data = riskkk)

sunk_cost <-`S1-S2`


riskkkmodel <- glm(Frequency ~ `System 1 or 2` + `Risk Tendency`, data = riskkk, family = "binomial")

summary(riskkkmodel)

riskkkmodel2 <- glm(`Risk Tendency`  ~ `System 1 or 2` + Frequency + sunk_cost, data = riskkk, family = "binomial")

summary(riskkkmodel2)

riskkkmodel3 <- glm(`System 1 or 2`  ~ `Risk Tendency` + Frequency + sunk_cost, data = riskkk, family = "binomial")

summary(riskkkmodel3)

riskkkmodel4 <- glm(sunk_cost  ~ `Risk Tendency` + Frequency + `System 1 or 2`, data = riskkk)
summary(riskkkmodel4)

exp(cbind(OR=coef(riskkkmodel4),confint(riskkkmodel4)))

exp(coef(riskkkmodel4))


ggplot(riskkk, aes(x = `S1-S2`)) +
  geom_histogram(fill = "#0072B2", color = "black", bins = 30) +
  labs(title = "Distribution of Sunk Cost Effect", x = "S1 - S2 (TL)", y = "Frequency") +
  theme_minimal()
ggplot(riskkk, aes(x = `System 1 or 2`, y = `S1-S2`, fill = `System 1 or 2`)) +
  geom_boxplot() +
  labs(title = "Sunk Cost Effect by System Type", x = "System Type", y = "S1 - S2 (TL)") +
  scale_fill_manual(values = c("1" = "#E69F00", "2" = "#56B4E9")) +
  theme_minimal()

install.packages("corrplot")
library(corrplot)
library(dplyr)

# Sayısal sütunları seç
num_data <- riskkk %>%
  select(`S1-S2`, `System 1 or 2`, `Risk Tendency`, Frequency) %>%
  mutate_all(as.numeric)

cor_matrix <- cor(num_data, use = "complete.obs")

corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.cex = 0.8,
         col = colorRampPalette(c("red", "white", "blue"))(200),
         title = "Correlation Matrix", mar = c(0, 0, 1, 0))

