# Load necessary libraries
library(GGally)
library(ggplot2)
library(ggridges)
library(gridExtra)
library(mclust)
library(rio)
library(tidyverse)
library(reshape2)

# read the data
data <- import("https://github.com/lamethods/data/raw/main/3_engSRLach/Manuscript_School%20Engagment.csv", 
               sep = ";")

# Rename engagement variables for better readability
engage_vars <- c("PRE_ENG_COND", "PRE_ENG_COGN", "PRE_ENG_EMOC")
engage <- select(data, all_of(engage_vars)) |> 
  as_tibble() |>
  rename("Behavioural" = "PRE_ENG_COND", # Behavioral engagement
         "Cognitive" = "PRE_ENG_COGN",   # Cognitive engagement
         "Emotional" = "PRE_ENG_EMOC")   # Emotional engagement

# Print the engagement data
engage

# Summary statistic to understand the engagement data
engage |> pivot_longer(cols = colnames(engage),
                       names_to = "Variable",
                       values_to = "Value") |>
  group_by(Variable) |>
  summarise(N = n(),
            Nunq = n_distinct(Value),
            Mean = mean(Value),
            SD = sd(Value),
            Min = min(Value),
            Median = median(Value),
            Max = max(Value))

# Convert engagement data into long format for analysis
engage_long <- engage |>
  as_tibble(rownames="type") |>
  pivot_longer(cols=-type, names_to="vars", values_to="vals")
engage_long

# Perform pairplot analysis for engagement variables
ggpairs(engage)

#Box plot analysis for engagement cariable
ggplot(engage_long, aes(y=vals, fill=vars)) +
  geom_boxplot() +
  facet_wrap(vars~.) +
  xlab("Engagement Score") +
  theme(legend.position="none")

# Rename grades and self-regulation variables for better readability
grades_vars <- c("ren.mat.pret", "ren.leng.pret")
regulate_vars <- c("enviroment_manag", "time_manag", "maladaptative_behavior", "informati_manag","sexo.pret")

grades <- select(data, all_of(grades_vars)) |> 
  as_tibble() |>
  rename("Mathematics" = "ren.mat.pret",
         "Spanish" = "ren.leng.pret")   

regulate <- select(data, all_of(regulate_vars)) |> 
  as_tibble() |>
  rename("Environment" = "enviroment_manag", 
         "Time" = "time_manag",
         "Maladaptive" = "maladaptative_behavior",
         "Information" = "informati_manag",
         "Gender" = "sexo.pret")

# Summary statistics for grades
grades |> pivot_longer(cols = colnames(grades),
                       names_to = "Variable",
                       values_to = "Value") |>
  group_by(Variable) |>
  summarise(N = n(),
            Nunq = n_distinct(Value),
            Mean = mean(Value),
            SD = sd(Value),
            Min = min(Value),
            Median = median(Value),
            Max = max(Value))

# Barplot of Mathematics grades by gender

data_combined_math <- bind_cols(grades, regulate) |>
  mutate(Gender = ifelse(Gender == 1, "Male", "Female"),
         Mathematics = factor(Mathematics)) |>
  filter(!is.na(Gender)) |>
  group_by(Gender, Mathematics) |>
  summarise(count = n(), .groups = 'drop') |>
  mutate(Proportions = count / sum(count))

p1 <- ggplot(data_combined_math, aes(x = Mathematics, y = Proportions, fill = Mathematics)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Gender) +
  theme_minimal() +
  labs(x = "Mathematics Grades", y = "Proportion", title = "") +
  guides(fill = guide_legend(title = "Grade"))

# Barplot of Spanish grades by gender
data_combined_spanish <- bind_cols(grades, regulate) |>
  mutate(Gender = ifelse(Gender == 1, "Male", "Female"),
         Spanish = factor(Spanish)) |>
  filter(!is.na(Gender)) |>
  group_by(Gender, Spanish) |>
  summarise(count = n(), .groups = 'drop') |>
  mutate(Proportions = count / sum(count))

p2 <- ggplot(data_combined_spanish, aes(x = Spanish, y = Proportions, fill = Spanish)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Gender) +
  theme_minimal() +
  labs(x = "Spanish Grades", y = "Proportion", title = "") +
  guides(fill = guide_legend(title = "Grade"))

# Print the barplots
print(p1)
print(p2)

# Merging the grades and regulation data into one dataframe
data_combined <- bind_cols(grades, regulate) |>
  mutate(Gender = ifelse(Gender == 1, "Male", "Female"))

# Compute the correlation matrix for the combined data (excluding Gender)
cor_matrix <- cor(data_combined |> select(-Gender), use = "complete.obs", method="kendall")

# Melt the correlation matrix for ggplot
melted_cor_matrix <- melt(cor_matrix)

# Create the correlation heatmap
p1 <- ggplot(data = melted_cor_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "white", size = 4) +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Kendall\nCorrelation") +
  theme_minimal() +
  #labs(title = "Correlation Heatmap of Grades and Regulation Variables", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))
print(p1)

# First BIC model for engagement variables using default settings
# BIC <- mclustBIC(engage, G=1:9)

load("BIC.Rda")

summary(BIC)
plot(BIC)

# Fit the model using the BIC
mod1 <- Mclust(engage, x = BIC)
summary(mod1)
plot(mod1, what = "classification")

# Fit the model using the BIC with default prior
# BIC2 <- mclustBIC(engage, prior=priorControl("defaultPrior"))

load("BIC2.Rda")
summary(BIC2)
plot(BIC2)

mod2 <- Mclust(engage, x = BIC2)
summary(mod2, parameters=TRUE)
plot(mod2, what="classification")

# Fit the model using the BIC with equal mixing proportions
# BIC3 <- mclustBIC(engage, G=2:5,
#                  prior=priorControl("defaultPrior"), 
#                  control=emControl(equalPro=TRUE))

load("BIC3.Rda")

summary(BIC3)
plot(BIC3)
 
mod3 <- Mclust(engage, x = BIC3)
summary(mod3, parameters=TRUE)
plot(mod3, what="classification")

# Fit the model using the BIC with different hyperparameter settings
# BIC4 <- mclustBIC(engage, G=2:5,
#                  prior=priorControl("defaultPrior", scale=diag(ncol(engage))))
# BIC5 <- mclustBIC(engage, G=2:5,
#                  prior=priorControl("defaultPrior", shrinkage=0))
# BIC6 <- mclustBIC(engage, G=2:5,
#                  prior=priorControl("defaultPrior", shrinkage=0, scale=diag(ncol(engage))))
 BICs <- mclustBICupdate(BIC4, BIC5, BIC6)
summary(BICs)
summary(BIC2) # compare with old optimal model

load("BIC4.Rda")
load("BIC5.Rda")
load("BIC6.Rda")

# Adding the noise component by calculating the Mahalanobis distance
distances <- mahalanobis(engage, center=colMeans(engage), cov=cov(engage))
cutoff <- qchisq(p = 0.9, df = ncol(engage))
init_noise <- distances > cutoff
pairs(engage, col=init_noise + 1, pch=init_noise + 1)

# Fit the model using the BIC with noise component and different priors
# BIC7 <- mclustBIC(engage, G=1:5,
#                  initialization=list(noise=init_noise),
#                  prior=priorControl("defaultPrior", shrinkage=0))

load("BIC7.Rda")

summary(BIC7)
summary(BICs) # compare with old optimal model

# Final model 
final_mod <- Mclust(engage, x = BIC7)
summary(final_mod, parameters=TRUE)
plot(final_mod, what="classification")

# Calculate means and mixng probability of cluster means
means <- data.frame(Profile = factor(1:final_mod$G),
                    t(final_mod$parameters$mean)) |>
  pivot_longer(cols = -1,
               names_to = "Variable",
               values_to = "Mean")

# convert variable names to factor
means$Variable <- factor(means$Variable, 
                         levels = colnames(final_mod$data))

# add mixing probabilities corresponding to profiles
means <- means |> 
  add_column(MixPro = final_mod$parameters$pro[means$Profile])
means

# Adding R script for incorporating "bs" and "wlbs" method function in MclustBootstrap
source("fixed_mclust_functions.R")

# Performing nonparametric and weighted likelihood bootstrap
boot1 <- MclustBootstrap_fixed(final_mod, type="bs", nboot=999, prior=priorControl("defaultPrior", shrinkage=0))
boot2 <- MclustBootstrap_fixed(final_mod, type="wlbs", nboot=999, prior=priorControl("defaultPrior", shrinkage=0))

sboot1 <- summary(boot1, what = "ci") # type="bs"
sboot1

sboot2 <- summary(boot2, what = "ci") # type="wlbs"
sboot2

# Bootstrap distribution of GMM mixing proportions and component means
par(mfcol = c(1, 3), mar = c(2, 4, 1, 1), mgp = c(2, 0.5, 0))
plot(boot1, what = "pro", xlim = c(0, 1), main="Nonparametric Bootstrap")

par(mfcol = c(1, 3), mar = c(2, 4, 1, 1), mgp = c(2, 0.5, 0))
plot(boot2, what = "pro", xlim = c(0, 1), main="Weighted Likelihood Bootstrap")

par(mfcol = c(3, 3), mar = c(2, 4, 1, 1), mgp = c(2, 0.5, 0))
plot(boot1, what = "mean", main="Nonparametric Bootstrap")

par(mfcol = c(3, 3), mar = c(2, 4, 1, 1), mgp = c(2, 0.5, 0))
plot(boot2, what = "mean", main="Weighted Likelihood Bootstrap")

# Add confidence intervals from non-parametric bootstrap to means data.frame
mean_ci <- means |> 
  select(-MixPro) |>
  add_column(lower = as.vector(sboot1$mean[1,,]),
             upper = as.vector(sboot1$mean[2,,]))
mean_ci

pro_ci <- means |> 
  select(-c(Variable, Mean)) |>
  distinct() |>
  mutate(Profile=factor(Profile, levels=c(1:3, 0))) |>
  add_row(tibble_row(Profile=factor(0), MixPro=final_mod$parameters$pro[final_mod$G + 1])) |>
  add_column(lower = sboot1$pro[1,],
             upper = sboot1$pro[2,])
pro_ci

# Merging means and mixing probabilities into one dataframe 
merged_df <- merge(mean_ci, pro_ci[, c("Profile", "MixPro")], by = "Profile")

# Plot means with error bars using nonparamtric bootstrap
ggplot(merged_df, aes(x = Variable, y = Mean,
                      group = Profile, 
                      shape = Profile, 
                      color = Profile)) +
  geom_point(aes(size = MixPro)) +
  geom_line(linewidth = 0.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                linewidth = 0.5, width = 0.1) +
  labs(x = NULL, y = "Latent profiles means") +
  scale_color_manual(values = mclust.options("classPlotColors")) +
  scale_size(range = c(1, 3), guide = "none") +
  theme_bw() +
  theme(legend.position = "top")

# Calculate column means for different classifications
colMeans(engage[final_mod$classification == 0,])
colMeans(engage[final_mod$classification != 0,])
colMeans(grades[final_mod$classification == 0,])
colMeans(grades[final_mod$classification != 0,])
colMeans(regulate[final_mod$classification == 0,])
colMeans(regulate[final_mod$classification != 0,], na.rm=TRUE)

# Final model classification table 
final_mod$classification
table(final_mod$classification)
prop.table(table(final_mod$classification)) * 100

# Entropy & average posterior probability 
# first approach: add + 1 for the noise component
probs <- final_mod$z                 # posterior conditional probs
probs_map <- apply(probs, 1, max)    # maximum a posteriori probs
clusters <- final_mod$classification # cluster assignment for each obs
n <- final_mod$n                     # number of obs
K <- final_mod$G + 1                 # number of latent profiles

# Entropy:
E <- 1 + sum(probs * log(probs))/(n * log(K))
E

# Case-specific entropy contributions:
Ei <- 1 + rowSums(probs * log(probs))/log(K)
sum(Ei)/n # equal to E

# Summarising the entropies by cluster
df_entropy  <- data.frame(clusters = as.factor(clusters), entropy = Ei)
df_entropy |>
  group_by(clusters) |>
  summarise(count = n(),
            mean = mean(entropy),
            sd = sd(entropy),
            min = min(entropy),
            max = max(entropy))

# Summarising the average posterior probabilities by cluster
df_AvePP <- data.frame(clusters = as.factor(clusters), pp = probs_map)
df_AvePP |>
  group_by(clusters) |>
  summarise(count = n(),
            mean = mean(pp),
            sd = sd(pp),
            min = min(pp),
            max = max(pp))

# Plot case-specific entropy contributions by clusters
ggplot(df_entropy, aes(y = clusters, x = entropy, fill = clusters)) +
  geom_density_ridges(stat = "binline", bins = 21, scale = 0.9, alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1.05)) +
  scale_fill_manual(values = mclust.options("classPlotColors")) +
  geom_vline(xintercept = E, lty = 2) +
  labs(x = "Case-specific entropy contribution", y = "Latent profile") +
  theme_ridges(center_axis_labels = TRUE) +
  theme(legend.position = "none",
        panel.spacing = unit(1, "lines"),
        strip.text.x = element_text(size = 8))

# Plot average posterior probability contributions by clusters
ggplot(df_AvePP, aes(y = clusters, x = pp, fill = clusters)) +
  geom_density_ridges(stat = "binline", bins = 21, scale = 0.9, alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1.05)) +
  scale_fill_manual(values = mclust.options("classPlotColors")) +
  labs(x = "MAP probabilities", y = "Latent profile") +
  theme_ridges(center_axis_labels = TRUE) +
  theme(legend.position = "none",
        panel.spacing = unit(1, "lines"),
        strip.text.x = element_text(size = 8))

# Mathematics grade distribution in cluster
df <- as.data.frame(cbind(grades, regulate, clusters=factor(final_mod$classification)))
df |>
  mutate(Mathematics=factor(Mathematics),
         Spanish=factor(Spanish)) |>
  filter(clusters != 0) |>
  mutate(clusters=factor(clusters, labels=paste("Cluster", 1:final_mod$G))) |>
  ggplot(aes(x=Mathematics)) +
  geom_bar(aes(fill=Mathematics)) +
  facet_wrap(~clusters, scales="free_y")

# Spanish grade distribution in clusters
df |>
  mutate(Mathematics=factor(Mathematics),
         Spanish=factor(Spanish)) |>
  filter(clusters != 0) |>
  mutate(clusters=factor(clusters, labels=paste("Cluster", 1:final_mod$G))) |>
  ggplot(aes(x=Spanish)) +
  geom_bar(aes(fill=Spanish)) +
  facet_wrap(~clusters, scales="free_y")

# Gender distribution in clusters
df <- as.data.frame(cbind(grades, regulate, clusters=factor(final_mod$classification)))
df |>
  mutate(Gender=factor(Gender),
         Mathematics=factor(Mathematics)) |>
  filter(clusters != 0 & !is.na(Gender)) |>
  mutate(clusters=factor(clusters, labels=paste("Cluster", 1:final_mod$G))) |>
  ggplot(aes(x=Gender)) +
  geom_bar(aes(fill=Gender)) +
  facet_wrap(~clusters, scales="free_y")

# Ploting covariates for cluster analysis
# Filter out Cluster 0
df_filtered <- df |> filter(clusters != 0)

# Box plot for Environment Management by cluster
df_filtered |>
  ggplot(aes(x = clusters, y = Environment, fill = clusters)) +
  geom_boxplot() +
  labs(title = "Box Plot of Environment Management by Cluster", 
       x = "Cluster", y = "Environment Management") +
  theme_minimal()

# Box plot for Time Management by cluster
df_filtered |>
  ggplot(aes(x = clusters, y = Time, fill = clusters)) +
  geom_boxplot() +
  labs(title = "Box Plot of Time Management by Cluster", 
       x = "Cluster", y = "Time Management") +
  theme_minimal()

# Box plot for Maladaptive Behavior by cluster
df_filtered |>
  ggplot(aes(x = clusters, y = Maladaptive, fill = clusters)) +
  geom_boxplot() +
  labs(title = "Box Plot of Maladaptive Behavior by Cluster", 
       x = "Cluster", y = "Maladaptive Behavior") +
  theme_minimal()

# Box plot for Information Management by cluster
df_filtered |>
  ggplot(aes(x = clusters, y = Information, fill = clusters)) +
  geom_boxplot() +
  labs(title = "Box Plot of Information Management by Cluster", 
       x = "Cluster", y = "Information Management") +
  theme_minimal()
