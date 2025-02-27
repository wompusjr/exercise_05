library(tidyverse)
#### challenge 1
#step one
d <- read_csv("https://raw.githubusercontent.com/difiore/ada-datasets/main/IMDB-movies.csv", col_names = TRUE)
attach(d)
#step two
d <- d |> filter(startYear >= 1920 & startYear <= 1979) |>
  filter(runtimeMinutes >= 60 & runtimeMinutes <= 180) |>
  mutate(Decade = ifelse(startYear >= 1920 & startYear <= 1929, "20s", 
                         ifelse(startYear >= 1930 & startYear <= 1939, "30s",
                                ifelse(startYear >= 1940 & startYear <= 1949, "40s",
                                       ifelse(startYear >= 1950 & startYear <= 1959, "50s",
                                              ifelse(startYear >= 1960 & startYear <= 1969, "60s",
                                                     ifelse(startYear >= 1970 & startYear <= 1979, "70s", NA)))))))
#step three
library(ggplot2)
(p <- ggplot(d, aes(x=runtimeMinutes)) + 
  geom_histogram(color="lightblue", fill = "steelblue") +
  facet_wrap(~ Decade))
#step four
(results <- d |>
  group_by(Decade) |>
  summarise(mean_runtime = mean(runtimeMinutes, na.rm = TRUE), 
            sd_runtime = sqrt(sum((runtimeMinutes - mean(runtimeMinutes))^2)/length(runtimeMinutes))))
#step five
(samp_results <- d|> 
    group_by(Decade) |>
    slice_sample(n = 100) |>
    select(runtimeMinutes) |>
    summarise (samp_mean_runtime = mean(runtimeMinutes, na.rm = TRUE),
               samp_sd_runtime = sd(runtimeMinutes, na.rm = TRUE)))
#step six
(standard_errors <- samp_results$samp_sd_runtime/sqrt(100))

#step seven

#step eight
library(mosaic)
reps <- 1000
samp_results_df <- do(1:reps, {
  d |> 
    group_by(Decade) |> 
    slice_sample(n = 100, replace = FALSE) |> 
    select(runtimeMinutes) |> 
    summarise(
      samp_mean_runtime = mean(runtimeMinutes, na.rm = TRUE),
      samp_sd_runtime = sd(runtimeMinutes, na.rm = TRUE)
    )
})
#step nine
samp_dist <- samp_dist_df |>
  group_by(Decade) |>
  summarise(
    samp_dist_mean_runtime = mean(samp_mean_runtime),
    samp_dist_sd_runtime = sd(samp_mean_runtime)
  )
(p2 <- ggplot(samp_dist_df, aes(x = samp_mean_runtime)) + 
    geom_histogram(color="lightblue", fill = "steelblue") +
    facet_wrap(~ Decade))
detach(d)
#step ten 

#### challenge 2
#step one
z <- read_csv("https://raw.githubusercontent.com/difiore/ada-datasets/main/zombies.csv", col_names = TRUE)
attach(z)
#step two
(z_result <- z |>
    summarise(mean_height = mean(height, na.rm = TRUE), 
              sd_height = sqrt(sum((height - mean(height))^2)/length(height)),
              mean_weight = mean(weight, na.rm = TRUE), 
              sd_weight = sqrt(sum((weight - mean(weight))^2)/length(weight)),
              mean_zkills = mean(zombies_killed, na.rm = TRUE), 
              sd_zkills = sqrt(sum((zombies_killed - mean(zombies_killed))^2)/length(zombies_killed)),
              mean_ed = mean(years_of_education, na.rm = TRUE), 
              sd_ed = sqrt(sum((years_of_education - mean(years_of_education, na.rm = TRUE))^2, na.rm = TRUE) / (length(years_of_education) - sum(is.na(years_of_education))))))
#step three
install.packages("ggpubr")
library(ggpubr)
ph <- ggplot(z, aes(y = height)) + 
    geom_boxplot(color="lightblue", fill = "steelblue")+
    facet_wrap(~ gender)
pw <- ggplot(z, aes(y = weight)) + 
  geom_boxplot(color="lightblue", fill = "steelblue")+
  facet_wrap(~ gender)
pz <- ggplot(z, aes(y = zombies_killed)) + 
  geom_boxplot(color="lightblue", fill = "steelblue")+
  facet_wrap(~ gender)
pe <- ggplot(z, aes(y = years_of_education)) + 
  geom_boxplot(color="lightblue", fill = "steelblue")+
  facet_wrap(~ gender)
(stepthree <- ggarrange(ph, pw, pz, pe,
                    labels = NA,
                    ncol = 2, nrow = 2))
#step four
(ph2 <- ggplot(z, aes(age, height, color=gender)) + 
  geom_point() +
    scale_color_manual(values = c("Female" = "purple",
                                  "Male"="green")))
(pw2 <- ggplot(z, aes(age, weight, color=gender)) + 
    geom_point() +
    scale_color_manual(values = c("Female" = "purple",
                                  "Male"="green")))
(stepfour <- ggarrange(ph2,pw2,
                        labels = NA,
                        ncol = 1, nrow = 2))
#step five
ph3norm <- ggplot(z, aes(x = height)) + 
    geom_histogram(color="lightblue", fill = "steelblue")
pw3norm <- ggplot(z, aes(x = weight)) + 
  geom_histogram(color="lightblue", fill = "steelblue")
pz3norm <- ggplot(z, aes(x = zombies_killed)) + 
  geom_histogram(color="lightblue", fill = "steelblue")
pe3norm <- ggplot(z, aes(x = years_of_education)) + 
  geom_histogram(color="lightblue", fill = "steelblue")
(ggarrange(ph3norm,pw3norm,pz3norm,pe3norm,
          labels = NA,
          ncol = 2, nrow = 2))
par(mfrow = c(2, 2))
qqnorm(z$height, main = "Height - QQ")
qqline(z$height, col = "gray")
qqnorm(z$weight, main = "Weight - QQ")+ 
qqline(z$weight, col = "gray")
qqnorm(z$zombies_killed, main = "Zombies Killed - QQ")+ 
qqline(z$zombies_killed, col = "gray")
qqnorm(z$years_of_education, main = "Years of Education - QQ")+ 
qqline(z$years_of_education, col = "gray")
par(mfrow = c(2, 2))
#step six
library(manipulate)
CI <- function(x,y, level = 0.95) {
  alpha <- 1 - level
  ci <- x + c(-1, 1) * qnorm(1 - (alpha/2)) * y
  return(ci)
}
(sampleone <- z|> 
    slice_sample(n = 50) |>
    summarise (samp_mean_height = mean(height, na.rm = TRUE),
               samp_sd_height = sd(height, na.rm = TRUE),
               samp_mean_weight = mean(weight, na.rm = TRUE),
               samp_sd_weight = sd(weight, na.rm = TRUE),
               samp_mean_zkilled = mean(zombies_killed, na.rm = TRUE),
               samp_sd_zkilled = sd(zombies_killed, na.rm = TRUE),
               samp_mean_ed = mean(years_of_education, na.rm = TRUE),
               samp_sd_ed = sd(years_of_education, na.rm = TRUE)))
(se_height <- sampleone$samp_sd_height/sqrt(length(sampleone)))
(ci_height <- CI(sampleone$samp_mean_height,se_height))

(se_weight <- sampleone$samp_sd_weight/sqrt(length(sampleone)))
(ci_weight <- CI(sampleone$samp_mean_weight,se_weight))

(se_zkilled <- sampleone$samp_sd_zkilled/sqrt(length(sampleone)))
(ci_zkilled <- CI(sampleone$samp_mean_zkilled,se_zkilled))

(se_ed <- sampleone$samp_sd_ed/sqrt(length(sampleone)))
(ci_ed <- CI(sampleone$samp_mean_ed,se_ed))
#step seven
library(purrr)
reps <- 199
(samp_dist_z <- map_dfr(1:reps, ~ {
  z |> 
    slice_sample(n = 50, replace = FALSE) |> 
    summarise(
      samp_mean_height = mean(height, na.rm = TRUE),
      samp_sd_height = sd(height, na.rm = TRUE),
      samp_mean_weight = mean(weight, na.rm = TRUE),
      samp_sd_weight = sd(weight, na.rm = TRUE),
      samp_mean_zkilled = mean(zombies_killed, na.rm = TRUE),
      samp_sd_zkilled = sd(zombies_killed, na.rm = TRUE),
      samp_mean_ed = mean(years_of_education, na.rm = TRUE),
      samp_sd_ed = sd(years_of_education, na.rm = TRUE)
    )
}))
samp_dist_z <- bind_rows(sampleone, samp_dist_z)
(samp_dist_z_result <- samp_dist_z |>
  summarise(
    s_mean_height = mean(samp_mean_height, na.rm = TRUE),
    s_sd_height = sd(samp_mean_height, na.rm = TRUE),
    s_mean_weight = mean(samp_mean_weight, na.rm = TRUE),
    s_sd_weight = sd(samp_mean_weight, na.rm = TRUE),
    s_mean_zkilled = mean(samp_mean_zkilled, na.rm = TRUE),
    s_sd_zkilled = sd(samp_mean_zkilled, na.rm = TRUE),
    s_mean_ed = mean(samp_mean_ed, na.rm = TRUE),
    s_sd_ed = sd(samp_mean_ed, na.rm = TRUE)
  ))
#step eight
ph4norm <- ggplot(samp_dist_z, aes(x = samp_mean_height)) + 
  geom_histogram(color="lightblue", fill = "steelblue")
pw4norm <- ggplot(samp_dist_z, aes(x = samp_mean_weight)) + 
  geom_histogram(color="lightblue", fill = "steelblue")
pz4norm <- ggplot(samp_dist_z, aes(x = samp_mean_zkilled)) + 
  geom_histogram(color="lightblue", fill = "steelblue")
pe4norm <- ggplot(samp_dist_z, aes(x = samp_mean_ed)) + 
  geom_histogram(color="lightblue", fill = "steelblue")
(ggarrange(ph4norm,pw4norm,pz4norm,pe4norm,
           labels = NA,
           ncol = 2, nrow = 2))
par(mfrow = c(2, 2))
qqnorm(samp_dist_z$samp_mean_height, main = "Height - QQ")
qqline(samp_dist_z$samp_mean_height, col = "gray")
qqnorm(samp_dist_z$samp_mean_weight, main = "Weight - QQ")+ 
  qqline(samp_dist_z$samp_mean_weight, col = "gray")
qqnorm(samp_dist_z$samp_mean_zkilled, main = "Zombies Killed - QQ")+ 
  qqline(samp_dist_z$samp_mean_zkilled, col = "gray")
qqnorm(samp_dist_z$samp_mean_ed, main = "Years of Education - QQ")+ 
  qqline(z$samp_dist_z$samp_mean_ed, col = "gray")
par(mfrow = c(2, 2))
#step nine
(s_ci_height <- quantile(samp_dist_z$samp_mean_height,probs = c(0.025, 0.975)))
(s_ci_weight <- quantile(samp_dist_z$samp_mean_weight,probs = c(0.025, 0.975)))
(s_ci_zkilled <- quantile(samp_dist_z$samp_mean_zkilled,probs = c(0.025, 0.975)))
(s_ci_ed <- quantile(samp_dist_z$samp_mean_ed,probs = c(0.025, 0.975)))
#step ten
n_boot <- 10000
##height
hboot <- vector(length = n_boot)
n_height <- length(z$height)
for (i in 1:n_boot) {
  hboot[[i]] <- mean(sample(z$height, n_height, replace = TRUE))
}
(ci_hboot <- quantile(hboot, probs = c(0.025, 0.975)))
##weight
wboot <- vector(length = n_boot)
n_weight <- length(z$weight)
for (i in 1:n_boot) {
  wboot[[i]] <- mean(sample(z$weight, n_weight, replace = TRUE))
}
(ci_wboot <- quantile(wboot, probs = c(0.025, 0.975)))
##zkilled
zboot <- vector(length = n_boot)
n_zk <- length(z$zombies_killed)
for (i in 1:n_boot) {
  zboot[[i]] <- mean(sample(z$zombies_killed, n_zk, replace = TRUE))
}
(ci_zboot <- quantile(zboot, probs = c(0.025, 0.975)))
##ed
edboot <- vector(length = n_boot)
n_ed <- length(z$years_of_education)
for (i in 1:n_boot) {
  edboot[[i]] <- mean(sample(z$years_of_education, n_ed, replace = TRUE))
}
(ci_edboot <- quantile(edboot, probs = c(0.025, 0.975)))
