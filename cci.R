# CCI calculator --------------------------------------------------------------
#
# - Uses 5 metrics to calculate an overall crossfit score
# - metrics
#   - brute strength (bench, squat)
#   - skill (snatch, mu)
#   - cardio (mile time, fran time)
#   - physics (age, gender, height)
#
# -----------------------------------------------------------------------------

# Libraries -------------------------------------------------------------------

library(tidyr)
library(dplyr)
library(ggplot2)
library(ggradar)

# -----------------------------------------------------------------------------



# Helpers ---------------------------------------------------------------------

split_time <- function(time) {
  time_list <- stringr::str_split(time, pattern = ":")
  return(time_list)
}

time_to_seconds <- function(time) {
  min_sec <- split_time(time = time)
  minutes <- as.numeric(min_sec[[1]][1])
  seconds <- as.numeric(min_sec[[1]][2])
  total_seconds <- (minutes * 60) + seconds
  return(total_seconds)
}

time_to_seconds("6:32")

# -----------------------------------------------------------------------------






# Physics ---------------------------------------------------------------------

physics_coef <- function(age, sex = "male", height, weight) {
  froning <- 195 / 69
  camille <- 130 / 62

  if (sex != "male") {
    proto_physics <- camille
  } else {
    proto_physics <- froning
  }

  my_physics <- weight / height
  
  start <- 100
  age_penalty <- abs(27 - age)
  body_type_penalty <- abs(proto_physics - my_physics) * 10
  
  physics_total <- (start - age_penalty - body_type_penalty) / 100

  return(physics_total)
}

physics_coef(age = 25, sex = "female", height = 65, weight = 155)

# -----------------------------------------------------------------------------





# Bruteness -------------------------------------------------------------------

brute_coef <- function(bench = 200, squat = 300, physics) {
  brute_total <- ((bench * 1.1) + (squat * 1.2)) / 1000
  brute_out <- brute_total + (physics * -0.1)
  if (brute_out >= 1) {
    return(1)
  }
  return(brute_out)
}

brute_coef(365, 365, 0.8)


# -----------------------------------------------------------------------------



# Skill -----------------------------------------------------------------------

skill_coef <- function(snatch, mu, weight = 195) {
  snatch_ratio <- snatch / weight
  mu_ratio <- mu / weight
  skill_total <- ((snatch_ratio * 0.5) + (mu_ratio * 0.5))
  return(skill_total)
}

skill_coef(150, 10, 217)
skill_coef(250, 10, 217)
skill_coef(305, 30, 195)


# -----------------------------------------------------------------------------




# Cardio ----------------------------------------------------------------------

cardio_coef <- function(mile, fran, weight = 195) {
  mile_time <- time_to_seconds(mile)
  fran_time <- time_to_seconds(fran)
  mile_beta <- (mile_time + (weight / 100) + 1)
  fran_beta <- (fran_time + (weight / 100) + 1)
  cardio_total <- ((1000 - (mile_beta + fran_beta)) * 1.7) / 1000
  return(cardio_total)
}

cardio_coef(mile = "5:30", fran = "6:13")

# -----------------------------------------------------------------------------


# CCI -------------------------------------------------------------------------

cci_breakdown <- function(age, height, weight, sex, bench, squat, snatch, mu, 
                          mile, fran) {
  ph <- physics_coef(age = age, sex = sex, height = height, weight = weight)
  br <- brute_coef(bench = bench, squat = squat, physics = ph)
  sk <- skill_coef(snatch = snatch, mu = mu, weight = weight)
  ca <- cardio_coef(mile = mile, fran = fran, weight = weight)

  cci_out <- data.frame(physics = ph, brute = br, skill = sk, cardio = ca)
  return(cci_out)

}

cci_score <- function(age, height, weight, sex, bench, squat, snatch, mu, mile, fran) {
  breakdown <- cci_breakdown(
    age = age, height = height, weight = weight, sex = sex, 
    bench = bench, squat = squat, snatch = snatch, mu = mu, 
    mile = mile, fran = fran)
  score_out <- gather(breakdown, metric, score) %>% 
    summarize(score = mean(score))
  return(score_out)
}

cci_breakdown(age = 36, height = 75, weight = 217, sex = "male", bench = 335, 
    squat = 365, snatch = 250, mu = 5, mile = "6:32", fran = "4:30")

cci_score(age = 36, height = 75, weight = 217, sex = "male", bench = 335, 
          squat = 365, snatch = 250, mu = 5, mile = "6:32", fran = "4:30")

# -----------------------------------------------------------------------------







# Test ------------------------------------------------------------------------


bind_rows(
  cci_breakdown(age = 29, height = 69, weight = 195, sex = "male", bench = 400, 
      squat = 475, snatch = 305, mu = 30, mile = "5:32", fran = "2:13") %>% 
    as_tibble(.), 
  cci_breakdown(age = 36, height = 75, weight = 217, sex = "male", bench = 335, 
      squat = 365, snatch = 250, mu = 5, mile = "6:32", fran = "4:30") %>% 
    as_tibble(.), 
  cci_breakdown(age = 25, height = 60, weight = 115, sex = "female", bench = 132, 
      squat = 220, snatch = 105, mu = 3, mile = "6:13", fran = "4:34") %>% 
    as_tibble(.), 
  cci_breakdown(age = 25, height = 71, weight = 207, sex = "male", bench = 275, 
      squat = 315, snatch = 115, mu = 0, mile = "8:10", fran = "8:00") %>% 
    as_tibble(.), 
  cci_breakdown(age = 35, height = 60, weight = 132, sex = "female", bench = 130, 
      squat = 230, snatch = 75, mu = 1, mile = "8:46", fran = "7:12") %>% 
    as_tibble(.), 
  cci_breakdown(age = 39, height = 69, weight = 151, sex = "female", bench = 98, 
      squat = 150, snatch = 75, mu = 1, mile = "8:03", fran = "7:12") %>% 
    as_tibble(.), 
  cci_breakdown(age = 31, height = 68, weight = 192, sex = "male", bench = 285, 
      squat = 315, snatch = 125, mu = 2, mile = "7:37", fran = "8:34") %>% 
    as_tibble(.)) %>%
  mutate(group = c("Froning", "Joseph", "Kelly", "Sharell", "Amada", "Hot Rod", 
                   "Donovan"), 
         group = forcats::fct_relevel(group, "Froning", "Joseph", "Sharell", 
                                      "Kelly", "Amada")) %>% 
  select(group, everything()) %>% 
  ggradar()




bind_rows(
  cci_score(age = 29, height = 69, weight = 195, sex = "male", bench = 400, 
      squat = 475, snatch = 305, mu = 30, mile = "5:32", fran = "2:13") %>% 
    as_tibble(.), 
  cci_score(age = 36, height = 75, weight = 217, sex = "male", bench = 335, 
      squat = 365, snatch = 250, mu = 5, mile = "6:32", fran = "4:30") %>% 
    as_tibble(.), 
  cci_score(age = 25, height = 60, weight = 115, sex = "female", bench = 132, 
      squat = 220, snatch = 105, mu = 3, mile = "6:13", fran = "4:34") %>% 
    as_tibble(.), 
  cci_score(age = 25, height = 71, weight = 207, sex = "male", bench = 275, 
      squat = 315, snatch = 115, mu = 0, mile = "8:10", fran = "8:00") %>% 
    as_tibble(.), 
  cci_score(age = 35, height = 60, weight = 132, sex = "female", bench = 130, 
      squat = 230, snatch = 75, mu = 1, mile = "8:46", fran = "7:12") %>% 
    as_tibble(.), 
  cci_score(age = 39, height = 69, weight = 151, sex = "female", bench = 98, 
      squat = 150, snatch = 75, mu = 1, mile = "8:03", fran = "7:12") %>% 
    as_tibble(.), 
  cci_score(age = 31, height = 68, weight = 192, sex = "male", bench = 285, 
      squat = 315, snatch = 125, mu = 2, mile = "7:37", fran = "8:34") %>% 
    as_tibble(.)) %>%
    mutate(group = c("Froning", "Joseph", "Kelly", "Sharell", "Amada", "Hot Rod", 
                   "Donovan"), 
         group = forcats::fct_relevel(group, "Froning", "Joseph", "Sharell", 
                                      "Kelly", "Amada"))

# -----------------------------------------------------------------------------
