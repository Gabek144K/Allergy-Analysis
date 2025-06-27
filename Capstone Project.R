library(tidyverse)
library(readr)
library(ggplot2)
library(corrplot)

my_theme <- theme_minimal() +
  theme(
    legend.position = "right"
  )
food_allergy<- read.csv("food-allergy-analysis.csv")

## Count of Negative Numbers (This is to represent the columns with negative integers)
food_allergy %>%
  select(where(is.numeric)) %>%
    mutate(across(everything(), function(x){sum(x < 0, na.rm = TRUE)})) %>%
      distinct_all() %>%
        pivot_longer(everything(), names_to = "column", values_to = "negative_count") %>%
          filter(negative_count > 0) %>%
          ggplot(aes(fct_reorder(column, negative_count), negative_count, fill = column)) +
        geom_col(width = 1) +
      coord_flip() +
    labs(title = "Count of negative numbers", x = "Variable", y = "Count") + 
  theme_minimal() +
  theme(
    legend.position = "none"
  )

## Excluding Kids with Negative numbers from the data set
Kids_with_negative_numbers <- food_allergy %>%
  select(SUBJECT_ID, AGE_START_YEARS, ATOPIC_DERM_START, ALLERGIC_RHINITIS_START, MILK_ALG_START, MILK_ALG_END, 
    EGG_ALG_START, EGG_ALG_END, SOY_ALG_START,                                                                                                             SOY_ALG_END, ASTHMA_END, ASTHMA_START) %>%
      mutate(contains_negative_number = AGE_START_YEARS < 0 | ATOPIC_DERM_START < 0 | ALLERGIC_RHINITIS_START < 0 | 
          MILK_ALG_START < 0 | MILK_ALG_END < 0 | EGG_ALG_START < 0 | EGG_ALG_END < 0 | SOY_ALG_START< 0 | 
             SOY_ALG_END < 0 | ASTHMA_END < 0 | ASTHMA_START < 0) %>%
             filter(contains_negative_number) %>%
          pull(SUBJECT_ID)
        cat(paste0("A total ", length(Kids_with_negative_numbers), " patients have values that are less than 0\n", collapse = ""))
      cat(paste0("Number of patients before exclusion: ", nrow(food_allergy), "\n", collapse = ""))
    food_allergy1 <- food_allergy %>%
  filter(!(SUBJECT_ID %in% Kids_with_negative_numbers))
cat(paste0("Number of patients after exclusion: ", nrow(food_allergy1), "\n", collapse = ""))

library(tools)
names(food_allergy1) <- tolower(names(food_allergy1))

## Condition Count 
conditions_start_columns <- food_allergy1 %>%
    select(contains("start")) %>%
  select(-age_start_years, -age_start_years, -age_start_years) %>%
names()
food_allergy1 %>%
  select(all_of(conditions_start_columns)) %>%
    mutate(across(everything(), ~ !is.na(.x))) %>%
      pivot_longer(everything(), names_to = "condition", values_to = "diagnosed") %>%
        count(condition, diagnosed) %>%
          mutate(condition = str_remove(condition, "_start")) %>%
           mutate(condition = str_replace(condition, "_alg", " allergy")) %>%
          mutate(condition = str_replace(condition, "_derm", " dermatitis")) %>%
        mutate(condition = str_replace(condition, "_", " ")) %>%
      mutate(condition = tools::toTitleCase(condition)) %>%
    ggplot(aes(y = condition, x = n, fill = diagnosed)) +
  geom_col() + 
my_theme +
  scale_fill_discrete(name = "Diagnosed") + 
labs(title = "Conditions count", x = "Count", y = "Condition")


## Most Common Food Allegies 
most_common_food_allergies <- food_allergy1 %>% 
  select(subject_id, contains("_alg_start"), gender_factor, race_factor) %>%
    pivot_longer(c("shellfish_alg_start", "fish_alg_start", "milk_alg_start", "soy_alg_start", "egg_alg_start", "wheat_alg_start", "peanut_alg_start", "sesame_alg_start", "treenut_alg_start", "walnut_alg_start", "pecan_alg_start", "pistach_alg_start", "almond_alg_start", "brazil_alg_start", "hazelnut_alg_start", "cashew_alg_start"), names_to = "food", values_to = "age") %>%
    filter(!is.na(age)) %>%
  mutate(food = str_remove(food, "_alg_start")) %>%
mutate(food = factor(food)) 


most_common_food_allergies %>%
  count(food) %>%
    ggplot(aes(fct_reorder(food, n), n)) + 
      geom_col(position = "dodge") + 
    coord_flip() +
  my_theme +
labs(title = "Count of food allergies", x = "Food", y = "Count")


most_common_food_allergies %>%
  count(food, gender_factor) %>%
    ggplot(aes(fct_reorder(food, n), n, fill = gender_factor)) + 
      geom_col(position = "dodge") + 
      coord_flip() +
    my_theme +
  scale_fill_discrete(name = "Gender") + 
labs(title = "Count of food allergies by gender", x = "Food", y = "Count")


most_common_food_allergies %>%
  count(food, race_factor) %>%
    ggplot(aes(fct_reorder(food, n), n, fill = race_factor)) + 
      geom_col(position = "dodge") + 
      coord_flip() +
    my_theme +
  scale_fill_discrete(name = "Race") + 
labs(title = "Count of food allergies by race", x = "Food", y = "Count")


## Scatter Plot and Regression for the relationship between Food Allergy and Asthma Allergy start

## Milk
ggplot(food_allergy1, aes(x = milk_alg_start, y = asthma_start)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(
    title = "Relationship Between Milk Allergy Onset and Asthma Onset",
    x = "Milk Allergy Start (Years)",
    y = "Asthma Start (Years)"
  ) +
  my_theme
cor(food_allergy1$milk_alg_start, food_allergy1$asthma_start, use = "complete.obs")

## Soy
ggplot(food_allergy1, aes(x = soy_alg_start, y = asthma_start)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(
    title = "Relationship Between Soy Allergy Onset and Asthma Onset",
    x = "Soy Allergy Start (Years)",
    y = "Asthma Start (Years)"
  ) +
  my_theme
cor(food_allergy1$soy_alg_start, food_allergy1$asthma_start, use = "complete.obs")

## Egg
ggplot(food_allergy1, aes(x = egg_alg_start, y = asthma_start)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(
    title = "Relationship Between Egg Allergy Onset and Asthma Onset",
    x = "Egg Allergy Start (Years)",
    y = "Asthma Start (Years)"
  ) +
  my_theme
cor(food_allergy1$egg_alg_start, food_allergy1$asthma_start, use = "complete.obs")

## Wheat
ggplot(food_allergy1, aes(x = wheat_alg_start, y = asthma_start)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(
    title = "Relationship Between Wheat Allergy Onset and Asthma Onset",
    x = "Wheat Allergy Start (Years)",
    y = "Asthma Start (Years)"
  ) +
  my_theme
cor(food_allergy1$wheat_alg_start, food_allergy1$asthma_start, use = "complete.obs")

## Peanut 
ggplot(food_allergy1, aes(x = peanut_alg_start, y = asthma_start)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(
    title = "Relationship Between Peanut Allergy Onset and Asthma Onset",
    x = "Peanut Allergy Start (Years)",
    y = "Asthma Start (Years)"
  ) +
  my_theme
cor(food_allergy1$peanut_alg_start, food_allergy1$asthma_start, use = "complete.obs")

## Walnut
ggplot(food_allergy1, aes(x = walnut_alg_start, y = asthma_start)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(
    title = "Relationship Between Walnut Allergy Onset and Asthma Onset",
    x = "Walnut Allergy Start (Years)",
    y = "Asthma Start (Years)"
  ) +
  my_theme
cor(food_allergy1$walnut_alg_start, food_allergy1$asthma_start, use = "complete.obs")

## Almond
ggplot(food_allergy1, aes(x = almond_alg_start, y = asthma_start)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(
    title = "Relationship Between Almond Allergy Onset and Asthma Onset",
    x = "Almond Allergy Start (Years)",
    y = "Asthma Start (Years)"
  ) +
  my_theme
cor(food_allergy1$almond_alg_start, food_allergy1$asthma_start, use = "complete.obs")

## Hazelnut
ggplot(food_allergy1, aes(x = hazelnut_alg_start, y = asthma_start)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(
    title = "Relationship Between Hazelnut Allergy Onset and Asthma Onset",
    x = "Hazelnut Allergy Start (Years)",
    y = "Asthma Start (Years)"
  ) +
  my_theme
cor(food_allergy1$hazelnut_alg_start, food_allergy1$asthma_start, use = "complete.obs")


## Regression Model
model <- lm(asthma_start ~ milk_alg_start + soy_alg_start, data = food_allergy1)
summary(model)

model1 <- lm(asthma_start ~ egg_alg_start + wheat_alg_start, data = food_allergy1)
summary(model1)

model2 <- lm(asthma_start ~ peanut_alg_start + walnut_alg_start, data = food_allergy1)
summary(model2)

model3 <- lm(asthma_start ~ almond_alg_start + hazelnut_alg_start, data = food_allergy1)
summary(model3)

model4<-lm(milk_alg_start ~ asthma_start + soy_alg_start, data=food_allergy1)
summary(model4)

model5 <- lm(asthma_start ~ almond_alg_start + hazelnut_alg_start + as.factor(payer_factor), data = food_allergy1)
summary(model5)

model6 <- lm(asthma_start ~ milk_alg_start + soy_alg_start + as.factor(payer_factor), data = food_allergy1)
summary(model6)

huxtable<-huxreg(model,model1,model2,model3,model5,model6,stars=c(`***`=0.01,`**`=0.05,`*`=0.1))
quick_xlsx(huxtable,"project.xlsx")

