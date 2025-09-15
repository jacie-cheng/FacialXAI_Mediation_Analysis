rm(list = ls())

library(boot)
library(manymome)
library(lavaan)
library(modelr)
source("bootstrapping_functions.r")

### USER INPUTS ###

input_data <- read.csv('raw_combined_ai_xai.csv')

## DATA ##
input_data <- subset(input_data, Follows_AI != 0)
input_data$Follows_AI[input_data$Follows_AI == -1] <- 0
input_data <- subset(input_data, question != 9)
incorrect_data <- subset(input_data, Classifier_Acc == "Incorrect")
correct_data <- subset(input_data, Classifier_Acc == "Correct")
file_name <- "output/xai_presence_cohort.txt"
sink(file_name)

## FITTING THE MODELS ##
# sem
mod <- 
"
Q4 ~ Q2 + XAI_Presence
"
fit1 <- sem(mod, correct_data, fixed.x = TRUE)
fit1_coef <- coef(fit1)
names(fit1_coef) <- paste0(names(fit1_coef), ".g1")

fit2 <- sem(mod, incorrect_data, fixed.x = TRUE)
fit2_coef <- coef(fit2)
names(fit2_coef) <- paste0(names(fit2_coef), ".g2")

# glm
mod2 <- "Follows_AI ~ Q2 + XAI_Presence + Q4"

fit3 <- glm(mod2, family = "binomial", data = correct_data)
fit3_coef = coef(fit3)
names(fit3_coef) <- paste0("Follows_AI~",names(fit3_coef),".g1")

fit4 <- glm(mod2, family = "binomial", data = incorrect_data)
fit4_coef = coef(fit4)
names(fit4_coef) <- paste0("Follows_AI~",names(fit4_coef),".g2")

summary(fit1)
summary(fit3)
summary(fit2)
summary(fit4)

correct_coef_list = c(fit1_coef, fit3_coef)
incorrect_coef_list = c(fit2_coef, fit4_coef)

# ---------------------------------------------------------------------------- #

# path names to get coeff
correct_path_names = list ()
incorrect_path_names = list ()
##
correct_path_names[["Correct.Follows_AI~Q2"]][[1]] <- c("Follows_AI~Q2.g1")
correct_path_names[["Correct.Follows_AI~Q2"]][[2]] <- c("Q4~Q2.g1", "Follows_AI~Q4.g1")

incorrect_path_names[["Incorrect.Follows_AI~Q2"]][[1]] <- c("Follows_AI~Q2.g2")
incorrect_path_names[["Incorrect.Follows_AI~Q2"]][[2]] <- c("Q4~Q2.g2", "Follows_AI~Q4.g2")

## 
correct_path_names[["Correct.Follows_AI~XAI_Presence"]][[1]] <- c("Follows_AI~XAI_Presence.g1")
correct_path_names[["Correct.Follows_AI~XAI_Presence"]][[2]] <- c("Q4~XAI_Presence.g1", "Follows_AI~Q4.g1")

incorrect_path_names[["Incorrect.Follows_AI~XAI_Presence"]][[1]] <- c("Follows_AI~XAI_Presence.g2")
incorrect_path_names[["Incorrect.Follows_AI~XAI_Presence"]][[2]] <- c("Q4~XAI_Presence.g2", "Follows_AI~Q4.g2")


# ---------------------------------------------------------------------------- #

## run bootstrapping
total_run = 1000
set.seed(42)

random_data_c = list()
random_data_inc = list()
warning_correct = 0
warning_incorrect = 0
row_counter_correct = 0
row_counter_incorrect = 0

for (i in 1:total_run ){
    ## bootstrapping
    bootstrapped_correct = bootstrap_data(correct_data)
    bootstrapped_incorrect = bootstrap_data(incorrect_data)

    ## re-run model
    bootstrapped_correct_coef_list = try(get_coefficients(bootstrapped_correct))
    bootstrapped_incorrect_coef_list = try(get_coefficients(bootstrapped_incorrect))

    ## check for warnings, calculate path outputs, add to output list - CORRECT
    if (bootstrapped_correct_coef_list[1] == "simpleWarning") {
        warning_correct = warning_correct + 1
    } else if (class(bootstrapped_correct_coef_list) == "try-error"){
        warning_correct = warning_correct + 1
    } else {
        row_counter_correct = row_counter_correct + 1
        names(bootstrapped_correct_coef_list) <- paste0(names(bootstrapped_correct_coef_list),".g1")
        random_path_output = calculate_paths(bootstrapped_correct_coef_list, correct_path_names)
        random_data_c[[row_counter_correct]] = random_path_output
    }

    ## check for warnings, calculate path outputs, add to output list - INCORRECT
    if (bootstrapped_incorrect_coef_list[1] == "simpleWarning") {
        warning_incorrect = warning_incorrect + 1
    } else if (class(bootstrapped_incorrect_coef_list) == "try-error"){
        warning_incorrect = warning_incorrect + 1
    } else {
        row_counter_incorrect = row_counter_incorrect + 1
        names(bootstrapped_incorrect_coef_list) <- paste0(names(bootstrapped_incorrect_coef_list),".g2")
        random_path_output = calculate_paths(bootstrapped_incorrect_coef_list, incorrect_path_names)
        random_data_inc[[row_counter_incorrect]] = random_path_output
    }
}

## get original path values
correct_original_path_values = get_path_values(correct_path_names, correct_coef_list)
correct_original_path_names = get_path_names(correct_path_names)
incorrect_original_path_values = get_path_values(incorrect_path_names, incorrect_coef_list)
incorrect_original_path_names = get_path_names(incorrect_path_names)

correct_matrix = remove_outliers(random_data_c, row_counter_correct)
incorrect_matrix = remove_outliers(random_data_inc, row_counter_incorrect)

## calc bootstrap sd for each subpath
correct_sd <- apply(correct_matrix, 2, FUN = sd)
names(correct_sd) <- correct_original_path_names
correct_original_path_values_unlisted = unlist(correct_original_path_values)
names(correct_original_path_values_unlisted) <- correct_original_path_names 

incorrect_sd <- apply(incorrect_matrix, 2, FUN = sd)
names(incorrect_sd) <- incorrect_original_path_names
incorrect_original_path_values_unlisted = unlist(incorrect_original_path_values)
names(incorrect_original_path_values_unlisted) <- 

print("DIRECT AND INDIRECT EFFECTS")
print_direct_and_indirect_effects(correct_original_path_names, correct_original_path_values_unlisted, correct_sd)
print_direct_and_indirect_effects(incorrect_original_path_names, incorrect_original_path_values_unlisted, incorrect_sd)

print("TOTAL EFFECTS")
print_total_effects(correct_path_names, correct_original_path_values, random_data_c)
print_total_effects(incorrect_path_names, incorrect_original_path_values, random_data_inc)

# #####################################

sink() 