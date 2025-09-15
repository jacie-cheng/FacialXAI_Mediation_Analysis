rm(list = ls())

library(boot)
library(manymome)
library(lavaan)
library(modelr)

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
total_run = 1000
set.seed(42)

get_wanted_path = function(coef_list, path_names){
    wanted_path = list ()
    for (i in 1:length(path_names)){ 
        this_path = coef_list [ path_names [[i]] ]
        this_path = prod ( this_path ) # ! multiply all the coef. 
        # print(this_path)
        wanted_path [[i]] = this_path # save output 
    }
    return (wanted_path)
}

bootstrap_data = function(dataframe) {
    output_data <- data.frame()
    length_q <- unique(dataframe$question)
    for (k in seq_along(length_q)) {
        df <- subset(dataframe, question == length_q[k])
        bootstrapped_df = as.data.frame(resample_bootstrap(df))
        output_data <- rbind(output_data, bootstrapped_df)
    }
    return (output_data)
}

get_coefficients = function(bootstrapped_data) {
    bootstrap_fit1 <- sem(mod, bootstrapped_data, fixed.x = TRUE)
    bootstrap_fit1_coef = coef(bootstrap_fit1)

    bootstrap_fit3 <- glm(mod2, family = "binomial", data = bootstrapped_data)
    bootstrap_fit3_coef = coef(bootstrap_fit3)
    names(bootstrap_fit3_coef) <- paste0("Follows_AI~",names(bootstrap_fit3_coef))

    warning1 <- tryCatch(glm(mod2, family = "binomial", data = bootstrapped_correct),  warning=function(w) w)

    if (class(warning1)[1] == "simpleWarning") {
        return(class(warning1)[1])
    } else {
        bootstrap_correct_coef = c(bootstrap_fit1_coef, bootstrap_fit3_coef)
        return(bootstrap_correct_coef)
    }
}

calculate_paths <- function(coef_list, path_names){
    random_path_output = list()
    for (j in 1:length(path_names)) {
        random_output = get_wanted_path ( coef_list , path_names[[j]])
        random_path_output [[j]] = random_output
    }
    return(random_path_output)
}

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
    bootstrapped_correct_coef_list = get_coefficients(bootstrapped_correct)
    bootstrapped_incorrect_coef_list = get_coefficients(bootstrapped_incorrect)

    ## check for warnings, calculate path outputs, add to output list - CORRECT
    if (bootstrapped_correct_coef_list[1] == "simpleWarning") {
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
    } else {
        row_counter_incorrect = row_counter_incorrect + 1
        names(bootstrapped_incorrect_coef_list) <- paste0(names(bootstrapped_incorrect_coef_list),".g2")
        random_path_output = calculate_paths(bootstrapped_incorrect_coef_list, incorrect_path_names)
        random_data_inc[[row_counter_incorrect]] = random_path_output
    }
}

# # ---------------------------------------------------------------------------- #

## calc original values and get pathnames
## calc original paths

get_path_values = function(path_names, coef_list){
    original_output_list = list()
    for (i in 1:length(path_names)) {
        original_output = get_wanted_path ( coef_list , path_names[[i]])
        original_output_list [[i]] = original_output
    }
    return(original_output_list)
}

get_path_names = function(path_names){
    subpaths_list = list()
    for (j in 1:length(path_names)){
        for (k in 1:length(path_names[[j]])){
            subpath_name = paste(unlist(path_names[[j]][[k]]), collapse = ", ") 
            subpaths_list[[ length(subpaths_list) + 1 ]] = subpath_name
        }
    } 
    return(subpaths_list)
}

correct_original_path_values = get_path_values(correct_path_names, correct_coef_list)
correct_original_path_names = get_path_names(correct_path_names)
incorrect_original_path_values = get_path_values(incorrect_path_names, incorrect_coef_list)
incorrect_original_path_names = get_path_names(incorrect_path_names)

# # ---------------------------------------------------------------------------- #

## remove outliers
remove_outliers = function(matrix, counter) {
    ## make matrix
    matrix <- matrix(unlist(matrix), nrow = counter, byrow = TRUE)
    cat("Original matrix size:", dim(matrix))
    ## get IQR
    Q1 <- apply(matrix, 2, quantile, probs = 0.25)
    Q3 <- apply(matrix, 2, quantile, probs = 0.75)
    IQRs <- Q3 - Q1
    ## flag and remove
    outlier_mask <- sweep(matrix, 2, Q3 + 1.5 * IQRs, FUN = "<") & sweep(matrix, 2, Q1 - 1.5 * IQRs, FUN = ">")
    rows_to_keep <- apply(outlier_mask, 1, all)
    matrix <- matrix[rows_to_keep, ]
    return(matrix)
}

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
names(incorrect_original_path_values_unlisted) <- incorrect_original_path_names 

# # ---------------------------------------------------------------------------- #

print_direct_and_indirect_effects <- function(subpaths_list, my_original_output, sd_list){
    for (i in (1:length(subpaths_list))) {
        upper_ci <- my_original_output[[i]] + 1.96*sd_list[[i]]
        lower_ci <- my_original_output[[i]] - 1.96*sd_list[[i]]
        print(subpaths_list[[i]])
        cat(my_original_output[[i]], "sd:", sd_list[[i]], "[", lower_ci, ", ", upper_ci, "]", "\n")
        cat("original regression coef", my_original_output[[i]], "\n")
        cat("bootstrap sd", sd_list[[i]], "\n")
        cat("bootstrap upper ci", upper_ci, "\n")
        cat("bootstrap lower ci", lower_ci, "\n")
    }
}

print_total_effects <- function(path_names, original_output_list, output_of_random_data) {
    bootstapped_total_effects = list()
    for (i in 1:length(original_output_list)) {
        flat_list = unlist(original_output_list[[i]])
        total_effect = sum(flat_list)
        bootstapped_total_effects[[i]] = total_effect
    }
    names(bootstapped_total_effects) = names(path_names)

    ## total flow confidence interval
    bootstapped_total_effects_ci = list()
    for (i in 1:length(output_of_random_data)) {
        bootstapped_total_effects_ci[[i]] = list()
        for (j in 1:length(output_of_random_data[[i]])) {
            flat_list = unlist(output_of_random_data[[i]][[j]])
            total_effect = sum(flat_list)
            bootstapped_total_effects_ci[[i]][[j]] = total_effect
        }
    }
    for (i in (1:length(bootstapped_total_effects_ci[[1]]))) {
        values <- sapply(bootstapped_total_effects_ci, `[[`, i)
        total_sd <- sd(values)
        print(names(path_names)[[i]])
        cat("original regression coef", bootstapped_total_effects[[i]], "\n")
        cat("bootstrap sd", total_sd, "\n")
        upper_ci <- bootstapped_total_effects[[i]] + 1.96*total_sd
        cat("bootstrap upper ci", upper_ci, "\n")
        lower_ci <- bootstapped_total_effects[[i]] - 1.96*total_sd
        cat("bootstrap lower ci", lower_ci, "\n")
        cat(bootstapped_total_effects[[i]], "sd:", total_sd, "[", lower_ci, ", ", upper_ci, "]", "\n")
    }
}

print("DIRECT AND INDIRECT EFFECTS")
print_direct_and_indirect_effects(correct_original_path_names, correct_original_path_values_unlisted, correct_sd)
print_direct_and_indirect_effects(incorrect_original_path_names, incorrect_original_path_values_unlisted, incorrect_sd)

print("TOTAL EFFECTS")
print_total_effects(correct_path_names, correct_original_path_values, random_data_c)
print_total_effects(incorrect_path_names, incorrect_original_path_values, random_data_inc)

# #####################################

# sink()