rm(list = ls())

library(boot)
library(manymome)
library(lavaan)
library(modelr)

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

    warning1 <- tryCatch(glm(mod2, family = "binomial", data = bootstrapped_data),  warning=function(w) w)

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

print_direct_and_indirect_effects <- function(subpaths_list, my_original_output, sd_list){
    for (i in (1:length(subpaths_list))) {
        upper_ci <- my_original_output[[i]] + 1.96*sd_list[[i]]
        lower_ci <- my_original_output[[i]] - 1.96*sd_list[[i]]
        print(subpaths_list[[i]])
        cat(round(my_original_output[[i]],3), "sd:", round(sd_list[[i]],3), "[", round(lower_ci,3), ", ", round(upper_ci,3), "]", "\n")
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
        cat(round(bootstapped_total_effects[[i]],3), "sd:", round(total_sd,3), "[",round(lower_ci,3),", ",round(upper_ci,3),"]", "\n")
    }
}
