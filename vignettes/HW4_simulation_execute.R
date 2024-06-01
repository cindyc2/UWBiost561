# Define a function that encapsulates your code
#Copilot answer...


#VERSION 1 Runs all 25 trials and 2 levels of alpha and 5 trials automatically
# Define a function that encapsulates your code
# my_function <- function() {
#   set.seed(10)
#
#   imp_numbers <- 1:25 # when running test that use just 1
#   trials <- 5 # when running test that use just 1
#   alpha_vec <- c(0.5, 0.95)
#
#   # Loop over the levels
#   level_trial_list <- lapply(alpha_vec, function(alpha) {
#     cat(paste("Value of alpha:", alpha, "\n"))
#
#     # Loop over the different trials for this level
#     trial_list <- lapply(1:trials, function(trial) {
#       cat(paste("Working on trial:", trial, "\n"))
#       set.seed(trial) # To freeze the randomness of adj_mat
#
#       # Generate the data
#       data <- UWBiost561::generate_partial_clique(n = 10,
#                                                   clique_edge_density = 0.9,
#                                                   clique_fraction = 0.5)
#       adj_mat <- data$adj_mat
#
#       # Loop over the methods for this trial
#       result_list <- lapply(imp_numbers, function(imp_number) {
#         set.seed(trial) # To freeze the randomness of the method
#         cat('*')
#         result <- UWBiost561::compute_maximal_partial_clique_master(
#           adj_mat = adj_mat,
#           alpha = alpha,
#           number = imp_number,
#           time_limit = 30
#         )
#
#         return(result)
#       })
#       names(result_list) <- paste("Implementation:", imp_numbers)
#       cat("\n")
#
#       return(result_list)
#     })
#     names(trial_list) <- paste("Trial:", 1:trials)
#     cat("====\n")
#
#     return(trial_list)
#   })
#   names(level_trial_list) <- paste0("alpha:", alpha_vec)
#
#   # Save the results, alpha values, date, and session info
#   date_of_run <- Sys.time()
#   session_info <- devtools::session_info()
#   save(
#     level_trial_list,
#     alpha_vec,
#     date_of_run,
#     session_info,
#     file = "~/HW4_simulation.RData"
#   )
# }

# Call the function to execute your code
# my_function()
#
# #VERSION 2 trying to make smaller
#
#' Title
#'
#' @param num_trials is Number of Trials per implementation
#' @param imp_numbers The Implementation Number (or numbers) to use for the simulation. You can have all by using 1:25
#'
#' @return
#' @export
#'
#' @examples

my_function <- function(num_trials = 5, imp_numbers = 1:25) {
  set.seed(10)

  imp_numbers <- imp_numbers
  trials <- num_trials
  alpha_vec <- c(0.5, 0.95)

  # Loop over the levels
  level_trial_list <- lapply(alpha_vec, function(alpha) {
    cat(paste("Value of alpha:", alpha, "\n"))

    # Loop over the different trials for this level
    trial_list <- lapply(1:num_trials, function(trial) {
      cat(paste("Working on trial:", trial, "\n"))
      set.seed(trial) # To freeze the randomness of adj_mat

      # Generate the data
      data <- UWBiost561::generate_partial_clique(n = 10,
                                                  clique_edge_density = 0.9,
                                                  clique_fraction = 0.5)
      adj_mat <- data$adj_mat

      # Loop over the methods for this trial
      result_list <- lapply(imp_numbers, function(imp_number) {
        set.seed(trial) # To freeze the randomness of the method
        cat('*')
        result <- UWBiost561::compute_maximal_partial_clique_master(
          adj_mat = adj_mat,
          alpha = alpha,
          number = imp_number,
          time_limit = 30
        )

        return(result)
      })
      names(result_list) <- paste("Implementation:", imp_numbers)
      cat("\n")

      return(result_list)
    })
    names(trial_list) <- paste("Trial:", 1:num_trials)
    cat("====\n")

    return(trial_list)
  })
  names(level_trial_list) <- paste0("alpha:", alpha_vec)

  # Save the results, alpha values, date, and session info
  date_of_run <- Sys.time()
  session_info <- devtools::session_info()
  save(
    level_trial_list,
    alpha_vec,
    date_of_run,
    session_info,
    file = "~/demo_simulation.RData"
  )

  level_trial_list
}

# Call the function to execute your code
#my_function()
