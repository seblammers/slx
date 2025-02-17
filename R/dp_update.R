# if we remove a subject or a video we can update our params here
# to obtain how many trials are left
# we can use that to check if our df has the same number of rows with nrow()

# note that you have to adapt the function for your
# project!
# this serves as an introductory example and code skeleton
dp_update <- function(df = NULL,
                      n_subjects = NULL, # 11
                      # define stimulus levels
                      n_vel_levels = NULL, # 3: fast, medium, slow
                      n_standards = NULL, # 2: human, square
                      n_standard_types = NULL, # 2: moving, still
                      n_difference_type = NULL, # 2: shorter, longer
                      n_difficulty_levels = NULL, # 3: 0.5, 1, 1.5 (can be shorter or longer)
                      setup = FALSE) {
  if (setup) {
    df <- tibble(
      # put the basic parameters inside
      n_subjects = n_subjects,
      n_vel_levels = n_vel_levels,
      n_standards = n_standards,
      n_standard_types = n_standard_types,
      n_difference_type = n_difference_type,
      n_difficulty_levels = n_difficulty_levels,
      n_repetitions = 2, # 2: all distinct trial constellations appear twice
    )
  }

  # here is where the magic happens
  # the functions determines if new input is provided, and
  # if so 1) overwrites the basic parameters and then
  # recomputes the derived parameters

  # this is only reasonable if NOT in setup-mode
  if (!setup) {
    # fetch all function-arguments
    args_list <- as.list(match.call())

    # fetch the names of function-arguments
    # (depends on what is specified in the function-call)
    args_names <- names(args_list)

    # fetch names present in our parameters
    df_names <- names(df)

    # loop through available args that match existing params
    for (arg in args_names[args_names %in% df_names]) {

      # where a value was provided as a function-argument, update existing value
      df[[arg]] <- args_list[[arg]]
    }
  }

  # raise error if base-params are not specified
  # (either during setup or based on existing dp-object)
  assertthat::assert_that(
    !is.null(df),
    msg = "Please provide existing parameters to update OR use 'setup = TRUE' to create a new dp-object."
  )

  # calculate derived-params based on base-params
  out <- df %>%
    # setup/update derived parameters
    mutate(
      n_stim = n_vel_levels * n_standards * n_standard_types * n_difference_type * n_difficulty_levels, # distinct stimuli

      n_trials_subject = n_repetitions * n_stim,
      n_trials_total = n_trials_subject * n_subjects
    )

  # returns a tibble
  out
}
