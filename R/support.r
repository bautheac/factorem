
check_params <- function(name = NULL, data = NULL, sort_variable = NULL, sort_variables = NULL,
                         price_variable = NULL, price_variables = NULL, update_frequency = NULL,
                         return_frequency = NULL,  ranking_period = NULL, long_threshold = NULL,
                         short_threshold = NULL, sort_levels = NULL, geometric = NULL,
                         front = NULL, back = NULL, positions = NULL, long = NULL){

  if(! all(is.null(name), rlang::is_scalar_character(name)))
    stop("Parameter 'name' must be supplied as a scalar character vector.")

  if(! all(is.null(data), is.data.frame(data)))
    stop("Parameter 'data' must be supplied as a dataframe.")

  if(! all(is.null(sort_variable), rlang::is_scalar_character(sort_variable),
           sort_variable %in% sort_variables))
    stop("The parameter 'sort_variable' must be supplied as scalar character vector; one of '",
       paste(sort_variables, collapse = "', '"), "'.")

  if(! all(is.null(price_variable), rlang::is_scalar_character(price_variable),
           price_variable %in% price_variables))
    stop("The parameter 'price_variable' must be supplied as scalar character vector; one of '",
         paste(price_variables, collapse = "', '"), "'.")

  if(! all(is.null(update_frequency), rlang::is_scalar_character(update_frequency),
           update_frequency %in% c("year", "semester", "quarter", "month", "week", "day")))
      stop("Parameter 'update_frequency' must be supplied as a scalar character vector;
            one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'.")

  if(! all(is.null(return_frequency), rlang::is_scalar_character(return_frequency),
     return_frequency %in% c("year", "semester", "quarter", "month", "week", "day")))
      stop("Parameter 'return_frequency' must be supplied as a scalar character vector;
            one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'.")

  if(! all(is.null(ranking_period), rlang::is_scalar_integer(ranking_period)))
      stop("Parameter 'ranking_period' must be supplied as a scalar integeter vector.")

  if(! all(is.null(long_threshold), is.null(short_threshold),
           rlang::is_scalar_double(long_threshold), rlang::is_scalar_double(short_threshold),
           c(long_threshold, short_threshold) >= 0L, c(long_threshold, short_threshold) <= 1L))
      stop("Parameters 'long_threshold' & 'short_threshold' must be supplied as a scalar numeric
            vector; values must be between 0 and 1.")

  if(! all(is.null(sort_levels), rlang::is_scalar_logical(sort_levels)))
    stop("Parameter 'sort_levels' must be supplied as a scalar logical vector (TRUE or FALSE).")

  if(! all(is.null(geometric), rlang::is_scalar_logical(geometric)))
    stop("Parameter 'geometric' must be supplied as a scalar logical vector (TRUE or FALSE).")

  if(! all(is.null(front), is.null(back),
           rlang::is_scalar_integer(front), rlang::is_scalar_integer(back),
           c(front, back) %in% positions, front > back))
    stop("The parameters 'front' & 'back' must be supplied as scalar integer vectors; one of ",
         paste(positions, collapse = ", "), ", where front > back.")

  if(! all(is.null(long), rlang::is_scalar_logical(long)))
    stop("Parameter 'long' must be supplied as a scalar logical vector (TRUE or FALSE).")
}



