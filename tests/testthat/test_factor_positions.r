



context("factor positions")





tickers <- c(rep("ones", 10L), rep("tens", 10L), rep("twenties", 10L), rep("thirties", 10L), rep("forties", 10L),
             rep("fifties", 10L), rep("sixties", 10L), rep("seventies", 10L), rep("eighties", 10L), rep("nineties", 10L))
fields <- rep("uniform", 100L); dates <- rep(seq(as.Date("2018-01-01"), as.Date("2018-01-10"), by = 1L), 10L)
values <- c(runif(10L, 0L, 5L), runif(10L, 10L, 15L), runif(10L, 20L, 25L), runif(10L, 30L, 35L), runif(10L, 40L, 45L),
            runif(10L, 50L, 55L), runif(10L, 60L, 65L), runif(10L, 70L, 75L), runif(10L, 80L, 85L), runif(10L, 90L, 95L))
levels <- tibble::tibble(ticker = tickers, field = fields, date = dates, value = values)

test_that("factor_positions return right positions (levels sort)", {

  periods <- tibble::tibble(date = sort(dates)) %>% dplyr::mutate(year = lubridate::year(date), unit = lubridate::yday(date)) %>%
    dplyr::select(-date)
  names <- rep(c("nineties", "eighties", "seventies", "sixties", "fifties", "ones", "tens", "twenties", "thirties", "forties"), 10L)
  positions <- rep(c(rep("long", 5L), rep("short", 5L)), 10L)
  positions <- dplyr::mutate(periods, name = names, position = positions) %>% data.table::as.data.table()
  expect_equal(factor_positions(data = levels, update_frequency = "day", sort_variable = "uniform", sort_levels = T,
                                ranking_period = 1L, long_threshold = 0.5, short_threshold = 0.5), positions)

  periods <- tibble::tibble(date = sort(dates)) %>% dplyr::mutate(year = lubridate::year(date), unit = lubridate::yday(date)) %>%
    dplyr::select(-date)
  names <- rep(c("nineties", "eighties", "seventies", "sixties", "fifties", "ones", "tens", "twenties", "thirties", "forties"), 10L)
  positions <- rep(c(rep("long", 5L), rep("short", 5L)), 10L)
  positions <- dplyr::mutate(periods, name = names, position = positions) %>%
    dplyr::filter(! name %in% c("seventies", "sixties", "fifties", "twenties", "thirties", "forties")) %>%
    data.table::as.data.table()
  expect_equal(factor_positions(data = levels, update_frequency = "day", sort_variable = "uniform", sort_levels = T,
                                ranking_period = 1L, long_threshold = 0.8, short_threshold = 0.2), positions)

  periods <- tibble::tibble(date = sort(dates)) %>% dplyr::mutate(year = lubridate::year(date), unit = lubridate::yday(date)) %>%
    dplyr::select(-date)
  names <- rep(c("nineties", "eighties", "seventies", "sixties", "fifties", "ones", "tens", "twenties", "thirties", "forties"), 10L)
  positions <- rep(c(rep("long", 5L), rep("short", 5L)), 10L)
  positions <- dplyr::mutate(periods, name = names, position = positions) %>%
    dplyr::filter(unit != 1L, ! name %in% c("seventies", "sixties", "fifties", "twenties", "thirties", "forties")) %>%
    data.table::as.data.table()
  expect_equal(factor_positions(data = levels, update_frequency = "day", sort_variable = "uniform", sort_levels = T,
                                ranking_period = 2L, long_threshold = 0.8, short_threshold = 0.2), positions)


})



tickers <- c(rep("ones", 10L), rep("tens", 10L), rep("twenties", 10L), rep("thirties", 10L), rep("forties", 10L),
             rep("fifties", 10L), rep("sixties", 10L), rep("seventies", 10L), rep("eighties", 10L), rep("nineties", 10L))
fields <- rep("uniform", 100L); dates <- rep(seq(as.Date("2018-01-01"), as.Date("2018-01-10"), by = 1L), 10L)
values <- c(seq(1L, 10, 1), seq(1L, 20L, 2L), seq(1L, 30L, 3L), seq(1L, 40L, 4L), seq(1L, 50L, 5L),
            seq(1L, 60L, 6L), seq(1L, 70L, 7L), seq(1L, 80L, 8L), seq(1L, 90L, 9L), seq(1L, 100L, 10L))
returns <- tibble::tibble(ticker = tickers, field = fields, date = dates, value = values)

test_that("factor_positions return right positions (returns sort)", {

  periods <- tibble::tibble(date = sort(dates)) %>% dplyr::mutate(year = lubridate::year(date), unit = lubridate::yday(date)) %>%
    dplyr::select(-date)
  names <- rep(c("nineties", "eighties", "seventies", "sixties", "fifties", "ones", "tens", "twenties", "thirties", "forties"), 10L)
  positions <- rep(c(rep("long", 5L), rep("short", 5L)), 10L)
  positions <- dplyr::mutate(periods, name = names, position = positions) %>% dplyr::filter(unit != 1L) %>% data.table::as.data.table()
  expect_equal(factor_positions(data = returns, update_frequency = "day", sort_variable = "uniform", sort_levels = F,
                                ranking_period = 1L, long_threshold = 0.5, short_threshold = 0.5), positions)

  periods <- tibble::tibble(date = sort(dates)) %>% dplyr::mutate(year = lubridate::year(date), unit = lubridate::yday(date)) %>%
    dplyr::select(-date)
  names <- rep(c("nineties", "eighties", "seventies", "sixties", "fifties", "ones", "tens", "twenties", "thirties", "forties"), 10L)
  positions <- rep(c(rep("long", 5L), rep("short", 5L)), 10L)
  positions <- dplyr::mutate(periods, name = names, position = positions) %>%
    dplyr::filter(unit != 1L, ! name %in% c("seventies", "sixties", "fifties", "twenties", "thirties", "forties")) %>%
    data.table::as.data.table()
  expect_equal(factor_positions(data = returns, update_frequency = "day", sort_variable = "uniform", sort_levels = F,
                                ranking_period = 1L, long_threshold = 0.8, short_threshold = 0.2), positions)

  periods <- tibble::tibble(date = sort(dates)) %>% dplyr::mutate(year = lubridate::year(date), unit = lubridate::yday(date)) %>%
    dplyr::select(-date)
  names <- rep(c("nineties", "eighties", "seventies", "sixties", "fifties", "ones", "tens", "twenties", "thirties", "forties"), 10L)
  positions <- rep(c(rep("long", 5L), rep("short", 5L)), 10L)
  positions <- dplyr::mutate(periods, name = names, position = positions) %>%
    dplyr::filter(! unit %in% 1L:2L, ! name %in% c("seventies", "sixties", "fifties", "twenties", "thirties", "forties")) %>%
    data.table::as.data.table()
  expect_equal(factor_positions(data = returns, update_frequency = "day", sort_variable = "uniform", sort_levels = F,
                                ranking_period = 2L, long_threshold = 0.8, short_threshold = 0.2), positions)

})

