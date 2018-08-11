cumulative_return <- function (data, geometric = TRUE)
{
  if (is.vector(data)) {
    data = na.omit(data)
    if (!geometric)
      return(sum(data))
    else {
      return(prod(1L + data) - 1L)
    }
  }
  else {
    data = checkData(data, method = "matrix")
    result = apply(data, 2L, cumulative_return, geometric = geometric)
    dim(result) = c(1L, NCOL(data))
    colnames(result) = colnames(data)
    rownames(result) = "cumulative return"
    return(result)
  }
}

calendar_returns <- function (data, digits = 1L, as.perc = TRUE, geometric = TRUE)
{
  ri = checkData(data, method = "zoo")
  columns = ncol(ri)
  columnnames = colnames(ri)
  rownames = rownames(ri)
  firstyear = as.numeric(format(strptime(as.POSIXct(time(ri)[1L]),
                                         "%Y-%m-%d"), "%Y"))
  lastyear = as.numeric(format(strptime(as.POSIXct(time(ri)[length(ri[,
                                                                      1L])]), "%Y-%m-%d"), "%Y"))
  year = format(strptime(as.POSIXct(time(ri)), "%Y-%m-%d"),
                "%Y")
  month = format(strptime(as.POSIXct(time(ri)), "%Y-%m-%d"),
                 "%b")
  monthlabels = strftime(seq.Date(as.Date("2000-01-01"), length.out = 12L,
                                  by = "months"), format = "%b")
  rowlabels = (firstyear:lastyear)
  for (column in 1L:columns) {
    target.df = as.data.frame(matrix(data = as.numeric(NA),
                                     length(rowlabels), length(monthlabels),
                                     dimnames = list(rowlabels, monthlabels)))
    for (i in 1L:length(ri[, 1L])) {
      if (!is.na(ri[i, column])) {
        target.df[year[i], month[i]] = ri[i, column]
      }
    }
    yearcol = as.data.frame(matrix(data = as.numeric(NA),
                                   length(rowlabels), 1L,
                                   dimnames = list(rowlabels, columnnames[column])))
    for (i in 1L:length(yearcol[, 1L])) {
      if (geometric)
        yearcol[i, columnnames[column]] = prod(1L + na.omit(as.numeric(target.df[i,
                                                                                ]))) - 1L
      else yearcol[i, columnnames[column]] = sum(as.numeric(target.df[i,
                                                                      ]), na.rm = TRUE)
      if (yearcol[i, columnnames[column]] == 0L)
        yearcol[i, columnnames[column]] = NA
    }
    target.df = cbind(target.df, yearcol)
    if (as.perc)
      multiplier = 100L
    else multiplier = 1L
    target.df = target.df * multiplier
    target.df = base::round(target.df, digits)
    if (column == 1L)
      result.df = target.df
    else {
      result.df = cbind(result.df, target.df[, 13L])
    }
  }
  colnames(result.df) = c(monthlabels, columnnames)
  result.df
}


