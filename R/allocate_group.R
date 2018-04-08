
#' Allocate groups
#'
allocate_group <- function(n_groups, grouping, n) {

  if (!is.null(grouping)) {

    if (sum(grouping) == n) {
      grouping
    } else {
      stop('grouping vector supplied does not add to the number of data points')
    }

  } else {

    if (!is.null(n_groups)) {

      num_per_group <- ceiling(n/n_groups)

      c(rep(num_per_group, times = n_groups - 1),
        ifelse(n %% n_groups == 0, num_per_group, n %% num_per_group))

    } else {

      pretty_groupings(n)

    }
  }
}


pretty_groupings <- function(n, min_num_per_group = 5, max_n_groups = 7) {

  if (n < 5) {
    return(n)
  }

  num_per_group <- min_num_per_group
  n_groups <- ceiling(n/num_per_group)

  while (any(n %% num_per_group < ceiling(0.5*num_per_group) & n %% num_per_group > 0, n_groups > max_n_groups)) {

    num_per_group <- num_per_group + 1
    n_groups <- ceiling(n/num_per_group)

  }


  c(rep(num_per_group, times = n_groups - 1),
    ifelse(n %% n_groups == 0, num_per_group, n %% num_per_group))

}
