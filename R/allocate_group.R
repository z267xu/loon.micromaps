
# Allocate points to rows based on grouping, number of groups (n_groups) or total number of data points (n)
# If grouping is provided, it is used
# If n_groups is provided, assign ceiling(n/n_groups) points to each of the first (n_groups - 1) groups,
# and assign the remainder points to last row
# If only n is supplied, uses pretty_groupings function
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


# Provides aesthetically pleasing allocation of points to rows/groups
# Avoids having too few points per group (min 5) or too many rows (max 7)
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
