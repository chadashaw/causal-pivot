library(dplyr, quietly = T, warn.conflicts = F)
library(purrr, quietly = T, warn.conflicts = F)
library(conflicted, quietly = T, warn.conflicts = F)

conflict_prefer("select", "dplyr", quiet = T)
conflict_prefer("filter", "dplyr", quiet = T)

options(dplyr.summarise.inform = F)

concat.df.list <- function(df.list, keycol = "key") {
  # concatenates a list of dataframes into a single dataframe
  # the keys of the list are added to a new column named keycol
  #
  # @param df.list  list of dataframes
  # @param  keycol  the name of the added column
  #
  # @returns  row concatenated dataframe with additional key column
  
  dfs.colnames <- df.list %>% map(~colnames(.x)) %>% unlist() %>% unique()
  
  df.all <- as.data.frame(matrix(
    numeric(),
    nrow = 0,
    ncol = length(dfs.colnames) + 1,
    dimnames = list(NULL, c(dfs.colnames, keycol))
  ))
  
  dfs <- list()
  
  for (key in names(df.list)) {
    d <- df.list[[key]]
    if (nrow(d) == 0) {
      next
    }
    d[[keycol]] = key
    dfs[[key]] <- d
  }
  
  bind_rows(dfs)
}

calc.quantile <- function(vals, n.qnt, na.rm = F) {
  # Transforms a vector of numeric values into a vector of quantiles into which
  # each corresponding value in the input vector falls.
  #
  # @param  vals  the input numeric vector
  # @param  n.qnt the number of quantiles into which we split the values
  #
  # @returns  transformed vector
  
  n.qnt <- n.qnt
  q.probs = seq(0, 1, 1 / n.qnt)

  cut(
    vals,
    breaks = quantile(
      vals,
      probs = q.probs,
      na.rm = na.rm
    ),
    labels = 1:(length(q.probs) - 1),
    include.lowest=TRUE
  ) %>%
    as.numeric()
}

save.plot <- function(my.plot, file.prefix, root.dir = getwd(), sub.dir = '', w = 7.35 * 2, h = 4.5 * 2) {
  # save plot (defaults to last_plot())
  #
  # @param filename   filename for save
  # @param plot       plot to save
  # @param dir        directory for save
  # @param w          plot width
  # @param h          plot height
  
  dir.create(file.path(root.dir, 'svg', sub.dir), showWarnings = F, recursive = T)
  dir.create(file.path(root.dir, 'png', sub.dir), showWarnings = F, recursive = T)
  dir.create(file.path(root.dir, 'pdf', sub.dir), showWarnings = F, recursive = T)
  
  ggsave(filename = file.path(root.dir, 'svg', sub.dir, paste0(file.prefix, '.svg')), plot = my.plot, width = w, height = h)
  ggsave(filename = file.path(root.dir, 'png', sub.dir, paste0(file.prefix, '.png')), plot = my.plot, width = w, height = h)
  pdf(file = file.path(root.dir, 'pdf', sub.dir, paste0(file.prefix, '.pdf')), width = w, height = h)
  print(my.plot)
  x <- dev.off()
  my.plot
}
