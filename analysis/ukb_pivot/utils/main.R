library(dplyr, quietly = T, warn.conflicts = F)
library(purrr, quietly = T, warn.conflicts = F)
library(ggplot2, quietly = T, warn.conflicts = F)
library(conflicted, quietly = T, warn.conflicts = F)

conflict_prefer("select", "dplyr", quiet = T)
conflict_prefer("filter", "dplyr", quiet = T)

options(dplyr.summarise.inform = F)

concat.df.list <- function(df.list, key.cols) {
  # concatenates a list of dataframes into a single dataframe
  # the keys of the list are added to a new column named keycol
  # also accepts list of objects that can be converted to dataframes
  #
  # @param df.list  list of dataframes
  # @param  keycol  the name of the added column
  #
  # @returns  row concatenated dataframe with additional key column
  
  if (length(key.cols) == 1) {
    return(
      df.list %>%
        purrr::imap(function(df, key) {
          df <- as.data.frame(df)
          if (nrow(df) == 0) {
            return(NULL)
          }
          df[[key.cols]] <- key
          df
        }) %>%
        do.call(rbind, .) %>%
        select(all_of(key.cols), everything())
    )
  }
  
  concat.df.list(purrr::map(df.list, ~ concat.df.list(.x, key.cols = key.cols[-1])), key.cols = key.cols[1])
  # dfs.colnames <- df.list %>% map(~names(.x)) %>% unlist() %>% unique()
  # 
  # df.all <- as.data.frame(matrix(
  #   numeric(),
  #   nrow = 0,
  #   ncol = length(dfs.colnames) + 1,
  #   dimnames = list(NULL, c(dfs.colnames, keycol))
  # ))
  # 
  # dfs <- list()
  # 
  # for (key in names(df.list)) {
  #   d <- as.data.frame(df.list[[key]]) # when given list of lists
  #   if (nrow(d) == 0) {
  #     next
  #   }
  #   d[[keycol]] = key
  #   dfs[[key]] <- d
  # }
  # 
  # bind_rows(dfs)
}

extract.leaf <- function(root, leaf.key) {
  # extracts a "leaf" from a nested list structure
  # useful for extracting results from another map or lapply
  # that return multiple result types via lists
  if (!is.list(root)) {
    return(NULL)
  }
  
  if (leaf.key %in% names(root)) {
    return(root[[leaf.key]])
  }
  
  map(root, ~ extract.leaf(.x, leaf.key))
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
  # dir.create(file.path(root.dir, 'png', sub.dir), showWarnings = F, recursive = T)
  dir.create(file.path(root.dir, 'pdf', sub.dir), showWarnings = F, recursive = T)
  # dir.create(file.path(root.dir, 'rds', sub.dir), showWarnings = F, recursive = T)
  
  ggsave(filename = file.path(root.dir, 'svg', sub.dir, paste0(file.prefix, '.svg')), plot = my.plot, width = w, height = h, dpi = 1000, units = "in")
  # ggsave(filename = file.path(root.dir, 'png', sub.dir, paste0(file.prefix, '.png')), plot = my.plot, width = w, height = h, dpi = 1000, units = "in")
  ggsave(filename = file.path(root.dir, 'pdf', sub.dir, paste0(file.prefix, '.pdf')), plot = my.plot, width = w, height = h, dpi = 1000, units = "in")
  # saveRDS(my.plot, file = file.path(root.dir, 'rds', sub.dir, paste0(file.prefix, '.rds')))
  my.plot
}
