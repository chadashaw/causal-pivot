library(rootSolve)

extract.args.for.function <- function(f, args.list) {
  formal.args <- names(formals(f))
  args.list[names(args.list) %in% formal.args]
}

f.call <- function(f, args.list, envir = parent.frame(), force.incl = c()) {
  args.list <- modifyList(
    extract.args.for.function(f, args.list),
    args.list[names(args.list) %in% force.incl]
  )
  do.call(f, args.list, envir = envir)
}

expit <- function(logit) {
  exp(logit) / (1 + exp(logit))
}

mle.root <- function(f, f.params, start, maxiter=50) {
  root.params <- list(
    f = f,
    start = start,
    maxiter = maxiter
  )

  root.result <- f.call(multiroot, c(f.params, root.params), force.incl = names(f.params))
  c(root.result$root, root.result$iter)
}

mle.optim <- function(f, d, f.params, start, maxit=100) {
  optim.params <- list(
    par = start,
    fn = f,
    gr = d,
    method = "BFGS",
    control = list(
      maxit = maxit,
      fnscale = -1
    )
  )
  
  f.call(optim, c(f.params, optim.params), force.incl = names(f.params))
}

lrt <- function(f.ll, ll.params) {
  # calculate log liklihood
  ll <- f.call(f.ll, ll.params)
  # calculate liklihood ratio test statistic
  # chi2 <- -2 * ll
  chi2 <- 2 * ll
  # return p.value
  pchisq(chi2, df = 2, lower.tail = F)
}

save.plot <- function(file.prefix, my.plot = last_plot(), out.dir = getwd(), w = 7.35 * 2, h = 4.5 * 2) {
  # save plot (defaults to last_plot()) to configured PLOTS_DIR
  #
  # @param filename   filename for save
  # @param plot       plot to save
  # @param dir        directory for save
  # @param w          plot width
  # @param h          plot height
  
  
  dir.create(file.path(out.dir, 'svg'), showWarnings = F, recursive = T)
  dir.create(file.path(out.dir, 'png'), showWarnings = F, recursive = T)
  dir.create(file.path(out.dir, 'pdf'), showWarnings = F, recursive = T)
  
  ggsave(filename = file.path(out.dir, 'svg', paste0(file.prefix, '.svg')), plot = my.plot, w = w, h = h)
  ggsave(filename = file.path(out.dir, 'png', paste0(file.prefix, '.png')), plot = my.plot, w = w, h = h)
  pdf(file = file.path(out.dir, 'pdf', paste0(file.prefix, '.pdf')), width = w, height = h)
  print(my.plot)
  x <- dev.off();
}