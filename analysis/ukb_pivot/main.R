
rmarkdown::render("setup.Rmd", envir = globalenv(), output_dir = output_dir, intermediates_dir = intermediates_dir)
rmarkdown::render("rv.summaries.Rmd", envir = globalenv(), output_dir = output_dir, intermediates_dir = intermediates_dir)
rmarkdown::render("lrt.Rmd", envir = globalenv(), output_dir = output_dir, intermediates_dir = intermediates_dir)
rmarkdown::render("liabilityG.lrt.Rmd", envir = globalenv(), output_dir = output_dir, intermediates_dir = intermediates_dir)
rmarkdown::render("collider.plots.Rmd", envir = globalenv(), output_dir = output_dir, intermediates_dir = intermediates_dir)
rmarkdown::render("ancestry.control.Rmd", envir = globalenv(), output_dir = output_dir, intermediates_dir = intermediates_dir)



