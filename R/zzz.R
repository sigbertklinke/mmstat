.onLoad <- function(libname, pkgname) {
	op <- options()
	op.mmstat <- list(
		mmstat.path = system.file(package = "mmstat"),
		mmstat.desc.author = '"Sigbert Klinke <sigbert@hu-berlin.de> [aut, cre]"',
		mmstat.desc.license = "GPL-2",
		mmstat.desc.suggests = NULL,
		mmstat.col   = c(daquamarine="#1B9E77",  dorange="#D95F02",   dblue="#7570B3",   dpink="#E7298A", 	
										 dgreen="#66A61E",     	dyellow="#E6AB02", 	dgold="#A6761D", 	dgray="#666666",
										 laquamarine="#66C2A5",  lorange="#FC8D62",  lblue="#8DA0CB",  lpink="#E78AC3", 	
										 lgreen="#A6D854",     	lyellow="#FFD92F", 	lgold="#E5C494", 	lgray="#B3B3B3"),
		mmstat.alpha = c(0.1, 0.25, 0.5, 1, 2.5, 5, 10, 20)
	)
	op.mmstat$mmstat.col.population <- op.mmstat$mmstat.col[1:8]
	op.mmstat$mmstat.col.sample     <- op.mmstat$mmstat.col[9:16]
	toset <- !(names(op.mmstat) %in% names(op))
	if(any(toset)) options(op.mmstat[toset])
	invisible()
}