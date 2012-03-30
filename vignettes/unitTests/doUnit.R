
#' Unit Test Vignette Functions
doUnit <- function(pkg, file){
	
	# generate the vignette for unit test on exit 
	on.exit( writeUnitVignette(pkg, paste(pkg, '-unitTests.Rnw', sep='')) )
	# load this package
	require( pkg, character.only = TRUE )
	
	## load RUnit
	runit <- "RUnit" ; require( runit, character.only = TRUE )
	if( file.exists( "unitTests-results" ) ){ unlink("unitTests-results", recursive = TRUE ) }
	dir.create( "unitTests-results" )
	
	path <- system.file("unitTests", package = pkg)
	testSuite <- defineTestSuite(name=paste(pkg, "unit testing"), dirs = path
								, rngKind = "default",
								, rngNormalKind = "default")
	tests <- runTestSuite(testSuite)
	printHTMLProtocol(tests, fileName= sprintf( "unitTests-results/%s-unitTests.html" , pkg ) )
	printTextProtocol(tests, fileName= sprintf( "unitTests-results/%s-unitTests.txt"  , pkg ) )
	
	# check for errors
	err <- getErrors(tests)
	if( err$nFail > 0) {
		stop(sprintf( "unit test problems: %d failures", err$nFail))
	}
	if( err$nErr > 0) {
		stop( sprintf( "unit test problems: %d errors", err$nErr) )
	}
	
	# copy result in tmp directory if possible
	if( file.exists( "/tmp" ) ){
		file.copy( sprintf( "unitTests-results/%s-unitTests.txt" , pkg ) , "/tmp", overwrite = TRUE )
		file.copy( sprintf( "unitTests-results/%s-unitTests.html", pkg ) , "/tmp", overwrite = TRUE )
	}
}

#' Writes the vignette that contains the results of the unit tests.
writeUnitVignette <- function(pkg, file){
	
	Rnw.template <- 
"
\\documentclass[10pt]{article}
%\\VignetteIndexEntry{@pkg@-unitTests}
\\usepackage{vmargin}
\\setmargrb{0.75in}{0.75in}{0.75in}{0.75in}

\\RequirePackage{ae,mathpple}    % ae as a default font pkg works with Sweave
\\RequirePackage[T1]{fontenc}

<<echo=FALSE,print=FALSE>>=
pkg <- '@pkg@'
require( pkg, character.only=TRUE )
prettyVersion <- packageDescription(pkg)$Version
prettyDate <- format(Sys.Date(), '%B %e, %Y')
authors <- packageDescription(pkg)$Author
@

\\usepackage[colorlinks]{hyperref}
\\author{\\Sexpr{authors}}
\\title{\\texttt{\\Sexpr{pkg}}: Unit testing results}
\\date{\\texttt{\\Sexpr{pkg}} version \\Sexpr{prettyVersion} as of \\Sexpr{prettyDate}}
\\begin{document}
\\maketitle

\\begin{verbatim}
@results@
\\end{verbatim}

\\section*{Session Information}
@sessionInfo@

\\end{document}
"
	# load the results of the unit tests
	results <- file.path('unitTests-results', paste(pkg, '-unitTests.txt', sep=''))
	results <- 
	if( file.exists( results ) ){
		paste(readLines(results), collapse="\n")
	} else{
		'unit test results not available'
	}
	
	# substitute template variables
	contents <- Rnw.template
	# package name
	contents <-	gsub("@pkg@", pkg, contents)
	# unit test results
	contents <-	gsub("@results@", results, contents)
	# session info
	contents <-	gsub("@sessionInfo@", gsub("\\", "\\\\", paste(toLatex(sessionInfo()), collapse="\n"), fixed=TRUE), contents)
	
	# write into the file
	writeLines(contents, file)
}
