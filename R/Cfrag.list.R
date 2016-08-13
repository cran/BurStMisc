"Cfrag.list" <-
function (x, file=NULL, item.num=c(3,10,5), indent=c("\t", "\t\t"),
	declaration.only=FALSE, long=FALSE, append=FALSE) 
{
        fun.copyright <- "Placed in the public domain 2009 by Burns Statistics Ltd."
	fun.version <- "Cfrag.list 003"

	subfun.bp <- function(z, inum, indent2) {
		zlen <- length(z)
		start <- seq(1, zlen, by=inum)
		end <- c(start[-1] - 1, zlen)
		cm1 <- paste("paste(z[", start, ":", end, "], collapse=', ')", 
			sep="")
		cm2 <- paste("paste(c(", paste(cm1, collapse=", "), "))")
		subans <- eval(parse(text=cm2))
		paste(indent2, subans, rep(c(",", ""), c(length(start)-1, 1)), 
			sep="")
	}

	# start of main function

	decl <- unlist(lapply(x, storage.mode))
	prefix <- rep("", length(decl))
	pdm <- match(decl, c("double", "integer", "character"), nomatch=NA)
	if(any(is.na(pdm))) 
		stop("at least one storage mode that can not be handled")
	decl[pdm == 2] <- if(long) "long" else "int"
	decl[pdm == 3] <- "char"
	prefix[pdm == 3] <- "*"

	dec.out <- paste(decl, " ", prefix, names(x), "[]", sep="")
	if(declaration.only) {
		dec.out <- paste(indent[1], dec.out, ";", sep="")
		if(length(file) && nchar(file)) {
			cat(dec.out, sep="\n", file=file, append=append)
			return(file[1])
		} else {
			return(dec.out)
		}
	}

	item.num <- rep(item.num, length=3)
	indent <- rep(indent, length=2)

	ans <- NULL
	for(i in 1:length(x)) {
		switch(decl[i], 
			double= {
				t.inum <- item.num[1]
			}, 
			long=,
			int={
				t.inum <- item.num[2]
			},
			char={
				t.inum <- item.num[3]
				x[[i]] <- paste('"', x[[i]], '"', sep="")
			})
		t.sa <- subfun.bp(x[[i]], t.inum, indent[2])
		ans <- c(ans, paste(indent[1], dec.out[i], " = {", sep=""), 
			t.sa, paste(indent[1], "};", sep=""))
	}

	if(length(file) && nchar(file)) {
		cat(ans, sep="\n", file=file, append=append)
		return(file[1])
	} else {
		return(ans)
	}
}

