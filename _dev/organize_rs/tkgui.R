library(tcltk)
library(tcltk2)

tclArrayVar <-
	function(Rarray=NULL)
	{
		if (!is.null(Rarray) && !is.vector(Rarray) && length(dim(Rarray))!=2)
			stop("Array must be one-dimensional or two-dimensional.")
		require(tcltk)

		n <- .TkRoot$env$TclVarCount <- .TkRoot$env$TclVarCount +
			1L
		name <- paste0("::RTcl", n)
		l <- list(env = new.env())
		assign(name, NULL, envir = l$env)
		reg.finalizer(l$env, function(env) tcl("unset", ls(env)))
		class(l) <- "tclVar"


		if (is.null(Rarray))
		{
			ndim <- 2
			.Tcl(paste("set ",name,"(0,0) \"\"",sep=""))
		}
		else
		{
			if (is.vector(Rarray))
			{
				ndim <- 1
				Rarray <- as.data.frame(Rarray)
			}
			else
				ndim <- 2
			for (i in (1:nrow(Rarray)))
				if (ndim==2)
					for (j in (1:ncol(Rarray)))
						.Tcl(paste("set ",name,"(",i,",",j,") \"",paste(Rarray[i,j]),"\"",sep=""))
			else
				.Tcl(paste("set ",name,"(",i,",",1,") \"",paste(Rarray[i,1]),"\"",sep=""))
			if (!is.null(rownames(Rarray)))
				for (i in (1:nrow(Rarray)))
					.Tcl(paste("set ",name,"(",i,",",0,") \"",rownames(Rarray)[i],"\"",sep=""))
			else
				for (i in (1:nrow(Rarray)))
					.Tcl(paste("set ",name,"(",i,",",0,") \"\"",sep=""))
			if (!is.null(colnames(Rarray)))
				for (j in (1:ncol(Rarray)))
					.Tcl(paste("set ",name,"(",0,",",j,") \"",colnames(Rarray)[j],"\"",sep=""))
			else
				for (j in (1:ncol(Rarray)))
					.Tcl(paste("set ",name,"(",0,",",j,") \"\"",sep=""))
			l$nrow <- nrow(Rarray)
			l$ncol <- ncol(Rarray)
		}
		l$ndim <- ndim
		l
	}

# Define a matrix
mat2 <- matrix(1:2000, nrow = 50, ncol = 40,
							 dimnames = list(paste("Row", 1:50), paste("Col", 1:40)))

# Define a tclArrayVar and initialize it to that matrix
tclArr2 <- tclArrayVar(mat2)

# Display the Tcl array in a Tk table widget (using edit method).
# The Tcl name of the array variable is displayed in the title bar.
edit(mat2)


win <- gwindow("Hello World, ad nauseum", visible=TRUE)
mat3=mat2[1:10,1:10]
res=gdf(mat3,container=gwindow())

> obj <- gbutton("Hello...",container=group,
								 + handler = function(h,...) gmessage("world"))
> obj <- glabel("Hello...", container =group,
								+ handler = function(h,...) gmessage("world"))
> obj <- gcombobox(c("Hello","world"), container=group)
> obj <- gedit("Hello world", container=group)
> obj <- gtext("Hello world", container=group, font.attr=list(style="bold"))


library("gWidgets")


# Display the Tcl array, showing only 10 rows and 10 columns
edit(tclArr2, height = 10, width = 5)
