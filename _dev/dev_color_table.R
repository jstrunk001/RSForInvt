#looks like ztable has similar functionality

color_table=function(
	x
	
	,pdf_out = NA
	,pdf_dims = c(ht=8,wd=11)
	,pdf_mar = c(8,8,8,10)
	
	,colors_fn = colorRamps::blue2red #colorRampPalette(c("blue", "red"))
	,colors_breaks = NA
	,colors_n = 10
	,colors_labels = NA
	
	,axes_show = T
	,axes_yNms = NA
	,axes_xNms = NA
	
	,table_precision = 1
	,table_main = "Color Table Plot"
){

	if(!is.na(pdf_out)) pdf(pdf_out,height=pdf_dims["ht"],width=pdf_dims["wd"])
	
	par(mar=pdf_mar)
	if(is.na(colors_breaks[1])) {
		colors_breaks_in = seq( from = floor(min(x*.99)/10)*10 , to = ceiling(max(x*1.01)/10)*10 , length.out = colors_n+1 )
	}
	if(!is.na(colors_breaks[1])){
		colors_breaks_in = colors_breaks
	}
	colors_n_in = length(colors_breaks_in)-1
	colors_in = colors_fn(colors_n_in)
	
	#create colored background based on x values
	image( 1:ncol(x) 
				 ,1:nrow(x) 
				 , t(x)
				 , col = colors_in
				 , breaks = colors_breaks_in
				 , axes = FALSE
				 ,xlab=""
				 ,ylab=""
				 , main = table_main  
	)
	
	
	#add color legend
	if(is.na(colors_labels[1])) colors_labels_in = round(colors_breaks_in[-1] - diff(colors_breaks_in)/2,table_precision )
	if(!is.na(colors_labels[1])) colors_labels_in = colors_labels
	color.legend(8,2,8.2,15, colors_labels_in , colors_in , gradient="y" )
	
	#add named axes
	if(axes_show){
		#x axis
		if(is.na(axes_xNms[1])) axis(1, 1:ncol(x), colnames(x),las=2)
		if(!is.na(axes_xNms[1])) axis(1, 1:ncol(x), axes_xNms,las=2)
		#y axis
		if(is.na(axes_yNms[1])) axis(2, 1:nrow(x), rownames(x),las=2)
		if(!is.na(axes_yNms[1])) axis(2, 1:nrow(x), axes_yNms,las=2)
	}
	
	#add numeric values to graph
	for (i in 1:ncol(x))
		for (j in 1:nrow(x))
			text(i, j, round(x[j,i],table_precision))

	
	if(!is.na(pdf_out)) dev.off()
	
}