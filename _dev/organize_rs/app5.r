library(shiny)
library(DT)

#if(F) source("r\\organizers_2.0.r")

shinyApp(

	ui <- fluidPage(

		mainPanel(

			actionButton("get_folder", "Select Project")
			,verbatimTextOutput("one_proj")
			,br()
			,br()
			,DTOutput('template')



		)

		# sidebarLayout(
		# 	sidebarPanel(
		# 		actionButton("get_folder", "Select Project")
		# 		#,actionButton("do_move", "Move Project")
		# 	)
		#
		# 	,
		# 	mainPanel(
		#
		# 		actionButton("get_folder", "Select Project")
		# 		,verbatimTextOutput("one_proj")
		# 		,DT::dataTableOutput('template')
		#
		#
		#
		# 	)
		#
		#
		# )




	)


	,server = function(input, output) {




		.scan_1project = function(dir_project){

			print(paste("Scan project:",dir_project))

			l_proj = list(
				dir_project = dir_project
				,subdirs_project = list.dirs(dir_project, full.names = TRUE, recursive = T)
				,files_all = list.files(dir_project, full.names = TRUE, recursive = T)
			)

			return(l_proj)

		}

		.template_1project = function(l_proj, dir_out ){

			require(plyr)

			from_project_dir  = l_proj[["dir_project"]]
			to_project_dir = file.path(dir_out, basename(l_proj[["dir_project"]]))
			to_original_dir = file.path(dir_out, basename(l_proj[["dir_project"]]),"original")
			subdirs = c("las","laz","dtm_original","dtm_fusion","tile_vectors","original","projection")
			subdirs_project = paste(to_project_dir,subdirs,sep="/")


			df_template=data.frame(
				type = ""
				,dir_old = tolower(l_proj$subdirs_project)
				,dirs_new1 = tolower(gsub(from_project_dir,to_original_dir,l_proj$subdirs_project))
				,dirs_new2 = ""
			)

			l_types = list(
				las = grep("[.]las.{0,1}$",l_proj$files_all,value=T,ignore.case=T)
				,laz = grep("[.]laz$",l_proj$files_all,value=T,ignore.case=T)
				,dtm = grep("[.]dtm$",l_proj$files_all,value=T,ignore.case=T)
				,tif = grep("[.]tif$",l_proj$files_all,value=T,ignore.case=T)
				,img = grep("[.]img$",l_proj$files_all,value=T,ignore.case=T)
				,asc = grep("[.]asc$",l_proj$files_all,value=T,ignore.case=T)
				,adf = grep("[.]adf$",l_proj$files_all,value=T,ignore.case=T)
				#,shp = grep("[.]shp$",l_proj$files_all,value=T,ignore.case=T)
				,doc = grep("[.]doc.{0,1}$",l_proj$files_all,value=T,ignore.case=T)
				,pdf = grep("[.]pdf$",l_proj$files_all,value=T,ignore.case=T)
				,html = grep("[.]html$",l_proj$files_all,value=T,ignore.case=T)
			)

			l_types[["others"]] = l_proj$files_all[!l_proj$files_all %in% unique(unlist(l_types))]

			l_folders = lapply(l_types, function(x)unique(dirname(x)))

			df_template = read.csv(text="type,dir_old,dir_new1,dir_new2",stringsAsFactors=F,colClasses="character")

			#first cut - look at individual files
			if(length(l_folders$las) > 0){
				df_template = rbind.fill(df_template
																 ,data.frame(type=".las",dir_old=l_types$las
																 						,dir_new1=gsub(from_project_dir,paste(to_project_dir,"las",sep="/"),l_types$las)
																 						,dir_new2=gsub(from_project_dir,to_original_dir,l_types$las)
																 )		)
			}
			if(length(l_folders$laz) > 0){
				df_template = rbind.fill(df_template
																 ,data.frame(type=".laz",dir_old=l_types$laz
																 						,dir_new1=gsub(from_project_dir,paste(to_project_dir,"laz",sep="/"),l_types$laz)
																 						,dir_new2=gsub(from_project_dir,to_original_dir,l_types$laz)
																 )		)
			}
			if(length(l_folders$dtm) > 0){
				df_template = rbind.fill(df_template
																 ,data.frame(type=".dtm",dir_old=l_types$dtm
																 						,dir_new1=gsub(from_project_dir,paste(to_project_dir,"dtm_fusion",sep="/"),l_types$dtm)
																 						,dir_new2=gsub(from_project_dir,to_original_dir,l_types$dtm)
																 )		)
			}
			if(length(l_folders$tif) > 0){
				df_template = rbind.fill(df_template
																 ,data.frame(type=".tif",dir_old=l_types$tif
																 						,dir_new1=gsub(from_project_dir,paste(to_project_dir,"dtm_original",sep="/"),l_types$tif)
																 						,dir_new2=gsub(from_project_dir,to_original_dir,l_types$tif)
																 )		)
			}
			if(length(l_folders$img) > 0){
				df_template = rbind.fill(df_template
																 ,data.frame(type=".img",dir_old=l_types$img
																 						,dir_new1=gsub(from_project_dir,paste(to_project_dir,"dtm_original",sep="/"),l_types$img)
																 						,dir_new2=gsub(from_project_dir,to_original_dir,l_types$img)
																 )		)
			}
			if(length(l_folders$asc) > 0){
				df_template = rbind.fill(df_template
																 ,data.frame(type=".asc",dir_old=l_types$asc
																 						,dir_new1=gsub(from_project_dir,paste(to_project_dir,"dtm_original",sep="/"),l_types$asc)
																 						,dir_new2=gsub(from_project_dir,to_original_dir,l_types$asc)
																 )		)
			}
			if(length(l_folders$adf) > 0){
				esri_files = unique(dirname(l_types$adf))
				df_template = rbind.fill(df_template
																 ,data.frame(type=".adf",dir_old=l_types$adf
																 						,dir_new1=gsub(from_project_dir,paste(to_project_dir,"dtm_original",sep="/"),esri_files)
																 						,dir_new2=gsub(from_project_dir,to_original_dir,esri_files)
																 )		)
			}
			if(length(l_folders$doc) > 0){

				doc_dirs = unique(dirname(l_types$doc))
				df_template = rbind.fill(df_template
																 ,data.frame(type=".doc",dir_old=l_types$doc
																 						,dir_new1=gsub(from_project_dir,paste(to_project_dir,"original_metafiles",sep="/"),l_types$doc)
																 						,dir_new2=gsub(from_project_dir,to_original_dir,l_types$doc)
																 )		)
			}
			if(length(l_folders$pdf) > 0){

				pdf_dirs = unique(dirname(l_types$pdf))
				df_template = rbind.fill(df_template
																 ,data.frame(type=".pdf",dir_old=l_types$pdf
																 						,dir_new1=gsub(from_project_dir,paste(to_project_dir,"original_metafiles",sep="/"),l_types$pdf)
																 						,dir_new2=gsub(from_project_dir,to_original_dir,l_types$pdf)
																 )		)
			}
			if(length(l_folders$html) > 0){

				html_files = unique(dirname(l_types$html))
				df_template = rbind.fill(df_template
																 ,data.frame(type=".html",dir_old=l_types$html
																 						,dir_new1=gsub(from_project_dir,paste(to_project_dir,"original_metafiles",sep="/"),l_types$html)
																 						,dir_new2=gsub(from_project_dir,to_original_dir,l_types$html)
																 )		)
			}
			if(length(l_folders$other) > 0){

				other_dirs = unique(dirname(l_types$others))
				df_template = rbind.fill(df_template
																 ,data.frame(type="other",dir_old=l_types$others
																 						,dir_new1=gsub(from_project_dir,to_original_dir,l_types$others)
																 						,dir_new2=""#gsub(from_project_dir,to_original_dir,l_types$others)
																 )		)
			}

			#second cut - organize by folders, las and laz may be in same folder ...
			spl_template = split(df_template, df_template$type)
			df_dirs_temp = rbind.fill(lapply(spl_template,.fn_temp_dirs))

			return(list(df_template,df_dirs_temp))

		}

		.fn_temp_dirs=function(df_tempi){

			#get unique set of directories
			dir_old_in = dirname(df_tempi$dir_old)
			dup_dir = duplicated(dir_old_in)
			df_tempi_unq =  df_tempi[!dup_dir,]

			#select first instance into template, set 2nd,3rd... instances into alternate "dir_new2" folder"
			if(length(df_tempi_unq) > 0){
				#if(sum(grepl(c(".las",".laz",".dtm",".tif",".adf",".img",".asc") , df_tempi_unq)) > 0){ #if grepl not really necessary
				df_tempi_unq_a = df_tempi_unq
				df_tempi_unq[-1,"dir_new1"] = df_tempi_unq_a[-1,"dir_new2"]
				df_tempi_unq[-1,"dir_new1"] = df_tempi_unq_a[-1,"dir_new1"]
				#}
			}

			#return directories only
			df_tempi_unq[,-1] = apply(df_tempi_unq[,-1],2,dirname)
			df_tempi_unq

		}



		shinyInput = function(FUN, len, id, ...) {
			inputs = character(len)
			for (i in seq_len(len)) {
				inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...))
			}
			inputs
		}

		# obtain the values of inputs
		shinyValue = function(id, len) {
			unlist(lapply(seq_len(len), function(i) {
				value = input[[paste0(id, i)]]
				if (is.null(value)) NA else value
			}))
		}


		proj_dir = observeEvent(input$get_folder, {

			#dir_proj_in = choose.dir(caption = "Select RS Project")
			dir_proj_in = "D:/temp/test_dir/New folder - Copy (3)/"
			proj_in=list()
			proj_in$one_proj = renderText(one_proj_in)
			proj_in$subdirs = DT::renderDataTable(data.frame(dirs = list.dirs(one_proj_in,recursive = F)))
			print("hello")
			proj_in$one_scan = .scan_1project(dir_proj_in)
			print("hello")
			proj_in$one_template = .template_1project(proj_in[["one_scan"]], "d:\\temp\\")

			proj_in$res = data.frame(
				original = proj_in$one_template[[2]][,1]
				,target = proj_in$one_template[[2]][,2]
				,use_alt = shinyInput(checkboxInput, nrow(proj_in$one_template[[2]]), 'use_alt_', value = F)
				,alternate = proj_in$one_template[[2]][,3]
				,stringsAsFactors = FALSE
			)


			output$template = renderDT(
				proj_in$res, server = FALSE, escape = FALSE, selection = 'none',editable=T, options = list(
					preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
					drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
					,columnDefs = list(list(width="20%", targets = 1:4))
				)
			)

			#return(proj_in)

		})


		print("hello")

		# render the table containing shiny inputs


	}
)


