	createTemplates=function(
											dir_projects
											, dir_out
											, rescan = F
											, copy = T
											, cut = F
											, templateRDS=c("c:/temp/template1.rds")
											, return_templates = F
											){

		options(stringsAsFactors = F)
		if(!dir.exists(dir_projects)) stop("argument 'dir_projects' does not exist, check arguments and try again.")

		if(!rescan & file.exists(templateRDS)){

			proj_templates = readRDS(templateRDS)

		}else{

			if(!dir.exists(dirname(templateRDS))) dir.create(dirname(templateRDS))

			#left to right
			dir_projects = .clean_path(dir_projects)
			dir_out = .clean_path(dir_out)

			#list projects
			dir_projects_in = .clean_path(list.dirs(dir_projects, full.names = TRUE, recursive = FALSE))

			print(c("all projects:",dir_projects_in))

			#summarize projects iteratively
			#proj_contents = lapply(dir_projects_in,.scan_1project)
			proj_contents = lapply(dir_projects_in,.scan_1project)

			#create templates for generic reorganization
			proj_templates = lapply(proj_contents,.template_1project, dir_out)

			saveRDS(proj_templates,templateRDS)

		}

			#browser()

		intro_folders=c(
			",This is a template for moving lidar projects into a consistent format."
			,",Column 'dir_old' is the current structure of the project."
			,",Column 'dir_new1' is the default target folder."
			,",Column 'dir_new2' is an alternative location inside of a 'misc' folder."
			,",If you wish to use dir_new2 place an X (or any text) in 'use_dirnew2'."
			,",You can manually updated dir_new1 if you don't like 'dir_new1' or 'dir_new2'."
			)
		intro_folders=c(intro_folders, rep("",10-length(intro_folders)))

		intro_files=c("Please see the folder template! Do not change anything in this folder","","","","")

		#write csv files and batch files to projects
		for(i in 1:length(proj_templates)){

			dfi = proj_templates[[i]][["folders"]]

			if(nrow(dfi) > 0){

				dfi_files = proj_templates[[i]][["files"]]
				dir_i = proj_templates[[i]][["in_dir"]]
				outi_folders = paste(dir_i,"\\","XXMOVE_FOLDER_TEMPLATEXX.CSV",sep="")
				outi_files = paste(dir_i,"\\","XXMOVE_FILE_TEMPLATEXX.CSV",sep="")

				dfi_folders=data.frame(dfi[,c(1:3)],use_dirnew2="",dfi[,c(4),drop=F])

				#write introductions and templates
				writeLines(intro_folders,con = outi_folders)
				writeLines(intro_files,con = outi_files)
				suppressWarnings(write.table(dfi_folders,outi_folders, append = T,row.names = F,col.names=T,sep=","))
				suppressWarnings(write.table(dfi_files,outi_files, append = T,row.names = F,col.names=T,sep=","))

			}

		}



		if(return_templates) return(proj_templates)

	}

	moveRS=function(dir=NA,copy=T,cut=F,dir_out = NA){

		if(is.na(dir)) dir_in = choose.dir()
		else dir_in = dir

		if(!copy & !cut){ warning("cut and copy set to F, nothing to do"); return()}
		if(copy){
			if(cut){
				cut=F
				print("Cannot set 'cut = T' if 'copy = T', cut will revert to 'F'")
				warning("Cannot set 'cut = T' if 'copy = T', cut will revert to 'F'")
			}
		}
		if(cut){
			do_cut = readline(prompt="Are you sure you want to 'cut' this project? File movement is permanent (y/n)")
			if(tolower(do_cut) != "y"){
				cut = F
				copy = T
			}
		}

		this_template = paste(dir_in,"\\","XXMOVE_FOLDER_TEMPLATEXX.CSV",sep="")

		if(!file.exists(this_template)){

			  if(is.na(dir_out)) dir_out = choose.dir(dir_in, caption = "Select folder")
				if(is.na(dir_out)) stop("No template in target folder, and no destination provided")
				oRganizeRS(dir_in,dir_out, rescan = T)
				warning("Please review the template:",this_template,"then re-run moveRS")
				return()

		}else{
			template_in = read.csv(this_template,skip=10,header=T,colClasses = "character")

			#test / fix primary target directory
			swap = sapply(template_in[,"use_dirnew2"],function(x)nchar(x)>0)
			template_in[,"temporary"] = NA
			template_in[,"temporary"] = template_in[,"dir_new1"]
			template_in[swap,"dir_new1"] = template_in[swap,"dir_new2"]
			template_in[swap,"dir_new2"] = template_in[swap,"temporary"]

			template_in[,"dir_new1"] = .clean_path( template_in[,"dir_new1"] )
			template_in[,"dir_old"] = .clean_path( template_in[,"dir_old"] )

			#sort by number of charaters in dir_new, place "other" files last
			charsi = sapply(template_in[,"dir_new1"],nchar)
			otheri = template_in[,"type"] == "other"
			charsi[otheri] = charsi[otheri] - min(charsi) - 1
			template_in1 = template_in[order(charsi,decreasing = T),]

			#identify cases of matching inputs
			#the last duplicate is skipped, causing the whole folder to be copied at the last iteration
			dups = duplicated(template_in[,"dir_old"], fromLast = F) | duplicated(template_in[,"dir_old"], fromLast = T)
			is_dup = sum( dups ) > 0

			if(is_dup){
				all_files = .clean_path(list.files(dir_in,recursive=T, full.names=T))
				remaining_files = all_files
			}

			#copy files
			for(i in 1:nrow(template_in1)){
				#create destination folder
				if(!dir.exists(template_in1[i,"dir_new1"])) dir.create(template_in1[i,"dir_new1"],recursive=T)
				#cut / copy folders if they are NOT intermixed - or for the last intermixed type
				if(!dups[i]){
					if(copy){
						try(file.copy(template_in1[i,"dir_old"],template_in1[i,"dir_new1"]))
					}
					if(cut) try(file.rename(template_in1[i,"dir_old"],template_in1[i,"dir_new1"]))
				}
				#cut / copy files if they ARE intermixed
				if(dups[i]){


					if(template_in1[i,"type"] != "other"){

						#get these files
						files_this_A = grep( tolower(template_in1[i,"dir_old"]) , tolower(all_files) , fixed=T , value=T )
						files_this_B = grep(paste(gsub("[.]","[.]",template_in1[i,"type"]),"$",sep="") , files_this_A , fixed=F , value=T )
						remaining_files = remaining_files[ ! remaining_files %in% files_this_B ]

						# files_i = list.files(template_in1[i,"dir_old"],pattern = template_in1[i,"type"], full.names = T, ignore.case = T)
						files_to = file.path(template_in1[i,"dir_new1"],basename(files_this_B))


					}
					if(template_in1[i,"type"] == "other"){

						#get these files
						files_this_B = grep( tolower(template_in1[i,"dir_old"]) , tolower(remaining_files) , fixed=T , value=T )
						remaining_files = remaining_files[ remaining_files %in% files_this_B ]
						files_to = file.path(template_in1[i,"dir_new1"],basename(files_this_B))

					}

					#actually move files
					if(copy){
						try( file.copy( files_this_B , files_to) )
					}
					if(cut){
						try( file.rename( files_this_B , files_to) )
						warning("When files are CUT, files move, but the base project folder and subfolders remain.\n"
										,"Please manually verify that project is empty, and delete empty project.")
					}


				}
			}
		}
	}

	.write_1template=function(dir, template_i){

		intro_folders=c(
			",This is a template for moving lidar projects into a consistent format."
			,",Column 'dir_old' is the current structure of the project."
			,",Column 'dir_new1' is the default target folder."
			,",Column 'dir_new2' is an alternative location inside of a 'misc' folder."
			,",If you wish to use dir_new2 place an X (or any text) in 'use_dirnew2'."
			,",You can manually updated dir_new1 if you don't like 'dir_new1' or 'dir_new2'."
		)
		intro_folders=c(intro_folders, rep("",10-length(intro_folders)))

		intro_files=c("Please see the folder template! Do not change anything in this folder","","","","")

		#write csv files and batch files to projects
		dfi = template_i[["folders"]]

		if(nrow(dfi) > 0){

			dfi_files = template_i[["files"]]
			dir_i = template_i[["in_dir"]]
			outi_folders = paste(dir_i,"\\","XXMOVE_FOLDER_TEMPLATEXX.CSV",sep="")
			outi_files = paste(dir_i,"\\","XXMOVE_FILE_TEMPLATEXX.CSV",sep="")

			dfi_folders=data.frame(dfi[,c(1:3)],use_dirnew2="",dfi[,c(4),drop=F])

			#write introductions and templates
			writeLines(intro_folders,con = outi_folders)
			writeLines(intro_files,con = outi_files)
			suppressWarnings(write.table(dfi_folders,outi_folders, append = T,row.names = F,col.names=T,sep=","))
			suppressWarnings(write.table(dfi_files,outi_files, append = T,row.names = F,col.names=T,sep=","))

		}

	}

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

	#can for file types
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

	df_template = rbind.fill(
		df_template
		,.fn_assign1(type = ".las"	,dir_old = from_project_dir ,dir_new = to_project_dir ,subfolder = "las" ,folder_list = l_types$las)
		,.fn_assign1(type = ".laz"	,dir_old = from_project_dir ,dir_new = to_project_dir ,subfolder = "laz" ,folder_list = l_types$laz)
		,.fn_assign1(type = ".dtm"	,dir_old = from_project_dir ,dir_new = to_project_dir ,subfolder = "dtm_fusion" ,folder_list = l_types$dtm)
		,.fn_assign1(type = ".tif"	,dir_old = from_project_dir ,dir_new = to_project_dir ,subfolder = "dtm_original" ,folder_list = l_types$tif)
		,.fn_assign1(type = ".img"	,dir_old = from_project_dir ,dir_new = to_project_dir ,subfolder = "dtm_original" ,folder_list = l_types$img)
		,.fn_assign1(type = ".asc"	,dir_old = from_project_dir ,dir_new = to_project_dir ,subfolder = "dtm_original" ,folder_list = l_types$asc)
		,.fn_assign1(type = ".adf"	,dir_old = from_project_dir ,dir_new = to_project_dir ,subfolder = "dtm_original" ,folder_list = unique(dirname(l_types$adf)))
		,.fn_assign1(type = ".doc"	,dir_old = from_project_dir ,dir_new = to_project_dir ,subfolder = "metafiles_original" ,folder_list = l_types$doc)
		,.fn_assign1(type = ".docx"	,dir_old = from_project_dir ,dir_new = to_project_dir ,subfolder = "metafiles_original" ,folder_list = l_types$docx)
		,.fn_assign1(type = ".pdf"	,dir_old = from_project_dir ,dir_new = to_project_dir ,subfolder = "metafiles_original" ,folder_list = l_types$pdf)
		,.fn_assign1(type = ".html"	,dir_old = from_project_dir ,dir_new = to_project_dir ,subfolder = "metafiles_original" ,folder_list = l_types$html)
		,.fn_assign1(type = "other"	,dir_old = from_project_dir ,dir_new = to_project_dir ,subfolder = "original" ,folder_list = l_types$others)
	)

	#second cut - organize by folders, las and laz may be in same folder ...
	spl_template = split(df_template, df_template$type)
	df_dirs_temp = rbind.fill(lapply(spl_template,.fn_temp_dirs))

	if(nrow(df_template)==0) df_dirs_temp = df_template

	return(list(files = df_template,folders = df_dirs_temp, in_dir = from_project_dir, out_dir = to_project_dir ))

}

.fn_assign1=function(
	type
	,dir_old
	,dir_new
	,subfolder
	,folder_list
){
	if( length(folder_list) > 0 ){

		folder_list_in = folder_list[order(nchar(folder_list))]
		dir_new1 = .clean_path(paste(dir_new,subfolder,basename(folder_list_in),sep="/"))
		dir_new2 = .clean_path(gsub(dir_old,paste(dir_new,"original",sep="/"),folder_list_in, fixed = T))

		#only place first instance of file type in primary subfolder, otherwise put everything in "original" folder
		df_temp_in = data.frame(
			type=type
			,dir_old = folder_list_in
			,dir_new1 = dir_new1 #c(dir_new1[1],dir_new2[-1])
			,dir_new2 = dir_new2 #c(dir_new2[1],dir_new1[-1])
		)

	}else 			df_temp_in = NULL

	return( df_temp_in )

}

.fn_temp_dirs=function(df_tempi){

	#get unique set of directories
	dir_old_in = dirname(df_tempi$dir_old)
	dup_dir = duplicated(dir_old_in)
	df_tempi_unq =  df_tempi[!dup_dir,]
	df_tempi_unq = df_tempi_unq[order(nchar(df_tempi_unq[,"dir_new1"])),]

	#select first instance into template, set 2nd,3rd... instances into alternate "dir_new2" folder"
	if(nrow(df_tempi_unq) > 1){
			df_tempi_unq_a = df_tempi_unq
			df_tempi_unq[-1,"dir_new1"] = df_tempi_unq_a[-1,"dir_new2"]
			df_tempi_unq[-1,"dir_new2"] = df_tempi_unq_a[-1,"dir_new1"]
	}

	#return directories only
	df_tempi_unq[,c("dir_old","dir_new1","dir_new2")] = apply(df_tempi_unq[,c("dir_old","dir_new1","dir_new2")],2,dirname)
	df_tempi_unq

}

.seekFiles = function(pattern, dir){

	list.files(dir, pattern=pattern, full.names=T,recursive = T,ignore.case = T)

}

.clean_path=function(
	path
	,backslash=T
	,force_endslash=F
){

	path_in=unlist(path)

	#fix paths
	#path_ok=gsub("XXXLEADINGXXX","\\\\\\\\",gsub("\\\\\\\\","\\\\",gsub("^\\\\\\\\","XXXLEADINGXXX",gsub("/","\\\\",gsub("\\\\$","",gsub("/$","",path_in))))))
	path_ok=gsub("XXXLEADINGXXX","\\\\\\\\",gsub("\\\\\\\\","\\\\",gsub("^\\\\\\\\","XXXLEADINGXXX",gsub("/","\\\\",gsub("\\\\\\\\$","",gsub("//$","",path_in))))))

	#if slashcap
	if(force_endslash) path_ok=paste(gsub("\\\\$","",path_ok),"\\",sep="")

	#use forward slash
	if(!backslash) path_ok= gsub("\\\\","/",path_ok)

	#return data
	return (path_ok)

}


#run tests



if(T){

	base_dir = "d:\\temp"
	copyFrom = file.path(base_dir,"test_copyFrom\\")
	copyTo = file.path(base_dir,"test_copyTo\\")
	createTemplates(copyFrom,copyTo, rescan = T)

	unlink(copyTo, recursive = T)
	moveRS(file.path(base_dir,"test_copyFrom\\New folder - Copy (3)"), copy = T, cut = F)
	moveRS(file.path(base_dir,"test_copyFrom\\New folder - Copy (3) - Copy"), copy = T, cut = F)

}
if(F){

	base_dir = "d:\\temp"
	#path variables
	cutFrom = file.path(base_dir,"test_cutFrom")
	cutTo = file.path(base_dir,"test_cutTo")
	copyFrom = file.path(base_dir,"test_copyFrom")

	#create a clean directory to cut from
	unlink(cutFrom, recursive = T)
	unlink(cutTo, recursive = T)
	dir.create(cutFrom)
	dir.create(cutTo)
	file.copy(list.dirs(copyFrom,full.names=T,recursive=F),cutFrom,recursive=T)

	#make templates for all subdirectories
	createTemplates(cutFrom,cutTo, rescan = T)

	#move / cut files
	for(folderi in list.dirs(cutFrom,recursive = F,full.names=T)) moveRS(folderi , copy = F, cut = T)

}
