oRganizeRS=function(dir_projects, dir_out){

	options(stringsAsFactors = F)

	#left to right
	dir_projects = gsub("\\\\","/",dir_projects)
	dir_out = gsub("\\\\","/",dir_out)

	#list projects
	dir_projects_in = list.dirs(dir_projects, full.names = TRUE, recursive = FALSE)

	print(c("all projects:",dir_projects_in))

	#summarize projects iteratively
	proj_details = lapply(dir_projects_in,.scan_1project)

	#create templates for generic reorganization
	proj_templates = lapply(proj_details,.template_1project, dir_out)




	return(proj_templates)

}


.scan_1project = function(dir_project){

	print(paste("Scan project:",dir_project))

	l_proj = list(

		dir_project = dir_project
		,subdirs_project = list.dirs(dir_project, full.names = TRUE, recursive = T)
		,files_main = list.files(dir_project, full.names = TRUE, recursive = F)

		,dirs_types = list()

		,paths_files = list(
		#get all unique files of the associated file types
			paths_las = .seekFiles("[.]las$",dir_project)
			,paths_laz = .seekFiles("[.]laz$",dir_project)
			,paths_dtm = .seekFiles("[.]dtm$",dir_project)
			,paths_tif = .seekFiles("[.]tif$",dir_project)
			,paths_img = .seekFiles("[.]img$",dir_project)
			,paths_adf = .seekFiles("[.]adf$",dir_project)
			,paths_shp = .seekFiles("[.]shp$",dir_project)
			,paths_doc = .seekFiles("[.]doc.$",dir_project)
			,paths_pdf = .seekFiles("[.]pdf$",dir_project)
		)
	)
	warning("[.]doc.$ search string not vetted")

	#get all unique subdirectories with associated file types
	l_proj[["dirs_types"]][["dirs_las"]] = try(unique(dirname(l_proj[["paths_files"]][["paths_las"]])) ,silent = T)
	l_proj[["dirs_types"]][["dirs_laz"]] = try(unique(dirname(l_proj[["paths_files"]][["paths_laz"]])) ,silent = T)
	l_proj[["dirs_types"]][["dirs_dtm"]] = try(unique(dirname(l_proj[["paths_files"]][["paths_dtm"]])) ,silent = T)
	l_proj[["dirs_types"]][["dirs_tif"]] = try(unique(dirname(l_proj[["paths_files"]][["paths_tif"]])) ,silent = T)
	l_proj[["dirs_types"]][["dirs_img"]] = try(unique(dirname(l_proj[["paths_files"]][["paths_img"]])) ,silent = T)
	l_proj[["dirs_types"]][["dirs_adf"]] = try(unique(dirname(dirname(l_proj[["paths_files"]][["paths_adf"]]))) ,silent = T)
	l_proj[["dirs_types"]][["dirs_shp"]] = try(unique(dirname(l_proj[["paths_files"]][["paths_shp"]])) ,silent = T)
	l_proj[["dirs_types"]][["dirs_doc"]] = try(unique(dirname(l_proj[["paths_files"]][["paths_doc"]])) ,silent = T)
	l_proj[["dirs_types"]][["dirs_pdf"]] = try(unique(dirname(l_proj[["paths_files"]][["paths_pdf"]])) ,silent = T)
	#catch errors and cast to character
	l_proj[["dirs_types"]] = lapply(l_proj[["dirs_types"]],function(x,...) if (class(x) == "try-error" ) character(0) else x )


	return(l_proj)

}

.template_1project = function(l_proj, dir_out ){


	if(length(l_proj$dirs_types$dirs_laz) >=1  | length(l_proj$dirs_types$dirs_laz) >=1){

		from_project_dir  = l_proj[["dir_project"]]
		to_project_dir = file.path(dir_out, basename(l_proj[["dir_project"]]))
		to_original_dir = file.path(dir_out, basename(l_proj[["dir_project"]]),"original")
		subdirs = c("las","laz","dtm_original","dtm_fusion","tile_vectors","original","projection")
		subdirs_project = paste(to_project_dir,subdirs,sep="/")


		df_template=data.frame(
				type = ""
				,dirs_old = tolower(l_proj$subdirs_project)
				,dirs_new1 = tolower(gsub(from_project_dir,to_original_dir,l_proj$subdirs_project))
				,dirs_new2 = ""
			)


		l_template = list(

			dir_project = to_project_dir
			,subdirs_project = subdirs_project
			,files_main = tolower(gsub(from_project_dir,to_original_dir,l_proj$files_main))

		)

		l_template[["dirs_types"]][["dirs_las"]] = rep(paste(to_project_dir,"las",sep="/"),length(l_proj[["dirs_types"]][["dirs_las"]]))
		l_template[["dirs_types"]][["dirs_laz"]] = rep(paste(to_project_dir,"laz",sep="/"),length(l_proj[["dirs_types"]][["dirs_laz"]]))
		l_template[["dirs_types"]][["dirs_dtm_fsn"]] = rep(paste(to_project_dir,"dtm_fusion",sep="/"),length(l_proj[["dirs_types"]][["dirs_dtm"]]))
		l_template[["dirs_types"]][["dirs_dtm_org"]] = paste(to_project_dir,"dtm_original",sep="/")
		l_template[["dirs_types"]][["dirs_tif"]] = gsub(from_project_dir,to_original_dir,l_proj[["dirs_types"]][["dirs_tif"]])
		l_template[["dirs_types"]][["dirs_img"]] = gsub(from_project_dir,to_original_dir,l_proj[["dirs_types"]][["dirs_img"]])
		l_template[["dirs_types"]][["dirs_adf"]] = gsub(from_project_dir,to_original_dir,l_proj[["dirs_types"]][["dirs_adf"]])
		l_template[["dirs_types"]][["dirs_shp"]] = gsub(from_project_dir,to_original_dir,l_proj[["dirs_types"]][["dirs_shp"]])
		l_template[["dirs_types"]][["dirs_doc"]] = gsub(from_project_dir,to_original_dir,l_proj[["dirs_types"]][["dirs_doc"]])
		l_template[["dirs_types"]][["dirs_pdf"]] = gsub(from_project_dir,to_original_dir,l_proj[["dirs_types"]][["dirs_pdf"]])

		for(i in 1:length(l_proj[["dirs_types"]])){

			if(length(l_proj[["dirs_types"]][[i]])>0){

				match_i = tolower(df_template$dirs_old) %in% tolower(l_proj[["dirs_types"]][[i]])
				df_template$dirs_new2[match_i] = l_template[["dirs_types"]][[i]]

			}

		}

browser()


	}else{

		warning(paste("Skipped",l_proj$dir_project,"no las or laz files found"))
		df_template=read.csv(text="type,dirs_old,dirs_new1,dirs_new2")

	}

	return( df_template )


}

.template_1project_old = function(l_proj, dir_out ){


	if(length(l_proj$dirs_types$dirs_laz) >=1  | length(l_proj$dirs_types$dirs_laz) >=1){

		from_project_dir  = l_proj[["dir_project"]]
		to_project_dir = file.path(dir_out, basename(l_proj[["dir_project"]]))
		to_original_dir = file.path(dir_out, basename(l_proj[["dir_project"]]),"original")
		subdirs = c("las","laz","dtm_original","dtm_fusion","tile_vectors","original","projection")
		subdirs_project = paste(to_project_dir,subdirs,sep="/")

		l_template = list(

			dir_project = to_project_dir
			,subdirs_project = subdirs_project

		)

		l_template[["dirs_types"]][["dirs_las"]] = rep(paste(to_project_dir,"las",sep="/"),length(l_proj[["dirs_types"]][["dirs_las"]]))
		l_template[["dirs_types"]][["dirs_laz"]] = rep(paste(to_project_dir,"laz",sep="/"),length(l_proj[["dirs_types"]][["dirs_laz"]]))
		l_template[["dirs_types"]][["dirs_dtm_fsn"]] = rep(paste(to_project_dir,"dtm_fusion",sep="/"),length(l_proj[["dirs_types"]][["dirs_dtm"]]))
		l_template[["dirs_types"]][["dirs_dtm_org"]] = paste(to_project_dir,"dtm_original",sep="/")
		l_template[["dirs_types"]][["dirs_tif"]] = gsub(from_project_dir,to_original_dir,l_proj[["dirs_types"]][["dirs_tif"]])
		l_template[["dirs_types"]][["dirs_img"]] = gsub(from_project_dir,to_original_dir,l_proj[["dirs_types"]][["dirs_img"]])
		l_template[["dirs_types"]][["dirs_adf"]] = gsub(from_project_dir,to_original_dir,l_proj[["dirs_types"]][["dirs_adf"]])
		l_template[["dirs_types"]][["dirs_shp"]] = gsub(from_project_dir,to_original_dir,l_proj[["dirs_types"]][["dirs_shp"]])
		l_template[["dirs_types"]][["dirs_doc"]] = gsub(from_project_dir,to_original_dir,l_proj[["dirs_types"]][["dirs_doc"]])
		l_template[["dirs_types"]][["dirs_pdf"]] = gsub(from_project_dir,to_original_dir,l_proj[["dirs_types"]][["dirs_pdf"]])


	}else{

		warning(paste("Skipped",l_proj$dir_project,"no las or laz files found"))

		l_template = list(dir_project = l_proj$dir_project, note = "no las or laz files found" )

	}

	return( list(original=l_proj,template=l_template ))


}

.seekFiles = function(pattern, dir){

	list.files(dir, pattern=pattern, full.names=T,recursive = T,ignore.case = T)

}



oRganizeRS("E:\\data","E:\\temp")

