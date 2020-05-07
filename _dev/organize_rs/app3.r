library(shiny)
library(shinyFiles)
library(ggplot2)  # for the diamonds dataset
library(DT)


	ui <- fluidPage(

		sidebarLayout(
			sidebarPanel(
				actionButton("get_folder", "Select Project")
			)
			 ,mainPanel(
			 	textOutput("one_proj")
			 	,DT::dataTableOutput("subdirs")
			 	#,DT::datatable("subdirs")
			 )
		)

	)

	server <- function(input, output) {

		# proj_dir = observeEvent(input$get_folder, {
		#
		# 	renderText(choose.dir(caption = "Select RS Project"))
		#
		# })

		proj_dir = observeEvent(input$get_folder, {

			one_proj_in = choose.dir(caption = "Select RS Project")
			output$one_proj = renderText(one_proj_in)
			output$subdirs = DT::renderDataTable(data.frame(dirs = list.dirs(one_proj_in,recursive = F)))

		})
		# output$one_proj <- DT::renderDataTable({
		#
		# 	inPath <- proj_dir
		#
		# 	if (is.null(inPath))
		# 		return(NULL)
		#
		# 	one_scan = .scan_1project(inPath)
		#
		# 	.template_1project(one_scan, "c:\\temp\\")
		#
		# })
	}

	shinyApp(ui, server)

