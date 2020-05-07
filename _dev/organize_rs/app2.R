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
			)
		)

	)

	server <- function(input, output) {

		proj_dir = observeEvent(input$get_folder, {

			output$one_proj = choose.dir(caption = "Select RS Project")

		})


	}

	shinyApp(ui, server)

