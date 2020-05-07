library(shiny)
library(shinyFiles)
library(ggplot2)  # for the diamonds dataset
library(DT)

if (interactive()) {

	ui <- fluidPage(

		sidebarLayout(
			sidebarPanel(
				actionButton("get_folder", "Select Project")
			)
			,mainPanel(
				verbatimTextOutput("one_proj")
			)
		)

	)

	server <- function(input, output) {

		proj_dir = observeEvent(input$get_folder, {

			output$one_proj = renderText(choose.dir(caption = "Select RS Project"))

		})


	}

	shinyApp(ui, server)
}
