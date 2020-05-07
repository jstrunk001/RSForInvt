library(shiny)
library(DT)
shinyApp(
	ui = fluidPage(
		DTOutput('x2')
	),
	server = function(input, output, session) {
		x = iris
		x$Date = Sys.time() + seq_len(nrow(x))
		output$x2 = renderDT(x, selection = 'none', server = F, editable = T)
	}
)
