library(shiny)
library(DT)
shinyApp(
	ui = fluidPage(DT::dataTableOutput('x1'), verbatimTextOutput('x2')),

	server = function(input, output) {
		# create a character vector of shiny inputs
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

		# a sample data frame
		res = data.frame(
			v1 = shinyInput(numericInput, 100, 'v1_', value = 0),
			v2 = shinyInput(checkboxInput, 100, 'v2_', value = TRUE),
			v3 = rnorm(100),
			v4 = sample(LETTERS, 100, TRUE),
			stringsAsFactors = FALSE
		)

		# render the table containing shiny inputs
		output$x1 = DT::renderDataTable(
			res, server = FALSE, escape = FALSE, selection = 'none', options = list(
				preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
				drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
			)
		)
		# print the values of inputs
		output$x2 = renderPrint({
			data.frame(v1 = shinyValue('v1_', 100), v2 = shinyValue('v2_', 100))
		})
	}
)
