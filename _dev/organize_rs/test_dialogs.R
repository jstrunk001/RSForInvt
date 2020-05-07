library("svGUI")
library("svDialogs")

form <- list(
	"Name:TXT" = "John Smith",
	"Age:NUM" = 25,
	"Sex:CB" = c("male", "female"),
	"Married:CHK" = FALSE
)
dlg_form(form, "My data")$res

dlg_form(form, title = "Fill the form", message = NULL, columns = 1,
				 strip.type = TRUE,  gui = .GUI)


user <- dlg_input("Who are you?", Sys.info()["user"])$res
if (!length(user)) {# The user clicked the 'cancel' button
	cat("OK, you prefer to stay anonymous!\n")
} else {
	cat("Hello", user, "\n")
}


res <- dlg_list(month.name, multiple = TRUE)$res

if (!length(res)) {
	cat("You cancelled the choice\n")
} else {
	cat("You selected:\n")
	print(res)
}


dlg_open(title = "Select one R file", filters = dlg_filters[c("R", "All"), ])$res
# Choose several files
dlg_open(multiple = TRUE)$res

