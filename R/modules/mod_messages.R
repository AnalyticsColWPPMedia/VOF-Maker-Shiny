# R/modules/mod_messages.R
# Every message of the app goes through app_notify(), so they look the same in
# every tab and are hard to miss:
# - errors: a modal in the middle of the screen that has to be closed ("OK");
# - warnings: an orange card at the top that stays until it is closed;
# - confirmations: a card at the top that closes by itself after a few seconds.
# Progress bars (withProgress) use the same place at the top (styles.css).

app_notify <- function(ui, type = c("message", "default", "warning", "error"), duration = NULL,
                       session = getDefaultReactiveDomain(), ...) {
  type <- match.arg(type)
  if (type == "error") {
    showModal(modalDialog(
      title = span(class = "vof-error-title", icon("circle-xmark"), "Please check"),
      div(class = "vof-error-body", ui),
      footer = modalButton("OK"), easyClose = FALSE, size = "m"
    ), session = session)
    return(invisible())
  }
  if (type == "warning") {
    shiny::showNotification(ui, duration = NULL, closeButton = TRUE, type = "warning", session = session)
  } else {
    shiny::showNotification(ui, duration = duration %||% 5, closeButton = TRUE, type = "message", session = session)
  }
  invisible()
}
