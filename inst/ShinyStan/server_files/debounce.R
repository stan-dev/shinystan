# From: https://gist.github.com/jcheng5/6141ea7066e62cafb31c
# Returns a reactive that debounces the given expression by the given time in
# milliseconds.
#
# This is not a true debounce in that it will not prevent \code{expr} from being
# called many times (in fact it may be called more times than usual), but
# rather, the reactive invalidation signal that is produced by expr is debounced
# instead. This means that this function should be used when \code{expr} is
# cheap but the things it will trigger (outputs and reactives that use
# \code{expr}) are expensive.
debounce <- function(expr, 
                     millis, 
                     env = parent.frame(), 
                     quoted = FALSE,
                     domain = getDefaultReactiveDomain()) {
  
  force(millis)
  
  f <- exprToFunction(expr, env, quoted)
  label <- sprintf("debounce(%s)", paste(deparse(body(f)), collapse = "\n"))
  
  v <- reactiveValues(
    trigger = NULL,
    when = NULL # the deadline for the timer to fire; NULL if not scheduled
  )
  
  # Responsible for tracking when f() changes.
  observeEvent(f(), {
    # The value changed. Start or reset the timer.
    v$when <- Sys.time() + millis / 1000
  }, ignoreNULL = FALSE)
  
  # This observer is the timer. It rests until v$when elapses, then touches
  # v$trigger.
  observe({
    if (is.null(v$when))
      return()
    
    now <- Sys.time()
    if (now >= v$when) {
      v$trigger <- runif(1)
      v$when <- NULL
    } else {
      invalidateLater((v$when - now) * 1000, domain)
    }
  })
  
  # This is the actual reactive that is returned to the user. It returns the
  # value of f(), but only invalidates/updates when v$trigger is touched.
  eventReactive(v$trigger, {
    f()
  }, ignoreNULL = FALSE)
}
