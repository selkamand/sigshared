# Create a function that is never run, purely for the purpose of removing the note about never using assertions despite importing it
# This note occurs because we never use assertions functions within functions, but rather to create the functions of our package
remove_use_of_assertions_note <- function(){
 assertions::assert("Rubbish")
}
