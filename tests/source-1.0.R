#### Simple integrity tests of the system datasets

options(useFancyQuotes=FALSE)
env <- as.environment("package:datasets")
d <- ls(env) # don't want .names
for(f in d) {
    if (f == "AirPassengers") {
        cat("\n** structure of dataset ", f, "\n", sep="")
    	str(get(f, envir=env, inherits=FALSE))
    }
}
