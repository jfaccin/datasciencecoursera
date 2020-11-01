pollutantmean <- function(dir, pollutant, id = 1:332) {
    files <- list.files(dir, full.names = TRUE)
    df <- data.frame()
    for(i in id) {
        data <- read.csv(files[i])
        df <- rbind(df, data)
    }
    mean <- mean(df[[pollutant]], na.rm = TRUE)
    mean
}

complete <- function(dir, id = 1:332) {
    files <- list.files(dir, full.names = TRUE)
    df <- data.frame(id = integer(), nobs = integer())
    for(i in id) {
        data <- read.csv(files[i])
        count <- sum(complete.cases(data))
        element <- data.frame(id = i, nobs = count)
        df <- rbind(df, element)
    }
    df
}

corr <- function(dir, threshold = 0) {
    files <- list.files(dir, full.names = TRUE)
    vec <- vector()
    for(name in files) {
        df <- read.csv(name)
        if (sum(complete.cases(df)) > threshold) {
            cr <- cor(df$sulfate, df$nitrate, use = "complete.obs")
            vec <- append(vec, cr)
        }
    }
    vec
}