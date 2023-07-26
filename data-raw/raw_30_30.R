## code to prepare `raw_30_30` dataset goes here
library(agcounter)
x = download_actgraph_test_data()
epoch = 10
sample_rate = 30
search_string = paste0("raw_", epoch, "_", sample_rate)
testfile = x$ActiLifeCounts
testfile = testfile[grepl(search_string, testfile)]
file = x$raw
file = file[grepl(search_string, file)]

# need to reorder
df = readr::read_csv(file, col_names = FALSE)
colnames(df) = c("Y", "X", "Z")
df = df[, c("X", "Y", "Z")]
df = round(df * 10000)
raw_30_30 = df
usethis::use_data(raw_30_30, overwrite = TRUE, compress = "xz")
