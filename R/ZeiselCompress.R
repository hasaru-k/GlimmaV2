library(Matrix)

sce <- ZeiselBrainData(ensembl=TRUE)
sce_raw <- sce

sce_filt <- sce[rowSums(counts(sce)) >= 300, ]

sce_filt_counts <- as(counts(sce_filt), "Matrix")
sce_filt_counts[sce_filt_counts <= 3] <- 0

write.csv(as.matrix(sce_filt_counts), "sce_counts.csv")
file.remove("sce_counts.csv.gz")
R.utils::gzip("sce_counts.csv", compression = 9)

write.csv(rowData(sce_filt), "sce_row_data.csv")
R.utils::gzip("sce_row_data.csv")
write.csv(colData(sce_filt), "sce_col_data.csv")
R.utils::gzip("sce_col_data.csv")

counts(sce_filt) <- sce_filt_counts
sce <- sce_filt

sce_read <- read.csv("sce_counts.csv.gz", row.names = 1, check.names = FALSE)
sce_read <- as.matrix(sce_read)
