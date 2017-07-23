## This script downloads the expression data for all breast cancer samples
## from GDC/TCGA, and filters them to have only the 

library("TCGAbiolinks")
library('biomaRt')


query <- GDCquery(project = "TCGA-BRCA",
    data.category = "Transcriptome Profiling",
    data.type = "Gene Expression Quantification",
    workflow.type = "HTSeq - Counts"
)

GDCdownload(query)
data <- GDCprepare(query)

mart <- useDataset("hsapiens_gene_ensembl", useMart("ensembl"))

genes <- rownames(data)
symbols <- getBM(filters= "ensembl_gene_id", 
    attributes= c("ensembl_gene_id","hgnc_symbol"), 
    values = genes, 
    mart= mart)

ind_nchar <- as.logical(nchar(symbols[["hgnc_symbol"]]))
symbols <- symbols[ind_nchar, ]
data <- data[symbols[["ensembl_gene_id"]], ]
rownames(data) <- symbols[["hgnc_symbol"]]

ind_tumor <- colData(data)[["definition"]]== "Primary solid Tumor"
data <- data[, ind_tumor]

pam50_genes <- c(
    "ACTR3B", 
    "ANLN", 
    "BAG1", 
    "BCL2", 
    "BIRC5", 
    "BLVRA", 
    "CCNB1", 
    "CCNE1", 
    "CDC20", 
    "CDC6", 
    "CDH3", 
    "CENPF", 
    "CEP55", 
    "CXXC5", 
    "EGFR", 
    "ERBB2", 
    "ESR1", 
    "EXO1", 
    "FGFR4", 
    "FOXA1", 
    "FOXC1", 
    "GPR160", 
    "GRB7", 
    "KIF2C", 
    "KRT14", 
    "KRT17", 
    "KRT5", 
    "MAPT", 
    "MDM2", 
    "MELK", 
    "MIA", 
    "MKI67", 
    "MLPH", 
    "MMP11", 
    "MYBL2", 
    "MYC", 
    "NAT1", 
    "NDC80", 
    "NUF2", 
    "ORC6L", 
    "PGR", 
    "PHGDH", 
    "PTTG1", 
    "RRM2", 
    "SFRP1", 
    "SLC39A6", 
    "TMEM45B", 
    "TYMS", 
    "UBE2C", 
    "UBE2T"
)

pam50_genes <- intersect(pam50_genes, symbols[["hgnc_symbol"]])

clinical_cols <- c("subtype_ER.Status", "subtype_PR.Status", 
    "subtype_HER2.Final.Status", 
    "subtype_Integrated.Clusters..with.PAM50."
)

subtypes <- colData(data)[, clinical_cols]
ind_has_subtypes <- sapply(seq_len(nrow(subtypes)), function(i) {
    all(!is.na(subtypes[i, ]))
})


data <- data[, ind_has_subtypes]

tcga_brca_clinical <- colData(data)
tcga_brca_clinical <- tcga_brca_clinical[, clinical_cols]
colnames(tcga_brca_clinical) <- gsub("subtype_", "", colnames(tcga_brca_clinical))

stypes <- c("ER.Status","PR.Status","HER2.Final.Status")

tcga_brca_clinical[, stypes] <- lapply(tcga_brca_clinical[, stypes], 
    function(col) {
        col <- as.character(col)
        ifelse(col %in% c("Positive", "Negative"), col, NA)
    }
)


tcga_brca_clinical <- as.data.frame(tcga_brca_clinical)
tcga_pam50_expression <- assay(data, "HTSeq - Counts")[pam50_genes, ]


devtools::use_data(tcga_pam50_expression, overwrite = TRUE)
devtools::use_data(tcga_brca_clinical, overwrite = TRUE)
