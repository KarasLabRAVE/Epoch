
#' @param object Epoch object
#' @rdname Epoch-method
#' @export
setMethod("show", "Epoch", 
    function(object) {
        tbl <- tblData(object)
        rd <- rowData(object)
        cd <- colData(object) 
        md <- metaData(object)

        # --- Table Preview ---
        cat("Epoch Object:\n")
        .printTable(tbl, header = "Time")

        # --- rowData ---
        .printMeta(rd, "rowData")

        # --- colData ---
        .printMeta(cd, "colData")
        
        # --- metadata ---
        .printMeta(md, "metaData")

        cat("Use tblData, rowData, colData, metaData to get the data\n")
    }
)