# FIXME
mapGenesToRownames <- basejump::mapGenesToRownames



#' Assert Formal Gene-to-Symbol Mappings
#'
#' @inherit assert
#' @export
#'
#' @param object Object class supporting `rownames`. All rownames in this object
#'   must intersect with the rownames defined in the `gene2symbol` argument.
#' @param genes `character`. Gene identifiers. Note that gene names (symbols)
#'   are also supported, but not recommended if the stable IDs can be easily
#'   provided instead.
#' @param gene2symbol `Gene2Symbol`. Gene-to-symbol mappings. Must contain
#'   `geneID` and `geneName` columns, with rownames defined. All of the `object`
#'   rownames must be defined here, otherwise the function will error.
#'
#' @examples
#' DataFrame <- S4Vectors::DataFrame
#' Gene2Symbol <- basejump::Gene2Symbol
#' 
#' object <- DataFrame(
#'     "sample1" = c(1L, 2L),
#'     "sample2" = c(3L, 4L),
#'     row.names = c("gene1", "gene2")
#' )
#' print(object)
#'
#' gene2symbol <- Gene2Symbol(
#'     object = DataFrame(
#'         geneID = c("ENSG00000000003", "ENSG00000000005"),
#'         geneName = c("TSPAN6", "TNMD"),
#'         row.names = rownames(object)
#'     )
#' )
#' print(gene2symbol)
#'
#' geneIDs <- gene2symbol[["geneID"]]
#' print(geneIDs)
#'
#' geneNames <- gene2symbol[["geneName"]]
#' print(geneNames)
#'
#' assertFormalGene2Symbol(
#'     object = object,
#'     genes = geneIDs,
#'     gene2symbol = gene2symbol
#' )
#' assertFormalGene2Symbol(
#'     object = object,
#'     genes = geneNames,
#'     gene2symbol = gene2symbol
#' )
assertFormalGene2Symbol <- function(
    object,
    genes,
    gene2symbol
) {
    assertHasRownames(object)
    assert_is_character(genes)
    assert_is_non_empty(genes)
    assert_is_all_of(gene2symbol, "Gene2Symbol")
    assert_are_identical(
        x = nrow(object),
        y = nrow(gene2symbol)
    )
    if (is.null(rownames(gene2symbol))) {
        rownames(gene2symbol) <- rownames(object)
    }
    # Map genes to object rownames, using gene2symbol.
    rownames <- mapGenesToRownames(
        object = gene2symbol,
        genes = genes
    )
    assert_is_subset(rownames, rownames(object))
}
