lesa.stakir <-
function (ranfiskur, synis.id = NULL, fjoldi = T, total = T,
    flokk.id = NULL, lengd = NULL, kvarnanumer = NULL, tmptable = F,
    oracle = fjolstOracle)
{
    if (!oracle) {
        result <- ffiskar[ffiskar$ranfiskur == ranfiskur, c("synis.id",
            "flokk.id", "ranfiskur", "lengd", "fj.tomra", "fj.aelt",
            "faerslunumer", "kvarnanr", "fj.omelt", "lenfl")]
        if (!is.null(synis.id))
            result <- result[!is.na(match(result$synis.id, synis.id)),
                ]
        i <- is.na(result$fj.aelt)
        if (any(i))
            result$fj.aelt[i] <- 0
        i <- is.na(result$fj.omelt)
        if (any(i))
            result$fj.omelt[i] <- 0
        cat("start total")
        if (total) {
            if (!is.null(synis.id))
                tmp <- fhopar[!is.na(match(fhopar$flokk.id, result$flokk.id)),
                  ]
            else tmp <- fhopar
#           x <- apply.shrink(abs(tmp$thyngd), tmp$flokk.id,
#                sum, names = c("flokk.id", "total"))
            x <- apply.shrink(tmp$thyngd, tmp$flokk.id,
                              sum, names = c("flokk.id", "total"))
            result <- join(result, x, "flokk.id", set = 0)
        }
        cat("start fjoldi")
        if (fjoldi) {
            if (ranfiskur == 901 || ranfiskur == 902)
                ranfiskur <- 1
            s.id = unique(result$synis.id)
            tmp <- lesa.lengdir(s.id, ranfiskur,oracle=oracle)
            tmp1 <- lesa.numer(s.id, ranfiskur,oracle=oracle)
            tmp <- Skala.med.toldum(tmp, tmp1)
            tmp <- tmp[, c("synis.id", "lengd", "fj.alls")]
            names(tmp)[3] <- "fjoldi"
            tmp$index <- paste(tmp$synis.id, tmp$lengd)
            result$index <- paste(result$synis.id, result$lengd)
            tmp <- tmp[!is.na(match(tmp$index, result$index)),
                ]
            result <- join(result, tmp[, c("index", "fjoldi")],
                "index", set = 0)
            j <- match("index", names(result))
            result <- result[, -j]
            result$fjoldi[is.na(result$fjoldi)] <- 0
        }
        i <- is.na(result$fj.aelt)
        if (any(i))
            result$fj.aelt[i] <- 0
        return(result)
    }
    ranf <- paste(ranfiskur, collapse = ",")
    lfl <- c(5, 8, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80, 90,
        100, 120, 200)
    x <- 0
    ranf <- paste("'", ranfiskur, "'", sep = "")
    if (tmptable)
        table <- "faeda.f_fiskar_tmp"
    else table <- "faeda.f_fiskar"
    nafn <- c("synis.id", "flokk.id", "ranfiskur", "lengd", "fj.tomra",
        "fj.aelt", "faerslunumer", "kvarnanr")
    nafn1 <- c("synis_id", "flokk_id", "ranfiskur", "lengd",
        "fj_tomra", "fj_aelt", "faerslunumer", "kvarnanr")
    dn <- nafn1
    dn2 <- nafn
    dn1 <- paste(dn, collapse = ",")
    skipun <- paste("select", dn1, "from", table, "where ranfiskur in (",
        ranf, ")")
    if (!is.null(lengd))
        skipun <- paste(skipun, "and lengd in (", paste(lengd,
            collapse = ","))
    if (!is.null(kvarnanumer))
        skipun <- paste(skipun, "and kvarnanr in (", paste(kvarnanumer,
            collapse = ","))
    x <- run.sqlskipun(skipun, synis.id, "synis_id")
    ind <- match(dn, nafn1)
    nafn <- nafn[ind]
    if (total) {
        if (tmptable)
            table <- "faeda.f_hopar_tmp"
        else table <- "faeda.f_hopar"
        skipun <- paste("select sum(NVL(thyngd,0)) total,flokk_id from",
            table)
        y <- run.sqlskipun(skipun, x$flokk.id, "flokk_id", "where", "group by flokk_id")
        x <- join(x, y, "flokk.id", set = 0)
    }
    x <- x[match(unique(x$flokk.id),x$flokk.id),]
    row.names(x) <- x$flokk.id
    print("fjoldi")
    if (fjoldi)
        x <- fjoldi.i.flokkum(x, ranfiskur, option = 2,oracle=oracle)
    i <- is.na(x$fj.aelt)
    if (any(i))
        x$fj.aelt[i] <- 0
    return(x)
}
