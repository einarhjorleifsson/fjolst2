lesa.flokkar <-
function (ranfiskur, lenfl = NULL, synis.id = NULL, fjoldi = T, 
    total = T, flokk.id = NULL, oracle = fjolstOracle) 
{
    if (!oracle) {
        result <- fflokkar[, c("synis.id", "flokk.id", "ranfiskur", 
            "lenfl", "fj.fmaga", "fj.omelt", "fj.tomra", "fj.aelt", 
            "faerslunumer")]
        result <- result[result$ranfiskur == ranfiskur, ]
        if (!is.null(synis.id)) 
            result <- result[!is.na(match(result$synis.id, synis.id)), 
                ]
        if (!is.null(lenfl)) 
            result <- result[!is.na(match(result$lenfl, lenfl)), 
                ]
        i <- is.na(result$fj.aelt)
        if (any(i)) 
            result$fj.aelt[i] <- 0
        i <- is.na(result$fj.omelt)
        if (any(i)) 
            result$fj.omelt[i] <- 0
        print(nrow(result))
        cat("start total")
        if (total) {
            if (!is.null(synis.id)) 
                tmp <- fhopar[!is.na(match(fhopar$flokk.id, result$flokk.id)), 
                  ]
            else tmp <- fhopar
            x <- apply.shrink(tmp$thyngd, tmp$flokk.id, sum, 
                names = c("flokk.id", "total"))
            result <- fjolst:::join(result, x, "flokk.id", set = 0)
        }
        cat("start fjoldi")
        if (fjoldi) {
            s.id = unique(result$synis.id)
            tmp <- lesa.lengdir(s.id, ranfiskur)
            tmp1 <- lesa.numer(s.id, ranfiskur)
            tmp <- Skala.med.toldum(tmp, tmp1)
            tmp <- tmp[, c("synis.id", "lengd", "fj.alls")]
            tmp$lenfl <- as.numeric(cut(tmp$lengd, old.stomach.breaks$breaks))
            tmp <- apply.shrink(tmp$fj.alls, list(tmp$synis.id, 
                tmp$lenfl), sum, names = c("synis.id", "lenfl", 
                "fjoldi"))
            tmp$index <- paste(tmp$synis.id, tmp$lenfl)
            result$index <- paste(result$synis.id, result$lenfl)
            tmp <- tmp[!is.na(match(tmp$index, result$index)), 
                ]
            result <- fjolst:::join(result, tmp[, c("index", "fjoldi")], 
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
    nafn <- c("synis.id", "flokk.id", "ranfiskur", "lenfl", "fj.fmaga", 
        "fj.omelt", "fj.tomra", "fj.aelt", "faerslunumer")
    nafn1 <- c("synis_id", "flokk_id", "ranfiskur", "len_fl", 
        "fj_fmaga", "fj_omelt", "fj_tomra", "fj_aelt", "faerslunumer")
    dn <- nafn1
    dn2 <- nafn
    dn1 <- paste(dn, collapse = ",")
    skipun <- paste("select", dn1, "from faeda.f_flokkar where ranfiskur in (", 
        ranf, ")")
    if (!is.null(synis.id)) 
        skipun <- paste(skipun, "and synis_id in (", paste(synis.id, 
            collapse = ","), ")")
    if (!is.null(flokk.id)) 
        skipun <- paste(skipun, "and flokk_id > ", flokk.id)
    if (!is.null(lenfl)) 
        skipun <- paste(skipun, "and lenfl in (", paste(lenfl, 
            collapse = ","), ")")
    x <- run.sqlskipun(skipun, synis.id, "synis_id")
     ind <- match(dn, nafn1)
    nafn <- nafn[ind]
    if (total) {
        print("total")
        table <- "faeda.f_hopar"
        skipun <- paste("select sum(NVL(thyngd,0)) total,flokk_id from", 
            table)
        y <- run.sqlskipun(skipun, x$flokk.id, "flokk_id", "where", "group by flokk_id")
        x <- fjolst:::join(x, y, "flokk.id", set = 0)
    }
    row.names(x) <- x$flokk.id
    print("fjoldi")
    if (fjoldi) 
        x <- fjoldi.i.flokkum(x, ranfiskur,oracle=oracle)
    i <- is.na(x$fj.aelt)
    if (any(i)) 
        x$fj.aelt[i] <- 0
    i <- is.na(x$fj.omelt)
    if (any(i)) 
        x$fj.omelt[i] <- 0
    return(x)
}
