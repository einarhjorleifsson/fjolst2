na.mean <-
function(v1, v2)
{
        ind <- c(1:length(v1))
        ind1 <- ind[is.na(v1) & !is.na(v2)]
        ind2 <- ind[is.na(v2) & !is.na(v1)]
        v <- (v1 + v2)/2
        if(length(ind1) > 0)
                v[ind1] <- v2[ind1]
        if(length(ind2) > 0)
                v[ind2] <- v1[ind2]
        return(v)
}
