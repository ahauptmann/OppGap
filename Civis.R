# high school ranking

high <- sql("
            select hs_id, 
            ai_name,
            count(*) as n,
            avg(sat_verbal_max + sat_math_max) as sat,
            avg(cast(case when st_education_mother in ('Grade School', 'Some High School', 'High school diploma or equivalent', 'Associate or two-year Degree', 'Business or Trade school', 'Some College') then 0
            when st_education_mother in ('Bachelors or four-year degree', 'Some graduate or professional school', 'Graduate or professional degree') then 1 else NULL end as float)) as mom,
            avg(cast(case when st_education_father in ('Grade School', 'Some High School', 'High school diploma or equivalent', 'Associate or two-year Degree', 'Business or Trade school', 'Some College') then 0
            when st_education_father in ('Bachelors or four-year degree', 'Some graduate or professional school', 'Graduate or professional degree') then 1 else null end as float)) as dad,
            avg(cast(case when st_income_range_sr in ('< $10,000', '$10,000 - $20,000', '$20,000 - $30,000', '$30,000 - $40,000', '$40,000 - $50,000') then 1
            when st_income_range_sr in ('$50,000 - $60,000', '$60,000 - $70,000', '$70,000 - $80,000', '$80,000 - $100,000', '$100,000 - $120,000', '$120,000 - $140,000', '$140,000 - $160,000', '$160,000 - $180,000', '$180,000 - $200,000', '> $200,000') then 0 else null end as float)) as poor 
            from cb_analytics.file
            where round(geo_fips_cd / 1000) in (2,4,6,9,10,11,12,13,15,18,23,24,25,28,32,33,34,36,37,41,42,44,45,48,50,51,53,54)
            and sat_verbal_max is not null and cohort_yyyy = 2011 and cohort_yyyy is not null
            group by 1, 2 order by 1, 2
            ")


# get rid of missing high school names and super large high schools
ok <- !is.na(high$ai_name) & high$n < 2000
dat <- high[ok,]

par(mfrow=c(3,2), mar=c(2,2,2,1), mgp=c(1.25,0.1,0), tck=0.005, lwd=0.5, family="Helvetica Neue Light")
for (i_col in c("sat", "mom", "dad", "poor")) {
  n <- dat$n
  y <- dat[,i_col]
  plot(n,y,xlab="",ylab="",pch=20, main=i_col)
  
  ok <- !is.na(y)
  L <- loess(y ~ n, subset=ok, span=0.5)
  points(n[ok], L$fit, pch=20, col="red")
  abline(h=mean(y, na.rm=T), col="blue", lwd=2)
  
  mu <- rep(NA, nrow(dat))
  mu[ok] <- L$fit
  mu[!ok] <- mean(y, na.rm=T)
  
  yhat <- rep(NA, nrow(dat))
  yhat <- (y * n + mu * 50) / (n + 50)
  yhat[is.na(yhat)] <- mean(y, na.rm=T)
  eval(parse(text=paste0("dat[,'", i_col, "2'] <- yhat")))
}

par(mfrow=c(3,2), mar=c(2,2,2,1), mgp=c(1.25,0.1,0), tck=0.005, lwd=0.5, family="Helvetica Neue Light")
for (i_col in c("sat", "mom", "dad", "poor")) {
  n <- dat$n
  y <- dat[,paste0(i_col, "2")]
  plot(n,y,xlab="",ylab="",pch=20, main=i_col)
  
  ok <- !is.na(y)
  L <- loess(y ~ n, subset=ok, span=0.5)
  points(n[ok], L$fit, pch=20, col="red")
  abline(h=mean(y, na.rm=T), col="blue", lwd=2)
}

high <- merge(x=high, y=dat[, c("hs_id", "sat2", "mom2", "dad2", "poor2")], all.x=T, all.y=F)
for (i_col in c("sat", "mom", "dad", "poor")) {
  ok <- is.na(high[,paste0(i_col, "2")])
  eval(parse(text=paste0("high[ok,'", i_col, "2'] <- high[ok,'", i_col, "']")))
}

# one last mean value replacement for the 5 or so missing cases
for (i_col in c("sat2", "mom2", "dad2", "poor2")) {
  ok <- is.na(high[,i_col])
  mu <- sum(high$n[!ok] * high[!ok,i_col]) / sum(high$n[!ok])
  high[ok,i_col] <- mu
}

##########################################################################################################
# psat people for race

psat <- sql("
            select hs_id, 
            ai_name,
            count(*) as n,
            avg(cast(case when st_race in ('white', 'asian') then 0
            when st_race in ('black', 'hispanic', 'native', 'other') then 1 else null end as float)) as minority,
            sum(case when sat_verbal_max is not null then 1 else 0 end) as sat_takers
            from cb_analytics.file
            where round(geo_fips_cd / 1000) in (2,4,6,9,10,11,12,13,15,18,23,24,25,28,32,33,34,36,37,41,42,44,45,48,50,51,53,54)
            and psat_score_reading_max is not null and cohort_yyyy = 2011 and cohort_yyyy is not null
            group by 1, 2 order by 1, 2
            ")


# get rid of missing high school names and super large high schools
ok <- !is.na(psat$ai_name) & psat$n < 2000
dat <- psat[ok,]

par(mfrow=c(3,2), mar=c(2,2,2,1), mgp=c(1.25,0.1,0), tck=0.005, lwd=0.5, family="Helvetica Neue Light")
for (i_col in c("minority")) {
  n <- dat$n
  y <- dat[,i_col]
  plot(n,y,xlab="",ylab="",pch=20, main=i_col)
  
  ok <- !is.na(y)
  L <- loess(y ~ n, subset=ok, span=0.5)
  points(n[ok], L$fit, pch=20, col="red")
  abline(h=mean(y, na.rm=T), col="blue", lwd=2)
  
  mu <- rep(NA, nrow(dat))
  mu[ok] <- L$fit
  mu[!ok] <- mean(y, na.rm=T)
  
  yhat <- rep(NA, nrow(dat))
  yhat <- (y * n + mu * 50) / (n + 50)
  yhat[is.na(yhat)] <- mean(y, na.rm=T)
  eval(parse(text=paste0("dat[,'", i_col, "2'] <- yhat")))
}

par(mfrow=c(3,2), mar=c(2,2,2,1), mgp=c(1.25,0.1,0), tck=0.005, lwd=0.5, family="Helvetica Neue Light")
for (i_col in c("minority")) {
  n <- dat$n
  y <- dat[,paste0(i_col, "2")]
  plot(n,y,xlab="",ylab="",pch=20, main=i_col)
  
  ok <- !is.na(y)
  L <- loess(y ~ n, subset=ok, span=0.5)
  points(n[ok], L$fit, pch=20, col="red")
  abline(h=mean(y, na.rm=T), col="blue", lwd=2)
}

psat <- merge(x=psat, y=dat[, c("hs_id", "minority2")], all.x=T, all.y=F)
for (i_col in c("minority")) {
  ok <- is.na(psat[,paste0(i_col, "2")])
  eval(parse(text=paste0("psat[ok,'", i_col, "2'] <- psat[ok,'", i_col, "']")))
}

##########################################################################################################

colnames(high)[colnames(high)=="n"] <- "sat_n"
colnames(psat)[colnames(psat)=="n"] <- "psat_n"
dat <- merge(x=high, y=psat, by="hs_id", all.x=T, all.y=T)
dat <- dat[,c("hs_id", "sat_n", "psat_n", "sat2", "mom2", "dad2", "poor2", "minority2")]

dat$edu <- apply(dat[, c("mom2", "dad2")], 1, mean, na.rm=T)
colnames(dat) <- gsub("2", "", colnames(dat))

dat$sat_n[is.na(dat$sat_n)] <- 0
dat$psat_n[is.na(dat$psat_n)] <- 0
for (i in c("sat", "edu", "poor")) {
  ok <- is.na(dat[,i])
  dat[ok,i] <- sum(dat$sat_n[!ok] * dat[!ok,i]) / sum(dat$sat_n[!ok])
}
for (i in c("minority")) {
  ok <- is.na(dat[,i])
  dat[ok,i] <- sum(dat$psat_n[!ok] * dat[!ok,i]) / sum(dat$psat_n[!ok])
}

dat <- dat[,!(colnames(dat) %in% c("mom","dad"))]

write.table(dat, "hs_rollups_20130618.txt", row.names=F, quote=F, sep="\t")
