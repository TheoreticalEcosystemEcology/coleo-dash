#### Generic functions used by the shiny append

compute_desc_comm <- function(dat, sites, year){

  comm_df <- make_comm(dat, year)

  rich_sp <- vegan::specnumber(comm_df)
  div <- vegan::diversity(comm_df)
  eveness <- vegan::diversity(comm_df)/log(vegan::specnumber(comm_df))
  geom_df <- data.frame(site_code = names(eveness), eveness, rich_sp, div)
  geom_sf <- merge(sites, geom_df, by=c("site_code"), all.y=TRUE)

  return(geom_sf)

}


make_comm <- function(dat, year){

  dat <- dat %>% filter(date_obs == year) %>% select(site_code,date_obs,taxa,count) %>% group_by(site_code,date_obs, taxa) %>% summarise(count=sum(count))
  comm_df <- reshape2::dcast(dat, site_code ~ taxa, value.var="count", fill = 0, fun.aggregate=sum, drop = TRUE, na.rm = TRUE)
  row.names(comm_df) <- comm_df[,1]
  comm_df <- as.matrix(comm_df[,-1])

  return(comm_df)

}

compute_beta_scbd <- function(dat, year){

  comm_df <- make_comm(dat, year)

  ### calculer LCBD
  comm_hell <- vegan::decostand(comm_df, method = "hell")
  lcbd <- adespatial::LCBD.comp(dist(comm_hell), sqrt.D = FALSE)
  beta <- lcbd$beta[2]

  ### Calcul SCBD
  sp_mean <- matrix(colMeans(comm_hell), nrow = nrow(comm_hell), ncol = ncol(comm_hell), byrow = TRUE)
  sij <- (comm_hell - sp_mean)^2
  SStotal <- sum(sij)
  SSj <- colSums(sij)
  SCBD <- data.frame(sp=names(SSj), val=SSj/SStotal, percent = SSj/SStotal*100)
  SCBD <- SCBD[order(SCBD$percent, decreasing = TRUE),]

  ### LCBD et SCBD pondéré par la beta
  # Pour comparer entre les années
  # LCBDbeta <- LCBD$val*beta
  # SCBDbeta <- SCBD$val*beta

  return(SCBD)

}


compute_beta <- function(dat, year){

  comm_df <- make_comm(dat, year)

  ### calculer LCBD
  comm_hell <- vegan::decostand(comm_df, method = "hell")
  lcbd <- adespatial::LCBD.comp(dist(comm_hell), sqrt.D = FALSE)
  lcbd$beta[2]
}


compute_beta_lcbd <- function(dat, year){

  comm_df <- make_comm(dat, year)

  ### calculer LCBD
  comm_hell <- vegan::decostand(comm_df, method = "hell")
  lcbd <- adespatial::LCBD.comp(dist(comm_hell), sqrt.D = FALSE)
  beta <- lcbd$beta[2]
  LCBD <- data.frame(site=row.names(comm_hell) ,val=lcbd$LCBD, percent = lcbd$LCBD*100)
  LCBD <- LCBD[order(LCBD$percent, decreasing = TRUE),]

  return(LCBD)

}
