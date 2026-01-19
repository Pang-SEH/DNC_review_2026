# Compadre / Comadre datasets
load("COMPADRE_v.6.23.5.0.RData")
load("COMADRE_v.4.23.3.1.RData")
library(data.table)
## total species with any number of populations
compadre_dt <- data.table(compadre$metadata)
compadre_dt <- compadre_dt[!is.na(Lat)]
compadre_dt <- compadre_dt[!(YearPublication == "Unpublished")] # remove one unpublished
table(compadre_dt$SpeciesAccepted)

compadre_dt$YearPublication <- as.numeric(compadre_dt$YearPublication)
min(compadre_dt$YearPublication)
max(compadre_dt$YearPublication)

# brute force calculation
compadre_accum <- NULL
sp_add <- NULL
# yr <- 1969
for(yr in 1966:2022){
  sub_dt <- compadre_dt[YearPublication <= yr]
  sp_list <- unique(sub_dt$SpeciesAccepted)
  newly_added <- 0
  for(sp in sp_list) {
    if(!sp %in% sp_add) {
      sp_add <- c(sp_add, sp)
      newly_added <- newly_added + 1
    }
  }
  new_row <- data.table(year = yr, sp_total = length(sp_list))
  compadre_accum <- rbind(compadre_accum, new_row)
}


# loop to thin out space 
library(spThin)
compadre_thin <- NULL
for(sp in unique(compadre_dt$SpeciesAccepted)) {
  sp_obs <- compadre_dt[SpeciesAccepted == sp, ]
  if(nrow(sp_obs) <= 1) {next}
  sub_thin <- thin(sp_obs, long.col = 'Lon', lat.col = 'Lat', spec.col = 'SpeciesAccepted',
                   thin.par = 5, reps = 50, max.files = 1, out.base = sp, locs.thinned.list.return = T,
                   out.dir = NULL, write.files = F, write.log.file = F, verbose = F)
  
  # keep reps with max number of occ
  rep_n <- unlist(lapply(sub_thin, nrow))
  rep_n <- which(rep_n == max(rep_n))
  sub_thin <- sub_thin[rep_n]
  
  # select rep with more recent occurrences
  sel_rep <- NULL
  for (rep in 1:length(sub_thin)) sel_rep <- c(sel_rep, mean(sp_obs$YearPublication[as.integer(rownames(sub_thin[[rep]]))]))
  sub_thin <- sub_thin[[which.max(sel_rep)]]
  
  sub_thin <- sp_obs[as.integer(rownames(sub_thin)),]
  compadre_thin <- rbind(compadre_thin, sub_thin)
}

compadre_thin <- compadre_thin[SpeciesAccepted %in% names(table(compadre_thin$SpeciesAccepted))[table(compadre_thin$SpeciesAccepted) > 2]]
## adding to compadre_accum for more than 3 spatially unique pops
# brute force calculation
sp_add <- NULL
# yr <- 1982
compadre_accum[, pop_3 := 0]
for(yr in 1966:2022){
  sub_dt <- compadre_thin[YearPublication <= yr]
  sp_list <- table(sub_dt$SpeciesAccepted)
  sp_list <- names(sp_list[sp_list >= 3])
  newly_added <- 0
  for(sp in sp_list) {
    if(!sp %in% sp_add) {
      sp_add <- c(sp_add, sp)
      newly_added <- newly_added + 1
    }
  }
  compadre_accum[year == yr, pop_3 := length(sp_list)]
}
compadre_accum

# same but for 5
sp_add <- NULL
# yr <- 1982
compadre_accum[, pop_5 := 0]
for(yr in 1966:2022){
  sub_dt <- compadre_thin[YearPublication <= yr]
  sp_list <- table(sub_dt$SpeciesAccepted)
  sp_list <- names(sp_list[sp_list >= 5])
  newly_added <- 0
  for(sp in sp_list) {
    if(!sp %in% sp_add) {
      sp_add <- c(sp_add, sp)
      newly_added <- newly_added + 1
    }
  }
  compadre_accum[year == yr, pop_5 := length(sp_list)]
}
compadre_accum

### now doing for comadre
## total species with any number of populations
comadre_dt <- data.table(comadre$metadata)
comadre_dt <- comadre_dt[!is.na(Lat)]
comadre_dt <- comadre_dt[!(YearPublication == "Unpublished")] # remove one unpublished
table(comadre_dt$SpeciesAccepted)

comadre_dt$YearPublication <- as.numeric(comadre_dt$YearPublication)
min(comadre_dt$YearPublication)
max(comadre_dt$YearPublication)

# brute force calculation
comadre_accum <- NULL
sp_add <- NULL
# yr <- 1969
for(yr in 1966:2022){
  sub_dt <- comadre_dt[YearPublication <= yr]
  sp_list <- unique(sub_dt$SpeciesAccepted)
  newly_added <- 0
  for(sp in sp_list) {
    if(!sp %in% sp_add) {
      sp_add <- c(sp_add, sp)
      newly_added <- newly_added + 1
    }
  }
  new_row <- data.table(year = yr, sp_total = length(sp_list))
  comadre_accum <- rbind(comadre_accum, new_row)
}


# loop to thin out space 
library(spThin)
comadre_thin <- NULL
for(sp in unique(comadre_dt$SpeciesAccepted)) {
  sp_obs <- comadre_dt[SpeciesAccepted == sp, ]
  if(nrow(sp_obs) <= 1) {next}
  sub_thin <- thin(sp_obs, long.col = 'Lon', lat.col = 'Lat', spec.col = 'SpeciesAccepted',
                   thin.par = 5, reps = 50, max.files = 1, out.base = sp, locs.thinned.list.return = T,
                   out.dir = NULL, write.files = F, write.log.file = F, verbose = F)
  
  # keep reps with max number of occ
  rep_n <- unlist(lapply(sub_thin, nrow))
  rep_n <- which(rep_n == max(rep_n))
  sub_thin <- sub_thin[rep_n]
  
  # select rep with more recent occurrences
  sel_rep <- NULL
  for (rep in 1:length(sub_thin)) sel_rep <- c(sel_rep, mean(sp_obs$YearPublication[as.integer(rownames(sub_thin[[rep]]))]))
  sub_thin <- sub_thin[[which.max(sel_rep)]]
  
  sub_thin <- sp_obs[as.integer(rownames(sub_thin)),]
  comadre_thin <- rbind(comadre_thin, sub_thin)
}

comadre_thin <- comadre_thin[SpeciesAccepted %in% names(table(comadre_thin$SpeciesAccepted))[table(comadre_thin$SpeciesAccepted) > 2]]
## adding to comadre_accum for more than 3 spatially unique pops
# brute force calculation
sp_add <- NULL
# yr <- 1982
comadre_accum[, pop_3 := 0]
for(yr in 1966:2022){
  sub_dt <- comadre_thin[YearPublication <= yr]
  sp_list <- table(sub_dt$SpeciesAccepted)
  sp_list <- names(sp_list[sp_list >= 3])
  newly_added <- 0
  for(sp in sp_list) {
    if(!sp %in% sp_add) {
      sp_add <- c(sp_add, sp)
      newly_added <- newly_added + 1
    }
  }
  comadre_accum[year == yr, pop_3 := length(sp_list)]
}
comadre_accum

# same but for 5
sp_add <- NULL
# yr <- 1982
comadre_accum[, pop_5 := 0]
for(yr in 1966:2022){
  sub_dt <- comadre_thin[YearPublication <= yr]
  sp_list <- table(sub_dt$SpeciesAccepted)
  sp_list <- names(sp_list[sp_list >= 5])
  newly_added <- 0
  for(sp in sp_list) {
    if(!sp %in% sp_add) {
      sp_add <- c(sp_add, sp)
      newly_added <- newly_added + 1
    }
  }
  comadre_accum[year == yr, pop_5 := length(sp_list)]
}
comadre_accum

comadre_accum[, group := 'COMADRE Animal MPMs']
compadre_accum[, group := 'COMPADRE Plant MPMs']

# remotes::install_github("padrinoDB/Rpadrino")
# library(Rpadrino)
# pdb <- pdb_download(save = FALSE)
# pdb$Metadata

accum_long <- melt(rbind(compadre_accum, comadre_accum), id.vars = c("year", 'group'))
library(ggplot2)
label_data <- data.frame(
  group = "COMADRE Animal MPMs",             # the facet you want to annotate
  x = 1975,                     # adjust x position
  y = 70,                       # adjust y position
  label = "Maguire\n(1973)"
)
ggplot(accum_long, aes(x = year, y = value, colour = variable)) +
  geom_hline(yintercept = 0, colour = 'grey5', linetype = 'dotted') +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = 1973, colour = 'grey20', linetype = 'dashed') +
  # geom_vline(xintercept = 2014, colour = 'grey20', linetype = 'dashed') +
  xlab('Year') + ylab('Number of species') +
  scale_colour_manual(values = c("#00A087FF", "#3C5488FF","#E64B35FF"), 
                      labels = c('≥ 1', '≥ 3', '≥ 5'),
                      name = 'Spatially unqiue\npopulations') +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 11),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = c(0.23, 0.75),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.key = element_rect(fill = "transparent", colour = NA),
        axis.line = element_line(color = "black"), # Add custom axe
        axis.line.y.right = element_blank(),       # Remove right axis line
        axis.line.x.top = element_blank()) +
  facet_wrap(~group, scales = 'free') +
  geom_text(data = label_data,
            aes(x = x, y = y, label = label),
            inherit.aes = FALSE,
            hjust = 0)

ggsave("Species with spatial populations data v2.svg", device = 'svg', plot = last_plot(), path = getwd(), 
       width = 13, height = 8, units = "cm", dpi = 300)
