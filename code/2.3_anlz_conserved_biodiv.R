# This script ...
# Author: Logan Berner, EcoSpatial Services L.L.C.
# Date: 2021-05-02
rm(list=ls())
setwd('A:/research/ecospatial_services/wus_forest_conservation/')
require(raster)
require(data.table)
require(ggplot2)
require(ggpubr)
require(viridis)
require(dplyr)

quantiles_95 <- function(x) {
  r <- quantile(x, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

# LOAD FILES =============================================================================================
cons.add.pxl.dt <- fread('output/wus_forest_conservation_scenario_additions_to_reach_goals_by_pxl.csv')
tree.sp.pxl.dt <- fread('output/wus_tree_sp_habitat_by_pxl.csv')
animal.sp.files <- list.files('gis_data/gap_species_habitat/species_habitat_extracts/', full.names = T)
pxl.dt <- fread('output/wus_geospatial_data_by_pxl.csv')

# drop some unneaded columns
cons.add.pxl.dt <- cons.add.pxl.dt[, c('state.name', 'owner.abb', 'area.km2') :=  NULL]
cons.add.pxl.dt[, status := 'proposed']

# COMBINE CURRENTLY CONSERVED AND HIGH CONSERVATION PRIORITY LANDS ==================================================
# identify currently protected forest
cpa.pxl.dt <- pxl.dt[forest.extent == 1 & gap.code <= 2]

# expand grid to join CPA with conservation addstate.name, owner.abb, area.km2) =state.name, owner.abb, area.km2) =
scenarios <- unique(cons.add.pxl.dt$scenario)
goals <- unique(cons.add.pxl.dt$goal)
mask.high.vuln <- c('no','yes')
pxls <- cpa.pxl.dt$pxl.id
cpa.expanded.dt <- data.table(expand.grid(pxl.id =  pxls, goal = goals, scenario = scenarios))
cpa.expanded.dt[, status := 'current']

# join current and high priority conservation areas
cons.pxl.dt <- rbind(cpa.expanded.dt, cons.add.pxl.dt)
  

# TREE SPECIES HABITAT =============================================================================================

# compute total forest habitat area for each species 
tree.sp.pxl.dt[, forest.habitat.km2 := .N, by = 'species.code']

# join conservation scenarios with tree habitat pxls
tree.sp.dt <- cons.pxl.dt[tree.sp.pxl.dt, on = 'pxl.id', allow.cartesian=TRUE]
tree.sp.dt <- tree.sp.dt[is.na(scenario) == F]

# compute amount of habitat conserved under each conservation scenario and target
tree.sp.habitat.dt <- tree.sp.dt[, .(habitat.km2 = .N, forest.habitat.km2 = first(forest.habitat.km2)), by = c('goal','scenario','species.code','taxa','kingdom','name.common','name.scientific')]
tree.sp.habitat.dt[, habitat.pcnt := habitat.km2 / forest.habitat.km2 * 100]

tree.sp.habitat.current.dt <- tree.sp.dt[status == 'current', .(habitat.km2 = .N, forest.habitat.km2 = first(forest.habitat.km2)), by = c('goal','scenario','species.code','taxa','kingdom','name.common','name.scientific')]
tree.sp.habitat.current.dt[, habitat.pcnt := habitat.km2 / forest.habitat.km2 * 100]

# VERTEBRATE SPECIES HABITAT =============================================================================================
animal.sp.habitat.lst <- list()
animal.sp.habitat.current.lst <- list()

for (i in 1:length(animal.sp.files)){
  sp.dt <- fread(animal.sp.files[i])
  sp.dt[, forest.habitat.km2 := .N, by = species.code]
  sp.dt <- cons.pxl.dt[sp.dt, on = 'pxl.id', allow.cartesian=TRUE]
  sp.dt <- sp.dt[is.na(scenario) == F]
  
  # currently conserved habitat
  sp.habitat.current.dt <- sp.dt[status == 'current', .(habitat.km2 = .N, forest.habitat.km2 = first(forest.habitat.km2)), by = c('goal','scenario','species.code','taxa','kingdom','name.common','name.scientific')]
  sp.habitat.current.dt[, habitat.pcnt := habitat.km2 / forest.habitat.km2 * 100]
  animal.sp.habitat.current.lst[[i]] <- sp.habitat.current.dt
  
  # current + proposed conserved habatit 
  sp.habitat.dt <- sp.dt[, .(habitat.km2 = .N, forest.habitat.km2 = first(forest.habitat.km2)), by = c('goal','scenario','species.code','taxa','kingdom','name.common','name.scientific')]
  sp.habitat.dt[, habitat.pcnt := habitat.km2 / forest.habitat.km2 * 100]
  animal.sp.habitat.lst[[i]] <- sp.habitat.dt
  print(i)
}

animal.sp.habitat.dt <- rbindlist(animal.sp.habitat.lst)
animal.sp.habitat.current.dt <- rbindlist(animal.sp.habitat.current.lst)


# COMBINE TREE AND ANIMAL HABITAT DATA ====================================================================================
sp.habitat.dt <- rbind(tree.sp.habitat.dt, animal.sp.habitat.dt)
sp.habitat.dt$goal <- factor(sp.habitat.dt$goal, levels = c(30,50), labels = c('30% forest conservation','50% forest conservation'))
sp.habitat.dt$scenario <- factor(sp.habitat.dt$scenario, levels = c('forest','carbon','biodiv'), 
                                 labels = c('Conservation priority','Carbon priority','Biodiversity priority'))

sp.habitat.current.dt <- rbind(tree.sp.habitat.current.dt, animal.sp.habitat.current.dt)

fwrite(sp.habitat.dt, 'output/wus_forest_habitat_conserved_by_scenario_for_species.csv')
fwrite(sp.habitat.current.dt, 'output/wus_forest_habitat_conserved_currently_for_species.csv')


# SUMMARIZE ACROSS SCENARIOS, TARGETS, AND TAXA ============================================================================
taxa.habitat.smry.dt <- sp.habitat.dt[, .(habitat.pcnt.p500 = quantile(habitat.pcnt, 0.5), 
                                                    habitat.pcnt.p025 = quantile(habitat.pcnt, 0.025),
                                                    habitat.pcnt.p975 = quantile(habitat.pcnt, 0.975)),
                                                by = c('goal','scenario','taxa','kingdom')]
setorder(taxa.habitat.smry.dt, goal, scenario)
taxa.habitat.smry.dt

# currently protected 
taxa.habitat.current.smry.dt <- sp.habitat.current.dt[, .(habitat.pcnt.p500 = quantile(habitat.pcnt, 0.5), 
                                          habitat.pcnt.p025 = quantile(habitat.pcnt, 0.025),
                                          habitat.pcnt.p975 = quantile(habitat.pcnt, 0.975)),
                                      by = c('taxa','kingdom')]
setorder(taxa.habitat.current.smry.dt, goal, scenario)
taxa.habitat.current.smry.dt

# FIGURES =================================================================================

# Current habitat protection ------------------------------
taxa.habitat.current.smry.dt[, habitat.pcnt.p500.lab := paste0(round(habitat.pcnt.p500),'%')]

fig.a <- ggplot(sp.habitat.current.dt, aes(x=taxa, y=habitat.pcnt, fill=taxa)) + 
            stat_summary(fun.data = quantiles_95, geom="boxplot", position = 'dodge', width = 0.75, alpha = 0.8) + 
            scale_fill_viridis(discrete = T, option = "D", name = "Taxa") + 
            geom_text(data = taxa.habitat.current.smry.dt, aes(taxa, habitat.pcnt.p500, label = habitat.pcnt.p500.lab),  
              position = position_dodge(width = 0.8), color = 'black', size = 4, vjust = -0.5) + 
            xlab("Animal or plant taxa") + ylab('Current forest habitat conserved (%)') + 
            theme_bw() + 
            theme(legend.position = c(0.75, 0.8), legend.text=element_text(size=8), legend.title=element_text(size=12), 
                  axis.text.x = element_blank(), axis.text=element_text(size=12), axis.title=element_text(size=12))
fig.a 


# Current + high priority conservation --------------------------------
taxa.habitat.smry.dt[, habitat.pcnt.p500.lab := paste0(round(habitat.pcnt.p500),'%')]

fig.b <- ggplot(sp.habitat.dt, aes(x=taxa, y=habitat.pcnt, fill=taxa)) + 
  facet_grid(goal ~ scenario) + 
  stat_summary(fun.data = quantiles_95, geom="boxplot", position = 'dodge', width = 0.75, alpha = 0.8) + 
  scale_fill_viridis(discrete = T, option = "D") +
  geom_text(data = taxa.habitat.smry.dt, aes(taxa, habitat.pcnt.p500, label = habitat.pcnt.p500.lab),  
            position = position_dodge(width = 0.8), color = 'black', size = 2, vjust = -0.5) + 
  xlab("Animal or plant taxa") + ylab('Potential forest habitat conserved (%)') + 
  theme_bw() + 
  theme(legend.position = 'none', legend.text=element_text(size=12), legend.title=element_text(size=12), 
        axis.text.x = element_blank(), axis.text=element_text(size=12), axis.title=element_text(size=12))
fig.b

# Combined figure 
fig.combo <- ggarrange(fig.a, fig.b, labels=c('(a)','(b)'), widths = c(0.5,1), label.x = c(0.01, 0.01), label.y = 1)
fig.combo
ggsave('figures/fig5_forest_habitat_conservation.jpg', width = 8, height = 6, units = 'in')

print ("All done!!")
# END SCRIPT ===========================================================================
# ggplot(trees.sp.habitat.dt, aes(habitat.pcnt)) + facet_grid(goal ~ scenario) + geom_density()
# ggplot(trees.sp.habitat.dt, aes(habitat.pcnt, group = scenario))  + facet_wrap('goal') + geom_boxplot()
