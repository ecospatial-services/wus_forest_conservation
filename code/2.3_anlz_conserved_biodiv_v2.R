# This script R script computes, summarizes, and visualizes the amount of plant and animal species habitat that is currently protected 
# and that would be protected if preservation targets were met using several priority rankings w/ and w/o considering forest vulnerability. 
# Author: Logan Berner, EcoSpatial Services L.L.C.
# Date: 2021-06-06
rm(list=ls())
setwd('A:/research/ecospatial_services/wus_forest_conservation/')
require(raster)
require(data.table)
require(ggplot2)
require(ggpubr)
require(viridis)
require(dplyr)

quantiles_95 <- function(x) {
  r <- quantile(x, probs=c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}


# LOAD FILES =============================================================================================
cons.add.pxl.dt <- fread('output/wus_forest_preservation_priority_additions_to_reach_targets_by_pxl.csv')
tree.sp.pxl.dt <- fread('output/wus_tree_sp_habitat_by_pxl.csv')
animal.sp.files <- list.files('gis_data/gap_species_habitat/species_habitat_extracts/', full.names = T)
pxl.dt <- fread('output/wus_geospatial_data_by_pxl.csv')

# focus only on the scenarios that consider forestland protected if GAP 1 or 2 (i.e., not only GAP 1)
cons.add.pxl.dt <- cons.add.pxl.dt[gap.status == 2]

# drop some unneaded columns
cons.add.pxl.dt <- cons.add.pxl.dt[, c('state.name', 'owner.abb', 'area.km2','gap.ira','gap.status') :=  NULL]
cons.add.pxl.dt[, status := 'proposed']


# COMBINE CURRENTLY CONSERVED AND HIGH preservation PRIORITY LANDS ==================================================
# identify currently protected forest
cpa.pxl.dt <- pxl.dt[forest.extent == 1 & gap.code <= 2]

# expand grid to join CPA with preservation
priorities <- unique(cons.add.pxl.dt$priority)
targets <- unique(cons.add.pxl.dt$target)
mask.high.vuln <- c('no','yes')
pxls <- cpa.pxl.dt$pxl.id
cpa.expanded.dt <- data.table(expand.grid(pxl.id =  pxls, target = targets, priority = priorities, vuln.mask = mask.high.vuln))
cpa.expanded.dt[, status := 'current']

# join current and high priority preservation areas
cons.pxl.dt <- rbind(cpa.expanded.dt, cons.add.pxl.dt)
  

# TREE SPECIES HABITAT =============================================================================================

# mask to forest
tree.sp.pxl.dt$forest.extent <- pxl.dt$forest.extent[match(tree.sp.pxl.dt$pxl.id, pxl.dt$pxl.id)]
tree.sp.pxl.dt <- tree.sp.pxl.dt[forest.extent == 1]

# compute total forest habitat area for each species 
tree.sp.pxl.dt[, forest.habitat.km2 := .N, by = 'species.code']

# join preservation priorities with tree habitat pxls
tree.sp.dt <- cons.pxl.dt[tree.sp.pxl.dt, on = 'pxl.id', allow.cartesian=TRUE]
tree.sp.dt <- tree.sp.dt[is.na(priority) == F]

# compute amount of habitat conserved under each preservation priority and target
tree.sp.habitat.dt <- tree.sp.dt[, .(protected.forest.habitat.km2 = .N, forest.habitat.km2 = first(forest.habitat.km2)), by = c('target','priority','vuln.mask','species.code','taxa','kingdom','name.common','name.scientific')]
tree.sp.habitat.dt[, protected.forest.habitat.pcnt := protected.forest.habitat.km2 / forest.habitat.km2 * 100]

tree.sp.habitat.current.dt <- tree.sp.dt[status == 'current', .(protected.forest.habitat.km2 = .N, forest.habitat.km2 = first(forest.habitat.km2)), by = c('target','priority','vuln.mask','species.code','taxa','kingdom','name.common','name.scientific')]
tree.sp.habitat.current.dt[, protected.forest.habitat.pcnt := protected.forest.habitat.km2 / forest.habitat.km2 * 100]

# VERTEBRATE SPECIES HABITAT =============================================================================================
animal.sp.habitat.lst <- list()
animal.sp.habitat.current.lst <- list()

for (i in 1:length(animal.sp.files)){
  sp.dt <- fread(animal.sp.files[i])
  
  # mask to forest 
  sp.dt$forest.extent <- pxl.dt$forest.extent[match(sp.dt$pxl.id, pxl.dt$pxl.id)]
  sp.dt <- sp.dt[forest.extent == 1]
  
  # compute extent of forest habitat
  sp.dt[, forest.habitat.km2 := .N, by = species.code]
  
  # join sp habitat with preservation priority grid cells
  sp.dt <- cons.pxl.dt[sp.dt, on = 'pxl.id', allow.cartesian=TRUE]
  sp.dt <- sp.dt[is.na(priority) == F]
  
  # currently conserved habitat
  habitat.current.dt <- sp.dt[status == 'current', .(protected.forest.habitat.km2 = .N, forest.habitat.km2 = first(forest.habitat.km2)), by = c('target','priority','vuln.mask','species.code','taxa','kingdom','name.common','name.scientific')]
  habitat.current.dt[, protected.forest.habitat.pcnt := protected.forest.habitat.km2 / forest.habitat.km2 * 100]
  animal.sp.habitat.current.lst[[i]] <- habitat.current.dt
  
  # current + proposed conserved habitat 
  habitat.dt <- sp.dt[, .(protected.forest.habitat.km2 = .N, forest.habitat.km2 = first(forest.habitat.km2)), by = c('target','priority','vuln.mask','species.code','taxa','kingdom','name.common','name.scientific')]
  habitat.dt[, protected.forest.habitat.pcnt := protected.forest.habitat.km2 / forest.habitat.km2 * 100]
  animal.sp.habitat.lst[[i]] <- habitat.dt
  print(c(i, round(i/length(animal.sp.files),3), unique(sp.dt$name.common)))
}

animal.sp.habitat.dt <- rbindlist(animal.sp.habitat.lst)
animal.sp.habitat.current.dt <- rbindlist(animal.sp.habitat.current.lst)


# COMBINE TREE AND ANIMAL HABITAT DATA ====================================================================================
sp.habitat.dt <- rbind(tree.sp.habitat.dt, animal.sp.habitat.dt)
sp.habitat.dt$target <- factor(sp.habitat.dt$target, levels = c(30,50), labels = c('30% forest preservation','50% forest preservation'))
sp.habitat.dt$priority <- factor(sp.habitat.dt$priority, levels = c('forest','carbon','biodiv'), 
                                 labels = c('Preservation priority','Carbon priority','Biodiversity priority'))

sp.habitat.current.dt <- rbind(tree.sp.habitat.current.dt, animal.sp.habitat.current.dt)

fwrite(sp.habitat.dt, 'output/wus_species_forest_habitat_preserved_by_priority.csv')
fwrite(sp.habitat.current.dt, 'output/wus_species_forest_habitat_preserved_currently.csv')

# sp.habitat.dt <- fread('output/wus_species_forest_habitat_preserved_by_priority.csv')
# sp.habitat.current.dt <- fread('output/wus_species_forest_habitat_preserved_currently.csv')

# EXTRACT DATA FOR A FEW SPECIES OF INTEREST ================================================================================
sp.habitat.dt <- setorder(sp.habitat.dt, target, priority, vuln.mask, taxa, name.common)

sp.habitat.current.dt[name.scientific == 'Canis lupus']$protected.forest.habitat.pcnt
sp.habitat.dt[name.scientific == 'Canis lupus']

sp.habitat.current.dt[name.scientific == 'Lynx canadensis']$protected.forest.habitat.pcnt # canadian lynx
sp.habitat.dt[name.scientific == 'Lynx canadensis'] # canadian lynx


sp.habitat.current.dt[name.scientific == 'Brachyramphus marmoratus']$protected.forest.habitat.pcnt # marbled murlet
sp.habitat.dt[name.scientific == 'Brachyramphus marmoratus'] # marbled murlet

sp.habitat.current.dt[name.scientific == 'Strix occidentalis']$protected.forest.habitat.pcnt # spotted owl
sp.habitat.dt[name.scientific == 'Strix occidentalis'] # spotted owl



# SUMMARIZE ACROSS priorities, TARGETS, AND TAXA ============================================================================
taxa.habitat.smry.dt <- sp.habitat.dt[, .(n.sp = .N,
                                          protected.forest.habitat.pcnt.p500 = quantile(protected.forest.habitat.pcnt, 0.5), 
                                          protected.forest.habitat.pcnt.p025 = quantile(protected.forest.habitat.pcnt, 0.025),
                                          protected.forest.habitat.pcnt.p975 = quantile(protected.forest.habitat.pcnt, 0.975)),
                                      by = c('target','priority','vuln.mask','taxa','kingdom')]
setorder(taxa.habitat.smry.dt, target, priority, vuln.mask)
taxa.habitat.smry.dt

# currently protected (data from take one scenario beacuse here they are all the same)
taxa.habitat.current.smry.dt <- sp.habitat.current.dt[target == 30 & priority == 'forest' & vuln.mask == 'no',
                                                      .(protected.forest.habitat.pcnt.p500 = quantile(protected.forest.habitat.pcnt, 0.5), 
                                                        protected.forest.habitat.pcnt.p025 = quantile(protected.forest.habitat.pcnt, 0.025),
                                                        protected.forest.habitat.pcnt.p975 = quantile(protected.forest.habitat.pcnt, 0.975)),
                                      by = c('taxa','kingdom')]
taxa.habitat.current.smry.dt

# COMPUTE PERCENTAGE OF SPECIES WITH AT LEAST 30% OF THEIR HABITAT PROTECTED UNDER EACH SCENAIRO ==========================================
sp.habitat.dt[protected.forest.habitat.pcnt >= 30, gte30pcnt := 'yes']
sp.habitat.dt[protected.forest.habitat.pcnt < 30, gte30pcnt := 'no'] 

gte30pcnt.dt <- sp.habitat.dt[, .N, by = c('target','priority','vuln.mask','taxa','kingdom','gte30pcnt')]
gte30pcnt.dt[, N.taxa := sum(N), by = c('target','priority','vuln.mask','taxa','kingdom')]
gte30pcnt.dt[, pcnt.gte30pcnt := round(N/N.taxa*100)]
gte30pcnt.dt[vuln.mask == 'no' & target == '50% forest preservation' & gte30pcnt == 'yes']


# FIGURES =================================================================================

# Current habitat protection ------------------------------
taxa.habitat.current.smry.dt[, protected.forest.habitat.pcnt.p500.lab := paste0(round(protected.forest.habitat.pcnt.p500),'%')]

fig.a <- ggplot(sp.habitat.current.dt[vuln.mask == 'yes'], aes(x=taxa, y=protected.forest.habitat.pcnt, fill=taxa)) + 
            stat_summary(fun.data = quantiles_95, geom="boxplot", position = 'dodge', width = 0.75, alpha = 0.6) + 
            scale_fill_viridis(discrete = T, option = "D", name = "Taxa") + 
            geom_text(data = taxa.habitat.current.smry.dt, aes(taxa, 0, label = protected.forest.habitat.pcnt.p500.lab),  
              position = position_dodge(width = 0.8), color = 'black', size = 4, vjust = -0.5) + 
            xlab("Animal or plant taxa") + ylab('Current forest habitat preserved (%)') + 
            theme_bw() + 
            theme(legend.position = c(0.75, 0.8), legend.text=element_text(size=8), legend.title=element_text(size=12), 
                  axis.text.x = element_blank(), axis.text=element_text(size=12), axis.title=element_text(size=12))
                  # panel.grid.major = element_blank(), panel.grid.minor = element_blank())
fig.a 


# Current + high priority preservation --------------------------------
taxa.habitat.smry.dt[, protected.forest.habitat.pcnt.p500.lab := paste0(round(protected.forest.habitat.pcnt.p500),'%')]

fig.b <- ggplot(sp.habitat.dt[vuln.mask == 'yes'], aes(x=taxa, y=protected.forest.habitat.pcnt, fill=taxa)) + 
  facet_grid(target ~ priority) + 
  stat_summary(fun.data = quantiles_95, geom="boxplot", position = 'dodge', width = 0.75, alpha = 0.6) + 
  scale_fill_viridis(discrete = T, option = "D") +
  geom_text(data = taxa.habitat.smry.dt[vuln.mask == 'yes'], aes(taxa, 0, label = protected.forest.habitat.pcnt.p500.lab),  
            position = position_dodge(width = 0.8), color = 'black', size = 2.25, vjust = -0.5) + 
  xlab("Animal or plant taxa") + ylab('Potential forest habitat preserved (%)') + 
  theme_bw() + 
  theme(legend.position = 'none', legend.text=element_text(size=12), legend.title=element_text(size=12), 
        axis.text.x = element_blank(), axis.text=element_text(size=12), axis.title=element_text(size=12))
        # panel.grid.major = element_blank(), panel.grid.minor = element_blank())
fig.b

# Combined figure 
fig.combo <- ggarrange(fig.a, fig.b, labels=c('a','b'), widths = c(0.5,1), label.x = c(0.01, 0.01), label.y = 1)
fig.combo
ggsave('figures/fig7_forest_habitat_preservation.jpg', width = 8, height = 5, units = 'in')

print ("All done!!")

# END SCRIPT ===========================================================================
# ggplot(trees.sp.habitat.dt, aes(habitat.pcnt)) + facet_grid(target ~ priority) + geom_density()
# ggplot(trees.sp.habitat.dt, aes(habitat.pcnt, group = priority))  + facet_wrap('target') + geom_boxplot()
xx <- taxa.habitat.smry.dt[vuln.mask == 'no']
setorder(xx, taxa, target, priority)
xx[, protected.forest.habitat.pcnt.p500 := round(protected.forest.habitat.pcnt.p500)]
xx[priority == 'Preservation priority']       


