# This script summarizes who owns forestlands with high preservation priority across the western USA
# Author: Logan Berner, EcoSpatial Services L.L.C.
# Date: 2021-05-30
rm(list=ls())
setwd('A:/research/ecospatial_services/wus_forest_conservation/')
require(raster)
require(data.table)
require(ggplot2)
require(ggpubr)
require(viridis)
require(dplyr)

# LOAD FILES =============================================================================================
cons.add.pxl.dt <- fread('output/wus_forest_preservation_priority_additions_to_reach_targets_by_pxl.csv')


# COMPUTE OWNERSHIP OF HIGH PRESERVATION PRIORITY LANDS BY STATE =================================================
# NOTE: here, gaps.status is the minimum gap status (i.e., gaps.status == 2 considers lands protected under either gap 1 or 2)
forest.cnpa.ownr.state.smry.dt <- cons.add.pxl.dt[, .(priority.area.km2 = .N), by = c('gap.status','priority','target','vuln.mask','state.name','owner.abb')]
forest.cnpa.ownr.state.smry.dt[, total.area.km2 := sum(priority.area.km2), by = c('gap.status','priority','target','vuln.mask','state.name')]
forest.cnpa.ownr.state.smry.dt[, priority.area.pcnt := round(priority.area.km2 / total.area.km2 * 100)]
forest.cnpa.ownr.state.smry.dt[, priority.area.Mha := priority.area.km2 / 10^4]

forest.cnpa.ownr.state.smry.dt$target <- factor(forest.cnpa.ownr.state.smry.dt$target, levels = c(30,50), labels = c('30% forest preservation','50% forest preservation'))
forest.cnpa.ownr.state.smry.dt$priority <- factor(forest.cnpa.ownr.state.smry.dt$priority, levels = c('forest','carbon','biodiv'), 
                                                  labels = c('Preservation priority','Carbon priority','Biodiversity priority'))
forest.cnpa.ownr.state.smry.dt$priority.area.pcnt.lab <- paste0(forest.cnpa.ownr.state.smry.dt$priority.area.pcnt, '%')

# ownership for the region
cons.add.ownr.wus.smry.dt <- cons.add.pxl.dt[, .(priority.area.km2 = .N), by = c('gap.status','priority','target','vuln.mask','owner.abb')]
cons.add.ownr.wus.smry.dt[, total.area.km2 := sum(priority.area.km2), by = c('gap.status','priority','target','vuln.mask')]
cons.add.ownr.wus.smry.dt[, priority.area.pcnt := round(priority.area.km2 / total.area.km2 * 100)]
cons.add.ownr.wus.smry.dt[, priority.area.Mha := priority.area.km2 / 10^4]


# FIGURES: Regional ownership =====================================================================================
cons.add.ownr.wus.smry.dt$target <- factor(cons.add.ownr.wus.smry.dt$target, levels = c(30,50), labels = c('30% forest preservation','50% forest preservation'))
cons.add.ownr.wus.smry.dt$priority <- factor(cons.add.ownr.wus.smry.dt$priority, levels = c('forest','carbon','biodiv'), 
                                                labels = c('Preservation priority','Carbon priority','Biodiversity priority'))
cons.add.ownr.wus.smry.dt$priority.area.pcnt.lab <- paste0(cons.add.ownr.wus.smry.dt$priority.area.pcnt, '%')

# High vulnerability masked
ggplot(cons.add.ownr.wus.smry.dt[gap.status == 2 & priority.area.pcnt >= 2 & vuln.mask == 'yes'], aes(x=owner.abb, y=priority.area.Mha, fill=owner.abb)) + 
  facet_grid(target ~ priority) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = priority.area.pcnt.lab), position = position_dodge(width = 1), hjust = 0.5, vjust = 0, color = 'black', size = 3.5, angle = 0) + 
  scale_fill_viridis(discrete = T, option = "E") +
  theme_bw() + theme(legend.position="none") +
  ylim(c(0,20))+ 
  xlab("Forest ownership") + ylab('Forest area (Mha)')

ggsave('figures/fig5_forest_ownership.jpg', width = 6, height = 4, units = 'in')


# High vulnerability not masked
ggplot(cons.add.ownr.wus.smry.dt[gap.status == 2 & priority.area.pcnt >= 2 & vuln.mask == 'no'], aes(x=owner.abb, y=priority.area.Mha, fill=owner.abb)) + 
  facet_grid(target ~ priority) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = priority.area.pcnt.lab), position = position_dodge(width = 1), hjust = 0.5, vjust = 0, color = 'black', size = 3.5, angle = 0) + 
  scale_fill_viridis(discrete = T, option = "E") +
  theme_bw() + theme(legend.position="none") +
  ylim(c(0,20))+ 
  xlab("Forest ownership") + ylab('Forest area (Mha)')

ggsave('figures/figS3_forest_ownership_novulnmask.jpg', width = 6, height = 4, units = 'in')


# FIGURES: State ownership ============================================================================================================================

ggplot(forest.cnpa.ownr.state.smry.dt[gap.status == 2 & vuln.mask == 'yes' & priority.area.pcnt >= 2 & target == '30% forest preservation'], aes(fill=owner.abb, y=priority.area.Mha, x=owner.abb)) + 
  facet_grid(state.name ~ priority) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = priority.area.pcnt.lab), vjust = -0.2, color = 'black', size = 3.5) + 
  scale_fill_viridis(discrete = T, option = "E") +
  theme_bw() + theme(legend.position="none") +
  ylim(c(0,2.5))+ 
  xlab("Forest ownership") + ylab('Forest area (Mha)')

ggsave('figures/figS1_forest_ownership_by_state_target_30pcnt.jpg', width = 8, height = 6, units = 'in')


ggplot(forest.cnpa.ownr.state.smry.dt[gap.status == 2 & vuln.mask == 'yes' & priority.area.pcnt >= 2 & target == '50% forest preservation'], aes(fill=owner.abb, y=priority.area.Mha, x=owner.abb)) + 
  facet_grid(state.name ~ priority) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = priority.area.pcnt.lab), vjust = -0.2, color = 'black', size = 3.5) + 
  scale_fill_viridis(discrete = T, option = "E") +
  theme_bw() + theme(legend.position="none") +
  ylim(c(0,4.5))+ 
  xlab("Forest ownership") + ylab('Forest area (Mha)')

ggsave('figures/figS2_forest_ownership_by_state_target_50pcnt.jpg', width = 8, height = 6, units = 'in')


# High vulnerability not masked
ggplot(forest.cnpa.ownr.state.smry.dt[gap.status == 2 & vuln.mask == 'no' & priority.area.pcnt >= 2 & target == '30% forest preservation'], aes(fill=owner.abb, y=priority.area.Mha, x=owner.abb)) + 
  facet_grid(state.name ~ priority) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = priority.area.pcnt.lab), vjust = -0.2, color = 'black', size = 3.5) + 
  scale_fill_viridis(discrete = T, option = "E") +
  theme_bw() + theme(legend.position="none") +
  ylim(c(0,2.5))+ 
  xlab("Forest ownership") + ylab('Forest area (Mha)')

ggsave('figures/figS4_forest_ownership_by_state_target_30pcnt_novulnmask.jpg', width = 8, height = 6, units = 'in')


ggplot(forest.cnpa.ownr.state.smry.dt[gap.status == 2 & vuln.mask == 'no' & priority.area.pcnt >= 2 & target == '50% forest preservation'], aes(fill=owner.abb, y=priority.area.Mha, x=owner.abb)) + 
  facet_grid(state.name ~ priority) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = priority.area.pcnt.lab), vjust = -0.2, color = 'black', size = 3.5) + 
  scale_fill_viridis(discrete = T, option = "E") +
  theme_bw() + theme(legend.position="none") +
  ylim(c(0, 4.5))+ 
  xlab("Forest ownership") + ylab('Forest area (Mha)')

ggsave('figures/figS5_forest_ownership_by_state_target_50pcnt_novulnmask.jpg', width = 8, height = 6, units = 'in')



# INVENTORIED ROADLESS AREAS =======================================================================================

# What percentage of high priority regional forestlands occur in IRAs?
forest.cnpa.ira.smry.dt <- cons.add.pxl.dt[, .(priority.area.km2 = .N), by = c('gap.status','priority','target','vuln.mask')]
forest.cnpa.ira.smry.dt$ira.area.km2 <- cons.add.pxl.dt[gap.ira == 1, .(ira.area.km2 = .N), by = c('gap.status','priority','target','vuln.mask')][,5]
forest.cnpa.ira.smry.dt[, ira.pcnt := ira.area.km2 / priority.area.km2 * 100]
fivenum(round(forest.cnpa.ira.smry.dt$ira.pcnt))

# What percentage of high priority federal forestlands occurs in IRAs?
forest.cnpa.ira.smry.dt <- cons.add.pxl.dt[owner.abb == 'FED', .(priority.area.km2 = .N), by = c('gap.status','priority','target','vuln.mask')]
forest.cnpa.ira.smry.dt$ira.area.km2 <- cons.add.pxl.dt[gap.ira == 1, .(ira.area.km2 = .N), by = c('gap.status','priority','target','vuln.mask')][,5]
forest.cnpa.ira.smry.dt[, ira.pcnt := ira.area.km2 / priority.area.km2 * 100]
fivenum(round(forest.cnpa.ira.smry.dt$ira.pcnt))

