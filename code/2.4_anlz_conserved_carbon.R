# This script summarizes current carbon stocks and near tearm carbon accumulation across preservation targets and scenarios
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
pxl.dt <- fread('output/wus_geospatial_data_by_pxl.csv')
clm.pxl.dt <- fread('output/wus_clm_cnep_by_pxl_model_scenario_period.csv')

# drop some unneaded columns
cons.add.pxl.dt <- cons.add.pxl.dt[, c('state.name', 'owner.abb', 'area.km2','gap.ira') :=  NULL]
cons.add.pxl.dt[, status := 'proposed']
cons.add.pxl.dt <- cons.add.pxl.dt[gap.status == 2] # focus on lands protected under GAP 1 or 2 
cons.add.pxl.dt[, gap.status := NULL]

clm.pxl.dt <- clm.pxl.dt[year != 'x2030']
# clm.pxl.dt <- clm.pxl.dt[scenario != 'BAU']

# identify currently protected forest
cpa.pxl.dt <- pxl.dt[forest.extent == 1 & gap.code <= 2]

# variables needed when expanding the CPA data table to represent all targets/priorities/vulns/models 
priorities <- unique(cons.add.pxl.dt$priority)
targets <- unique(cons.add.pxl.dt$target)
vuln.mask <- c('no','yes')
models <- c('IPSL','MIROC','ENSEMBLE')
scenarios <- c('NOHARV','BAU')
pxls <- cpa.pxl.dt$pxl.id


# FOREST TOTAL ECOSYSTEM CARBON =============================================================================================

# expand current protected area data table
cpa.pxl.expanded.dt <- data.table(expand.grid(pxl.id =  pxls, target = 0, priority = priorities, vuln.mask = vuln.mask))
cpa.pxl.expanded.dt[, status := 'current']

# expand current protected area data table again so these can be summed with preservation targets
cpa.all.priorities.dt <- data.table(expand.grid(pxl.id =  pxls, target = targets, priority = priorities, vuln.mask = vuln.mask))
cpa.all.priorities.dt[, status := 'proposed']

# join together current and proposed land areas 
cons.pxl.dt <- rbind(cpa.pxl.expanded.dt, cons.add.pxl.dt, cpa.all.priorities.dt)

# append total ecosystem carbon to data table
cons.pxl.dt$totc.MgPxl <- pxl.dt$totc.MgPxl[match(cons.pxl.dt$pxl.id, pxl.dt$pxl.id)]

# compute total total ecosystem carbon
totc.smry.dt <- cons.pxl.dt[, .(totc.Pg = sum(totc.MgPxl) / 10^9), by = c('target','priority','vuln.mask')]

# add total regional carbon stock
region.totc.smry <- sum(pxl.dt[forest.extent == 1]$totc.MgPxl)/10^9
totc.smry.dt[, totc.reg.Pg := region.totc.smry]
totc.smry.dt[, totc.reg.pcnt := paste0(round(totc.Pg / totc.reg.Pg * 100),'%')]

# set factors
totc.smry.dt[, target := factor(target, labels = c('Current','30%','50%'))]
totc.smry.dt[, priority := factor(priority, levels = c('forest','carbon','biodiv'), 
                                  labels = c('Preservation priority','Carbon priority','Biodiversity priority'))]

setorder(totc.smry.dt, target, priority)
totc.smry.dt


# FOREST CARBON ACCUMULATION (2020 - 2050) =====================================================================================

# expand preservation priority data table so each model and scenario is represented (there is probably a more refined way...)
xx1 <- copy(cons.add.pxl.dt[, model := 'IPSL'])
xx2 <- copy(cons.add.pxl.dt[, model := 'MIROC'])
xx3 <- copy(cons.add.pxl.dt[, model := 'ENSEMBLE'])
cons.add.pxl.clm.dt <- rbind(xx1, xx2, xx3)

yy1 <- copy(cons.add.pxl.clm.dt[, scenario := 'NOHARV'])
yy2 <- copy(cons.add.pxl.clm.dt[, scenario := 'BAU'])
cons.add.pxl.clm.dt <- rbind(yy1, yy2)

# expand current protected area data table
cpa.pxl.expanded.clm.dt <- data.table(expand.grid(pxl.id =  pxls, target = 0, priority = priorities, vuln.mask = vuln.mask, model = models, scenario = scenarios))
cpa.pxl.expanded.clm.dt[, status := 'current']

# expand current protected area data table again so these can be summed with preservation targets
cpa.all.priorities.clm.dt <- data.table(expand.grid(pxl.id =  pxls, target = targets, priority = priorities, vuln.mask = vuln.mask, model = models, scenario = scenarios))
cpa.all.priorities.clm.dt[, status := 'proposed']

# join together current and proposed land areas 
cons.pxl.clm.dt <- rbind(cpa.pxl.expanded.clm.dt, cons.add.pxl.clm.dt, cpa.all.priorities.clm.dt)

# add cumulative net ecosystem production to data table
cons.pxl.clm.dt <- clm.pxl.dt[cons.pxl.clm.dt, on = c('pxl.id','model','scenario')]

# compute total cumulative NEP
cnep.smry.dt <- cons.pxl.clm.dt[, .(cnep.Pg = sum(cnep.MgPxl, na.rm=T) / 10^9), by = c('target','priority','vuln.mask','model','scenario')]
cnep.smry.dt <- cnep.smry.dt[, .(cnep.avg.Pg = mean(cnep.Pg), cnep.min.Pg = min(cnep.Pg), cnep.max.Pg = max(cnep.Pg)), by = c('target','priority','vuln.mask','scenario')]
setorder(cnep.smry.dt, target, priority, vuln.mask, scenario)
cnep.smry.dt

# set factors
cnep.smry.dt[, target := factor(target, labels = c('Current','30%','50%'))]
cnep.smry.dt[, priority := factor(priority, levels = c('forest','carbon','biodiv'), 
                                  labels = c('Preservation priority','Carbon priority','Biodiversity priority'))]


# FIGURES =================================================================================

# carbon stocks
totc.fig <- ggplot(totc.smry.dt[vuln.mask == 'yes'], aes(x=target, y=totc.Pg, fill=target)) + 
  facet_wrap(~priority) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = totc.reg.pcnt), vjust = -0.2, color = 'black', size = 3.5) + 
  scale_fill_viridis(discrete = T, option = "D") +
  lims(y = c(0,6.8))+
  theme_bw() + theme(legend.position="none", axis.text=element_text(size=12), axis.title=element_text(size=14)) +
  xlab("") + ylab('Carbon stocks (Pg)')

totc.fig

# carbon accumulation
cnep.fig <- ggplot(cnep.smry.dt[vuln.mask == 'yes' & scenario == 'NOHARV'], aes(x=target, y=cnep.avg.Pg, fill=target)) + 
  facet_wrap(~priority) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin = cnep.min.Pg, ymax = cnep.max.Pg), width=.2, position=position_dodge(0.9)) + 
  # geom_text(aes(label = totc.reg.pcnt), vjust = -0.2, color = 'black', size = 3.5) + 
  scale_fill_viridis(discrete = T, option = "D") +
  lims(y = c(0, 1.6)) + 
  theme_bw() + theme(legend.position="none", axis.text=element_text(size=12), axis.title=element_text(size=14)) +
  xlab("Forest preservation target") + ylab('Carbon accumulation (Pg)')
cnep.fig

# combine
fig.combo <- ggarrange(totc.fig, cnep.fig, labels=c('a','b'), nrow = 2, label.x = c(0.01, 0.01), label.y = 1)
fig.combo
ggsave('figures/fig6_forest_carbon.jpg', width = 8, height = 7, units = 'in')

print ("All done!!")



# FIGURES (VULN NOT MASKED) =================================================================================

# carbon stocks
totc.fig <- ggplot(totc.smry.dt[vuln.mask == 'no'], aes(x=target, y=totc.Pg, fill=target)) + 
  facet_wrap(~priority) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = totc.reg.pcnt), vjust = -0.2, color = 'black', size = 3.5) + 
  scale_fill_viridis(discrete = T, option = "D") +
  lims(y = c(0,6.8))+
  theme_bw() + theme(legend.position="none", axis.text=element_text(size=12), axis.title=element_text(size=14)) +
  xlab("") + ylab('Carbon stocks (Pg)')

totc.fig

# carbon accumulation
cnep.fig <- ggplot(cnep.smry.dt[vuln.mask == 'no' & scenario == 'NOHARV'], aes(x=target, y=cnep.avg.Pg, fill=target)) + 
  facet_wrap(~priority) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin = cnep.min.Pg, ymax = cnep.max.Pg), width=.2, position=position_dodge(0.9)) + 
  # geom_text(aes(label = totc.reg.pcnt), vjust = -0.2, color = 'black', size = 3.5) + 
  scale_fill_viridis(discrete = T, option = "D") +
  lims(y = c(0, 1.6)) + 
  theme_bw() + theme(legend.position="none", axis.text=element_text(size=12), axis.title=element_text(size=14)) +
  xlab("Forest preservation target") + ylab('Carbon accumulation (Pg)')
cnep.fig

# combine
fig.combo <- ggarrange(totc.fig, cnep.fig, labels=c('a','b'), nrow = 2, label.x = c(0.01, 0.01), label.y = 1)
fig.combo
ggsave('figures/figS6_forest_carbon.jpg', width = 8, height = 7, units = 'in')

print ("All done!!")

