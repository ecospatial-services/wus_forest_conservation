


pxl.dt

# every forest pixel in region 
pxl.expanded.dt <- data.table(expand.grid(pxl.id =  pxl.dt[forest.extent == 1]$pxl.id, target = targets, priority = priorities, vuln.mask = vuln.mask, model = models))
pxl.expanded.dt$gap.code <- pxl.dt$gap.code[match(pxl.expanded.dt$pxl.id, pxl.dt$pxl.id)]

cons.add.pxl.dt
clm.pxl.dt
