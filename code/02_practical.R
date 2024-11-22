# install.packages("prioritizr")
# install.packages("prioritizrdata")
# install.packages("terra")
# install.packages("remotes")
# remotes::install_bioc("lpsymphony")
# install.packages("highs", repos = "https://cran.rstudio.com/")

# Each cell represents a different planning unit, and cell values denote land acquisition costs. 

# 
# Requirements:
#   R
#   Rtools

# load packages
library(prioritizr)
library(prioritizrdata)
library(terra)


# 40 common species
basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/teaching/2024/MSC_practical/"

##########################
# SPECIES
##########################

raster_list <- list.files(path = paste0(basepath,"binary/"),
                          pattern = ".tif",
                          full.names = T)


species = terra::rast(raster_list)
names(species) = list.files(path = paste0(basepath,"binary/"),
                            pattern = ".tif",
                            full.names = F)
plot(sum(species))
species = aggregate(species, fact = 12, fun = "modal", na.rm = T)

plot(sum(species))
# plot(sum(species), add=T)
plot(species[[1:9]], nr = 3, axes = FALSE)
writeRaster(species, paste0(basepath, "species.tif"))

##############################################
## PROTECTED AREAS
##############################################

# v <- terra::vect(paste0(basepath,"WDPA/WDPA.shp"))
# # plot(v)
# # project it to a projection that ensures that madagascar is the same as the species data
# v2 <- terra::project(v, crs(species))
# 
# # crop so that it is the same size as the species data
# v3 <- crop(v2, ext(species))
# plot(sum(species))
# plot(v3, add=T)
# writeVector(v3, paste0(basepath, "locked_in.shp"))
# 
# 
# # convert to raster
# v4 <- rasterize(v3, species)
# plot(v4)
# # Save
# writeRaster(v4, paste0(basepath, "locked_in.tif"))




#########################
# COST
#########################

# ### How to create the raster
# # Load human footprint data
# cost = terra::rast(paste0(basepath, "hfp2017/hfp2017.tif"))#"human_footprint_madagascar_clip.tif"))
# cost
# plot(cost)
# 
# # project it to a projection that ensures that madagascar is the same as the species data
# cost2 <- terra::project(cost, crs(species))
# plot(cost2)
# 
# # make sure it is the same resolution as the species data (that the cells are all in line)
# cost3 <- resample(cost2, species)
# # crop so that it is the same size as the species data
# cost4 <- crop(cost3, ext(species))
# plot(cost4)
# 
# # Save
# writeRaster(cost4, paste0(basepath, "cost.tif"))

#####
# LOCK OUT

# load the file
cost = terra::rast(paste0(basepath, "cost.tif"))

lockout = ifel(cost >= 10, 1, 0  )
plot(lockout)

writeRaster(lockout, paste0(basepath, "locked_out.tif"))


###############
# load the file
cost = terra::rast(paste0(basepath, "cost.tif"))



# Plot the file
plot(cost)

# compare the extent and resolution of the files, are they the same?
ext(cost)
ext(species)
res(cost)
res(species)


###### Make the resolution of both datasets smaller
par(mfrow=c(1,2))
plot(cost)
cost = aggregate(cost, fact = 12, fun = "median", na.rm = T)
plot(cost)
plot(sum(species))

par(mfrow=c(1,1))


# compare the extent and resolution of the files, are they the same?
ext(cost)
ext(species)
res(cost)
res(species)



################
# calculate budget
budget <- terra::global(cost, "sum", na.rm = TRUE)[[1]] * 0.05

# create problem
p1 <-
  problem(cost, features = species) %>%
  add_min_shortfall_objective(budget) %>%
  add_relative_targets(0.2) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0.1, verbose = FALSE)

# print problem
print(p1)

# solve the problem
s1 <- solve(p1)

# extract the objective
print(attr(s1, "objective"))

# extract time spent solving the problem
print(attr(s1, "runtime"))

# extract state message from the solver
print(attr(s1, "status"))

# plot the solution
plot(s1, main = "Solution", axes = FALSE)

# calculate number of selected planning units by solution
eval_n_summary(p1, s1)

# calculate total cost of solution
eval_cost_summary(p1, s1)

# calculate target coverage for the solution
p1_target_coverage <- eval_target_coverage_summary(p1, s1)
print(p1_target_coverage)

# check percentage of the features that have their target met given the solution
print(mean(p1_target_coverage$met) * 100)

# It's super low - what might be happening?
# increase cost
# account for existing protected areas already

##########################
# NOW TRY SDM
##########################

raster_list <- list.files(path = paste0(basepath,"probability/"),
                          pattern = ".tif",
                          full.names = T)


species = terra::rast(raster_list)
names(species) = list.files(path = paste0(basepath,"probability/"),
                            pattern = ".tif",
                            full.names = F)
plot(sum(species))

species = aggregate(species, fact = 10, fun = "median")

# plot(sum(species), add=T)
plot(species[[1:9]], nr = 3, axes = FALSE)
plot(species[[10:18]], nr = 3, axes = FALSE)

################
# calculate budget
budget <- terra::global(cost, "sum", na.rm = TRUE)[[1]] * 0.05

# create problem
p1 <-
  problem(cost, features = species) %>%
  add_min_shortfall_objective(budget) %>%
  add_relative_targets(0.2) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0.1, verbose = FALSE)

# print problem
print(p1)

# solve the problem
s1 <- solve(p1)

# extract the objective
print(attr(s1, "objective"))

# extract time spent solving the problem
print(attr(s1, "runtime"))

# extract state message from the solver
print(attr(s1, "status"))

# plot the solution
plot(s1, main = "Solution", axes = FALSE)

# calculate number of selected planning units by solution
eval_n_summary(p1, s1)

# calculate total cost of solution
eval_cost_summary(p1, s1)

# calculate target coverage for the solution
p1_target_coverage <- eval_target_coverage_summary(p1, s1)
print(p1_target_coverage)

# check percentage of the features that have their target met given the solution
print(mean(p1_target_coverage$met) * 100)

# identical basically



### Exercise change the cost

# calculate budget
budget <- terra::global(cost, "sum", na.rm = TRUE)[[1]] * 0.08

# create problem
p1 <-
  problem(cost, features = species) %>%
  add_min_shortfall_objective(budget) %>%
  add_relative_targets(0.2) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0.1, verbose = FALSE)

# print problem
print(p1)

# solve the problem
s1 <- solve(p1)

# extract the objective
print(attr(s1, "objective"))

# extract time spent solving the problem
print(attr(s1, "runtime"))

# extract state message from the solver
print(attr(s1, "status"))

# plot the solution
plot(s1, main = "Solution", axes = FALSE)

# calculate number of selected planning units by solution
eval_n_summary(p1, s1)

# calculate total cost of solution
eval_cost_summary(p1, s1)

# calculate target coverage for the solution
p1_target_coverage <- eval_target_coverage_summary(p1, s1)
print(p1_target_coverage)

# check percentage of the features that have their target met given the solution
print(mean(p1_target_coverage$met) * 100)

# increases a little

##############################################
####      NOW ADD IN PAs       ###############
##############################################

# load protected areas
PAs = terra::rast(paste0(basepath, "locked_in.tif"))

# rescale
par(mfrow = c(1,2))
plot(PAs)
PAs = aggregate(PAs, fact = 10, fun = "max")
plot(PAs)
par(mfrow = c(1,1))

#add the land in
PAs = ifel((!is.na(cost) & is.na(PAs)),0,PAs )

# print data
print(PAs)
plot(PAs,  main = "Existing protected areas", axes = FALSE)

##########################################
# create new problem with locked in constraints added to it
budget <- terra::global(cost, "sum", na.rm = TRUE)[[1]] * 0.1

# create problem

p2 <-
  problem(cost, features = species) %>%
  add_min_shortfall_objective(budget) %>%
  add_relative_targets(0.2) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0.1, verbose = FALSE) %>%
  add_locked_in_constraints(PAs) # this is the only new line

# solve the problem
s2 <- solve(p2)

# plot the solution
plot(s2, main = "Solution", axes = FALSE)


# calculate number of selected planning units by solution
eval_n_summary(p2, s2)

# calculate total cost of solution
eval_cost_summary(p2, s2)

# calculate target coverage for the solution
p2_target_coverage <- eval_target_coverage_summary(p2, s2)
print(p2_target_coverage)

# check percentage of the features that have their target met given the solution
print(mean(p2_target_coverage$met) * 100)


####################################################
#### ADD boundary penalty
####################################################

# we can further modify the problem by adding penalties that punish overly 
# fragmented solutions (via add_boundary_penalties()). Here we will use a penalty 
# factor (i.e., boundary length modifier) of 0.003, and an edge factor of 50% so 
# that planning units that occur on the outer edge of the study area are not overly
# penalized.

# create new problem with boundary penalties added to it
p3 <-
  p2 %>%
  add_boundary_penalties(penalty = 0.003, edge_factor = 0.5)

# solve the problem
s3 <- solve(p3)

# plot the solution
plot(s3, main = "Solution", axes = FALSE)


# excercise:
# Modify the edge factor to 0.8 and see what happens

p3 <-
  p2 %>%
  add_boundary_penalties(penalty = 0.003, edge_factor = 0.2)

# solve the problem
s3 <- solve(p3)

# plot the solution
plot(s3, main = "Solution", axes = FALSE)

# excercise:
# Modify the penalty factor to 0.3 and see what happens

p3 <-
  p2 %>%
  add_boundary_penalties(penalty = 0.3, edge_factor = 0.5)

# solve the problem
s3 <- solve(p3)

# plot the solution
plot(s3, main = "Solution", axes = FALSE)

######################################################
###   calculate importance scores
######################################################

rc <-
  p3 %>%
  eval_ferrier_importance(s3)

# print scores
print(rc)

# plot the total importance scores
## note that gray cells are not selected by the prioritization
plot(
  rc[["total"]], main = "Importance scores", axes = FALSE,
  breaks = c(0, 1e-10, 0.005, 0.01, 0.025),
  col = c("#e5e5e5", "#fff7ec", "#fc8d59", "#7f0000")
)
