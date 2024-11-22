#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set the Working Directory
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Replace with the path where you downloaded `MSc_Protected_Areas`. 
# This means that your code will run from this directory. 
# Make sure you use `/` or `\\` in your path name, otherwise you will get an error.

setwd("C:/Users/kdh10kg/Documents/github/MSc_Protected_Areas/")


# Load Packages
library(prioritizr)
library(terra)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the files
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load species distributions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Open file
species = terra::rast("data/species.tif")


### Plot species individual maps
plot(species[[1:3]], nr = 1, axes = FALSE)


### Plot species together
# We have 41 species that we can use to develop a species richness map.
plot(sum(species))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load Cost Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# A very important element of protected area planning is the cost. 
# Wherever we put a protected area, there will be a cost involved. 
# 
# Cost can be:
#   
# - directly estimated: we can do an economic evaluation about economic losses of 
# converting land to  protected land. This can be through the cost of purchasing land, 
# costing the staff needed to patrol, the revenue from tourism, the loss 
# of opportunities (loss of mining land, grazing land, agricultural land, and even homes).
# 
# - indirectly estimated: we can estimate what land is most costly. 
# For example, the human footprint index is often used to see where people 
# are using land. We then make the assumption that areas that people use are 
# most costly and created the greatest conflict, and should be avoided as a place 
# to put a protected area.
# 
# Here, it's an excecise, so we use the Human footprint layer, 
# which is downloaded from this paper:
#   https://www.nature.com/articles/s41597-022-01284-8#Sec12


# load the file
cost0 = rast("data/cost.tif")

# Plot the file
par(mfrow = c(1,2)) # put 2 plots side by side
plot(cost0, main = "Cost as Human footprint")
plot(sum(species), main="Species richness")
par(mfrow = c(1,1)) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dataset Resolution
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Notice that cost is very fine scale while the species data are coarse. 
# We need to lower the resolution of the cost data so that it is the same as the species data.

# plot the data
par(mfrow=c(1,2)) # split into 2 plots
plot(cost0, main = "Cost High Res")
cost = terra::aggregate(cost0, fact = 12, fun = "median",  na.rm=TRUE)
plot(cost, main = "Cost Low Res")
par(mfrow=c(1,1)) # make sure the next plot is just 1 plot

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Spatial extent sanity checks
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Extent - Do they look the same?
ext(cost)
ext(species)

## Resolution - Do they look the same?
res(cost)
res(species)




# Solvers
# if needed, install HiGHS solver, but this should have been done before the practical
# install.packages("highs", repos = "https://cran.rstudio.com/")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Spatial Prioritization
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Formulate the problem

# I want to protect as many species as possible within my budget 
# by setting cells aside for protection, 
# aiming for 40% of the range of each species
  
# Translated to code:
# - We start by creating a new `problem()` that will use a
# - minimum shortfall objective to spend the budget while getting as much as possible (via `add_min_shortfall_objective()`), with 
# - relative targets of 30% of the range of each species(via `add_relative_targets()`), 
# - binary decisions to purchase+protect or not (via `add_binary_decisions()`), and 
# - near-optimal solutions (i.e., 10% from optimality) using the 
# - best solver installed on our computer (via `add_default_solver()`).


# calculate budget
budget <- terra::global(cost, "sum", na.rm = TRUE)[[1]] * 0.3

# create problem
p1 <-
  problem(cost, features = species) %>%
  add_min_shortfall_objective(budget) %>%
  add_relative_targets(0.3) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0.1, verbose = FALSE)

# print problem
print(p1)

## Solve the problem
s1 <- solve(p1)


## Get the time it took to solve in seconds
print(attr(s1, "runtime"))


## Get the status of the solver i.e. was it optimal or not?
print(attr(s1, "status"))


## Plot
plot(s1, main = "Solution", axes = FALSE)


## How many planning units were protected per solution?
eval_n_summary(p1, s1)

## How much does it cost?
eval_cost_summary(p1, s1)


## Was each species protected 30%?
p1_target_coverage <- eval_target_coverage_summary(p1, s1)
print(p1_target_coverage)


## How many were not protected 30%
print(mean(p1_target_coverage$met) * 100)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Exercise 2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# How many species have 70% of their range protected?
  
# Hint: change the values in add_relative_targets

# calculate budget
budget <- terra::global(cost, "sum", na.rm = TRUE)[[1]] * 0.3

# create problem
pt <-
  problem(cost, features = species) %>%
  add_min_shortfall_objective(budget) %>%
  add_relative_targets(0.7) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0.1, verbose = FALSE)

# print problem
print(pt)

# solve
st <- solve(pt)

#calculate percentage meeting target
pt_target_coverage <- eval_target_coverage_summary(pt, st)
print(pt_target_coverage)
print(mean(pt_target_coverage$met) * 100)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Adding existing PAs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Madagascar already had a Protected Area network.
# Do the areas we are protecting overlap with the existing protected areas? 
# Perhaps we should develop protected areas that complement and enhance the existing ones.

# load protected areas
PAs = terra::rast("data/locked_in.tif")

# add the land in
PAs = ifel((!is.na(cost0) & is.na(PAs)),0,PAs )

# Re-scale
par(mfrow = c(1,2))
plot(PAs, main="High Res PAs")
PAs = terra::aggregate(PAs, fact = 12, fun = "modal",  na.rm=TRUE)
plot(PAs, main="Low Res PAs")
par(mfrow = c(1,1))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Modify the problem to add PAs in
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# The function `add_locked_in_constraints()` 
# allows us to add in areas that are "locked in" 
# i.e. already conserved or protected. 


# need to reset budget because it was modified in exercise 2
budget <- terra::global(cost, "sum", na.rm = TRUE)[[1]] * 0.3

p2 <-  p1 %>%
  add_locked_in_constraints(PAs) # this is the only new line

# solve the problem
s2 <- solve(p2)

# plot the solution
par(mfrow=c(1,2)) # split plot into 2 panels
plot(s1, main = "without PAs", axes = FALSE)
plot(s2, main = "With PAs", axes = FALSE)
par(mfrow=c(1,1)) # Go back to one plot


# calculate number of selected planning units by solution
eval_n_summary(p2, s2)

# calculate total cost of solution
eval_cost_summary(p2, s2)

# calculate target coverage for the solution
p2_target_coverage <- eval_target_coverage_summary(p2, s2)
print(p2_target_coverage)

# check percentage of the features that have their target met given the solution
print(mean(p2_target_coverage$met) * 100)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add boundary penalty
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# We can further modify the problem by adding penalties that punish overly fragmented solutions 
# (via `add_boundary_penalties()`). Here we will use a penalty factor 
# (i.e., boundary length modifier) of 0.003, and an edge factor of 50% so that planning units 
# that occur on the outer edge of the study area are not overly penalized.


# need to reset budget because it was modified in exercise 2
budget <- terra::global(cost, "sum", na.rm = TRUE)[[1]] * 0.3

# create new problem with boundary penalties added to it
p3 <-
  p2 %>%
  add_boundary_penalties(penalty = 0.003, edge_factor = 0.7)

# solve the problem
s3 <- solve(p3)

# plot the solution
par(mfrow=c(1,3))# split into 3 plots side by side
plot(s1, main = "Baseline", axes = FALSE)
plot(s2, main = "With PAs", axes = FALSE)
plot(s3, main = "With Boundary layer", axes = FALSE)
par(mfrow=c(1,1))# go back to only 1 plot

# calculate target coverage for the solution
p3_target_coverage <- eval_target_coverage_summary(p3, s3)
print(p3_target_coverage)

# check percentage of the features that have their target met given the solution
print(mean(p3_target_coverage$met) * 100)


# What is the first thing you notice? It's much longer... 
# This is because it has to compute a lot more relationships between cells/planning units.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Exercise 3
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Modify the edge factor to 0.2 and the penalty to 0.7 and see what happens

pz <-
  p2 %>%
  add_boundary_penalties(penalty = 0.7, edge_factor = 0.2)

# solve the problem
sz <- solve(pz)

# plot the solution
plot(sz, main = "Solution", axes = FALSE)

# calculate target coverage for the solution
pz_target_coverage <- eval_target_coverage_summary(pz, sz)
print(pz_target_coverage)

# check percentage of the features that have their target met given the solution
print(mean(pz_target_coverage$met) * 100)


# Reducing the edge factor makes it even blockier!

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate importance scores
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# The importance of irreplaceaability tells you how important each cell is to the solution,
# if it is red, you absolutely need to protect that cell. If it is beige, 
# it is interchangeable with other cells. If you have a lot of common species (like we do), 
# then most of them will be beige, but if you have rare narrow range endemics, 
# then those cells will become red. 


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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Exercise 4 (Bonus)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# As a final exercise, you will "lock out" some areas from protection. 
# These are places like cities that should be ignored from the prioritisation. 
# 
# Go find the `locked_out.tif` raster in your data directory and use `terra::rast` 
# to import it. Then use the `  add_locked_out_constraints()` to lock out your raster 
# from the p2 problem we formulated. If you want you can also plot the locked_out data layer


locked_out = terra::rast("data/locked_out.tif")

# need to reset budget because it was modified in exercise 1
budget <- terra::global(cost, "sum", na.rm = TRUE)[[1]] * 0.3

# create new problem with boundary penalties added to it
p4 <-
  p2 %>%
  add_locked_out_constraints(locked_out)

# solve the problem
s4 <- solve(p4)

# plot the solution
par(mfrow=c(2,2))# split into 3 plots side by side
plot(s1, main = "Baseline", axes = FALSE)
plot(s2, main = "With PAs", axes = FALSE)
plot(s4, main = "With PAs + Avoid cities", axes = FALSE)
plot(s3, main = "With PAs + Boundary layer", axes = FALSE)
par(mfrow=c(1,1))# go back to only 1 plot

# calculate target coverage for the solution
p4_target_coverage <- eval_target_coverage_summary(p4, s4)
print(p4_target_coverage)

# check percentage of the features that have their target met given the solution
print(mean(p4_target_coverage$met) * 100)


# Notice that it reality it doesn't change much because we are using the same data to 
# derive the cost layer and the locked out data

plot(3*s1+s2, main = "Baseline", axes = FALSE)
