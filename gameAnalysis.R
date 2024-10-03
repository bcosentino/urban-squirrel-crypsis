library(survival)
library(coxme)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####Load data#####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

load("gameData.RData")

#data frames:
#r = data from road habitat
#nr = data from non-road habitats

#variables in each data frame:
#date = date of game play 
#start.time = time of game play
#user.id = identification code assigned to user
#environment = habitat type 
#scene.id = identification code for unique scene within habitat
#squirrel color = morph; b = melanic, g = gray
#scene order = sequential order in which image was displayed during game play
#time.to.find.sec = time to find squirrel in seconds (NA = did not find squirrel)
#season = leaf-on or leaf-off seasons
#perspective = ground or aerial viewpoint
#position = branch, climb, or ground postures
#env.scene.id = unique ID for combination of habitat and scene id
#Distance = distance of mount from camera in meters
#found.squirrel01 = binary indicator for finding squirrel: 1 = found, 0 = did not find

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####Variable adjustments#####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#convert detection time to deciseconds and NAs to max 150 ds (censored)
nr$time.ds <- round(nr$time.to.find.sec, digits=1)*10 #tenths of sec
nr$time.ds[is.na(nr$time.ds)] <- 150

r$time.ds <- round(r$time.to.find.sec, digits=1)*10 #tenths of sec
r$time.ds[is.na(r$time.ds)] <- 150

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####COX MODELS WITH RANDOM EFFECTS#####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Env = No road, Season = Fall, Persp = Ground, Position = Ground#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

nr.f.g.g <- subset(nr, season == "leafon" & perspective == "ground" & position == "ground")
cols_to_factor <- c("user.id", "environment", "squirrel.color", "season", "perspective", "position", "env.scene.id")
nr.f.g.g[cols_to_factor] <- lapply(nr.f.g.g[cols_to_factor], factor)

m.nr.f.g.g <- coxme(Surv(time.ds, found.squirrel01) ~ 
                      Distance + scene.order +
                      squirrel.color*environment + 
                      (1 | user.id) + (1 | env.scene.id), 
                    data = nr.f.g.g)
ma.nr.f.g.g <- car::Anova(m.nr.f.g.g)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Env = No road, Season = Fall, Persp = Ground, Position = Climb#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

nr.f.g.c <- subset(nr, season == "leafon" & perspective == "ground" & position == "climb")
cols_to_factor <- c("user.id", "environment", "squirrel.color", "season", "perspective", "position", "env.scene.id")
nr.f.g.c[cols_to_factor] <- lapply(nr.f.g.c[cols_to_factor], factor)

m.nr.f.g.c <- coxme(Surv(time.ds, found.squirrel01) ~ 
                      Distance + scene.order +
                      squirrel.color*environment + 
                      (1 | user.id) + (1 | env.scene.id), 
                    data = nr.f.g.c)
ma.nr.f.g.c <- car::Anova(m.nr.f.g.c)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Env = No road, Season = Fall, Persp = Ground, Position = Branch#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

nr.f.g.b <- subset(nr, season == "leafon" & perspective == "ground" & position == "branch")
cols_to_factor <- c("user.id", "environment", "squirrel.color", "season", "perspective", "position", "env.scene.id")
nr.f.g.b[cols_to_factor] <- lapply(nr.f.g.b[cols_to_factor], factor)

m.nr.f.g.b <- coxme(Surv(time.ds, found.squirrel01) ~ 
                      Distance + scene.order +
                      squirrel.color*environment + 
                      (1 | user.id) + (1 | env.scene.id), 
                    data = nr.f.g.b)
ma.nr.f.g.b <- car::Anova(m.nr.f.g.b)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Env = No road, Season = Fall, Persp = Above, Position = Ground#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

nr.f.a.g <- subset(nr, season == "leafon" & perspective == "above" & position == "ground")
cols_to_factor <- c("user.id", "environment", "squirrel.color", "season", "perspective", "position", "env.scene.id")
nr.f.a.g[cols_to_factor] <- lapply(nr.f.a.g[cols_to_factor], factor)

m.nr.f.a.g <- coxme(Surv(time.ds, found.squirrel01) ~ 
                      Distance + scene.order +
                      squirrel.color*environment + 
                      (1 | user.id) + (1 | env.scene.id), 
                    data = nr.f.a.g)
ma.nr.f.a.g <- car::Anova(m.nr.f.a.g)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Env = No road, Season = Fall, Persp = Above, Position = Climb#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

nr.f.a.c <- subset(nr, season == "leafon" & perspective == "above" & position == "climb")
cols_to_factor <- c("user.id", "environment", "squirrel.color", "season", "perspective", "position", "env.scene.id")
nr.f.a.c[cols_to_factor] <- lapply(nr.f.a.c[cols_to_factor], factor)

m.nr.f.a.c <- coxme(Surv(time.ds, found.squirrel01) ~ 
                      Distance + scene.order +
                      squirrel.color*environment + 
                      (1 | user.id) + (1 | env.scene.id), 
                    data = nr.f.a.c)
ma.nr.f.a.c <- car::Anova(m.nr.f.a.c)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Env = No road, Season = Fall, Persp = Above, Position = Branch#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

nr.f.a.b <- subset(nr, season == "leafon" & perspective == "above" & position == "branch")
cols_to_factor <- c("user.id", "environment", "squirrel.color", "season", "perspective", "position", "env.scene.id")
nr.f.a.b[cols_to_factor] <- lapply(nr.f.a.b[cols_to_factor], factor)

m.nr.f.a.b <- coxme(Surv(time.ds, found.squirrel01) ~ 
                      Distance + scene.order +
                      squirrel.color*environment + 
                      (1 | user.id) + (1 | env.scene.id), 
                    data = nr.f.a.b)
ma.nr.f.a.b <- car::Anova(m.nr.f.a.b)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Env = No road, Season = Winter, Persp = Ground, Position = Ground#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

nr.w.g.g <- subset(nr, season == "leafoff" & perspective == "ground" & position == "ground")
cols_to_factor <- c("user.id", "environment", "squirrel.color", "season", "perspective", "position", "env.scene.id")
nr.w.g.g[cols_to_factor] <- lapply(nr.w.g.g[cols_to_factor], factor)

m.nr.w.g.g <- coxme(Surv(time.ds, found.squirrel01) ~ 
                      Distance + scene.order +
                      squirrel.color*environment + 
                      (1 | user.id) + (1 | env.scene.id), 
                    data = nr.w.g.g)
ma.nr.w.g.g <- car::Anova(m.nr.w.g.g)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Env = No road, Season = Winter, Persp = Ground, Position = Climb#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

nr.w.g.c <- subset(nr, season == "leafoff" & perspective == "ground" & position == "climb")
cols_to_factor <- c("user.id", "environment", "squirrel.color", "season", "perspective", "position", "env.scene.id")
nr.w.g.c[cols_to_factor] <- lapply(nr.w.g.c[cols_to_factor], factor)

m.nr.w.g.c <- coxme(Surv(time.ds, found.squirrel01) ~ 
                      Distance + scene.order +
                      squirrel.color*environment + 
                      (1 | user.id) + (1 | env.scene.id), 
                    data = nr.w.g.c)
ma.nr.w.g.c <- car::Anova(m.nr.w.g.c)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Env = No road, Season = Winter, Persp = Ground, Position = Branch#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

nr.w.g.b <- subset(nr, season == "leafoff" & perspective == "ground" & position == "branch")
cols_to_factor <- c("user.id", "environment", "squirrel.color", "season", "perspective", "position", "env.scene.id")
nr.w.g.b[cols_to_factor] <- lapply(nr.w.g.b[cols_to_factor], factor)

m.nr.w.g.b <- coxme(Surv(time.ds, found.squirrel01) ~ 
                      Distance + scene.order +
                      squirrel.color*environment + 
                      (1 | user.id) + (1 | env.scene.id), 
                    data = nr.w.g.b)
ma.nr.w.g.b <- car::Anova(m.nr.w.g.b)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Env = No road, Season = Winter, Persp = Above, Position = Ground#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

nr.w.a.g <- subset(nr, season == "leafoff" & perspective == "above" & position == "ground")
cols_to_factor <- c("user.id", "environment", "squirrel.color", "season", "perspective", "position", "env.scene.id")
nr.w.a.g[cols_to_factor] <- lapply(nr.w.a.g[cols_to_factor], factor)

m.nr.w.a.g <- coxme(Surv(time.ds, found.squirrel01) ~ 
                      Distance + scene.order +
                      squirrel.color*environment + 
                      (1 | user.id) + (1 | env.scene.id), 
                    data = nr.w.a.g)
ma.nr.w.a.g <- car::Anova(m.nr.w.a.g)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Env = No road, Season = Winter, Persp = Above, Position = Climb#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

nr.w.a.c <- subset(nr, season == "leafoff" & perspective == "above" & position == "climb")
cols_to_factor <- c("user.id", "environment", "squirrel.color", "season", "perspective", "position", "env.scene.id")
nr.w.a.c[cols_to_factor] <- lapply(nr.w.a.c[cols_to_factor], factor)

m.nr.w.a.c <- coxme(Surv(time.ds, found.squirrel01) ~ 
                      Distance + scene.order +
                      squirrel.color*environment + 
                      (1 | user.id) + (1 | env.scene.id), 
                    data = nr.w.a.c)
ma.nr.w.a.c <- car::Anova(m.nr.w.a.c)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Env = No road, Season = Winter, Persp = Above, Position = Branch#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

nr.w.a.b <- subset(nr, season == "leafoff" & perspective == "above" & position == "branch")
cols_to_factor <- c("user.id", "environment", "squirrel.color", "season", "perspective", "position", "env.scene.id")
nr.w.a.b[cols_to_factor] <- lapply(nr.w.a.b[cols_to_factor], factor)

m.nr.w.a.b <- coxme(Surv(time.ds, found.squirrel01) ~ 
                      Distance + scene.order +
                      squirrel.color*environment + 
                      (1 | user.id) + (1 | env.scene.id), 
                    data = nr.w.a.b)
ma.nr.w.a.b <- car::Anova(m.nr.w.a.b)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Env = Road, Season = Fall, Persp = Ground, Position = Ground#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

r.f.g.g <- subset(r, season == "leafon" & perspective == "ground" & position == "ground")
cols_to_factor <- c("user.id", "environment", "squirrel.color", "season", "perspective", "position", "env.scene.id")
r.f.g.g[cols_to_factor] <- lapply(r.f.g.g[cols_to_factor], factor)

m.r.f.g.g <- coxme(Surv(time.ds, found.squirrel01) ~ 
                     Distance + scene.order +
                     squirrel.color + 
                     (1 | user.id) + (1 | env.scene.id), 
                   data = r.f.g.g)
ma.r.f.g.g <- car::Anova(m.r.f.g.g)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Env = Road, Season = Winter, Persp = Ground, Position = Ground#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

r.w.g.g <- subset(r, season == "leafoff" & perspective == "ground" & position == "ground")
cols_to_factor <- c("user.id", "environment", "squirrel.color", "season", "perspective", "position", "env.scene.id")
r.w.g.g[cols_to_factor] <- lapply(r.w.g.g[cols_to_factor], factor)

m.r.w.g.g <- coxme(Surv(time.ds, found.squirrel01) ~ 
                     Distance + scene.order +
                     squirrel.color + 
                     (1 | user.id) + (1 | env.scene.id), 
                   data = r.w.g.g)
ma.r.w.g.g <- car::Anova(m.r.w.g.g)