#Models that we've tried out

######## Model 1:
# Define model formula that applies to both will be applied to both males and females
mod <- cogscore ~ . + age:female + ep005_:eurod + sp002_mod:iv009_mod +
  iv009_mod:eurod + isced1997_r:co007_ + iv009_mod:casp + casp:co007_ + 
  mar_stat:hhsize + mar_stat:sp002_mod + hhsize:co007_ + chronic_mod:sp002_mod +
  chronic_mod:age + chronic_mod:female + chronic_mod:br015_ + chronic_mod:casp +
  eurod:ever_smoked + eurod:co007_ + chronic_mod:hc002_mod + chronic_mod:hc012_ +
  chronic_mod:hc029_ + hc002_mod:adla + hc002_mod:casp + hc002_mod:iadlza + 
  hc002_mod:maxgrip + hc002_mod:age + hc012_:age + hc029_:age + adla:age + 
  hc012_:adla + hc012_:iadlza + hc029_:adla + hc029_:iadlza + adla:maxgrip + 
  iadlza:maxgrip + bmi2:adla + bmi2:br015_ + co007_:ep005_

# Define model
fit <- lm(mod, data = na.omit(training_data))

# Display model assumptions through residuals
par(mfrow = c(2, 2))
plot(fit)

summary(fit)



######## Model 2:
# Define model formula
mod1 <- cogscore ~ age*(female + dn004_mod + iv009_mod + isced1997_r + 
                             mar_stat + hhsize + ch001_ + sp002_mod + chronic_mod +
                             casp + eurod + hc002_mod + hc012_ + hc029_ + 
                             iadlza + adla + maxgrip + bmi2 + ever_smoked +
                             br015_ + ep005_ + co007_) + 
  female*(dn004_mod + iv009_mod + isced1997_r + mar_stat + hhsize + ch001_ + 
            sp002_mod + chronic_mod + casp + eurod + hc002_mod + hc012_ + hc029_ + 
            iadlza + adla + maxgrip + bmi2 + ever_smoked + br015_ + ep005_ + co007_) +
  dn004_mod*(iv009_mod + isced1997_r + mar_stat + hhsize + ch001_ + sp002_mod + 
               chronic_mod + casp + eurod + hc002_mod + hc012_ + hc029_ + iadlza + 
               adla + maxgrip + bmi2 + ever_smoked + br015_ + ep005_ + co007_) +
  iv009_mod*(isced1997_r + mar_stat + hhsize + ch001_ + sp002_mod + chronic_mod + 
               casp + eurod + hc002_mod + hc012_ + hc029_ + iadlza + adla + 
               maxgrip + bmi2 + ever_smoked + br015_ + ep005_ + co007_) +
  isced1997_r*(mar_stat + hhsize + ch001_ + 
  sp002_mod + chronic_mod + casp + eurod + hc002_mod + hc012_ + hc029_ + iadlza +
    adla + maxgrip + bmi2 + ever_smoked + br015_ + ep005_ + co007_) +
  mar_stat*(hhsize + ch001_ + sp002_mod + chronic_mod + casp + eurod + 
              hc002_mod + hc012_ + hc029_ + iadlza + adla + maxgrip + bmi2 + 
              ever_smoked + br015_ + ep005_ + co007_) + 
  hhsize*(ch001_ + sp002_mod + chronic_mod + casp + eurod + hc002_mod + hc012_ + 
            hc029_ + iadlza + adla + maxgrip + bmi2 + ever_smoked + br015_ + ep005_ + co007_) + 
  ch001_*(sp002_mod + chronic_mod + casp + eurod + 
            hc002_mod + hc012_ + hc029_ + iadlza + adla + maxgrip + bmi2 + 
            ever_smoked + br015_ + ep005_ + co007_) + 
  sp002_mod*(chronic_mod + casp + eurod + hc002_mod + hc012_ + hc029_ + iadlza +
               adla + maxgrip + bmi2 + ever_smoked + br015_ + ep005_ + co007_) +
  chronic_mod*(casp + eurod + hc002_mod + hc012_ + hc029_ + iadlza + adla +
                 maxgrip + bmi2 + ever_smoked + br015_ + ep005_ + co007_) +
  casp*(eurod + hc002_mod + hc012_ + hc029_ + iadlza + adla + maxgrip + bmi2 + 
          ever_smoked + br015_ + ep005_ + co007_) + 
  eurod*(hc002_mod + hc012_ + hc029_ + iadlza + adla + maxgrip + bmi2 + 
           ever_smoked + br015_ + ep005_ + co007_) + 
  hc002_mod*(hc012_ + hc029_ + iadlza + adla + maxgrip + bmi2 + 
  ever_smoked + br015_ + ep005_ + co007_) +
  hc012_*(hc029_ + iadlza + adla + maxgrip + bmi2 + ever_smoked + br015_ + 
            ep005_ + co007_) + 
  hc029_*(iadlza + adla + maxgrip + bmi2 + ever_smoked + br015_ + ep005_ + co007_) + 
  iadlza*(adla + maxgrip + bmi2 + ever_smoked + br015_ + ep005_ + co007_) + 
  adla*(maxgrip + bmi2 + ever_smoked + br015_ + ep005_ + co007_) + 
  maxgrip*(bmi2 + ever_smoked + br015_ + ep005_ + co007_) + 
  bmi2*(ever_smoked + br015_ + ep005_ + co007_) + 
  ever_smoked*(br015_ + ep005_ + co007_) + br015_*(ep005_ + co007_) + ep005_:co007_
  

# Define model
fit1 <- lm(mod, data = na.omit(training_data))

# Display model assumptions through residuals
par(mfrow = c(2, 2))
plot(fit1)

summary(fit1)
step(fit1)