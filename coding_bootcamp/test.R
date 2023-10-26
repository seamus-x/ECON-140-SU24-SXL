wateruse <- haven::read_dta("~/ECON-140-FA23-2/coding_bootcamp/wateruse.dta")

wateruse$gpd <- with(wateruse, unit * 748 / num_days)

wateruse <- na.omit(wateruse)

wateruse_tr <- wateruse[sample(1:dim(wateruse)[1], round(dim(wateruse)[1]/2, 0)), ]
index_tr <- sample(1:)

lm1 <- lm(gpd ~ factor(zip) + poly(yearbuilt, 2) + poly(homesize, 2) +
            poly(num_baths, 2) + poly(num_beds, 2) + poly(num_rooms, 2) +
            poly(homeval, 2) + poly(lotsize, 2) + poly(degree_days, 2) +
            poly(precip, 2), data = wateruse)


summary(lm1)
sum(predict(lm1) - lm1$fitted.values)
