#########
#No/invalid allocation method

test_that("allocate fails when no/invalid allocation method is provided", {
  expect_error(allocate(),
               regexp='argument "allocation" is missing, with no default')
  expect_error(allocate(allocation="random"),
               regexp="'arg' should be one of .proportional., .power., .neyman., .optimal.")
  expect_error(allocate(N.h=c(10,20,30)),
               regexp='argument "allocation" is missing, with no default')
  expect_error(allocate(n.h=c(10,20,30)),
               regexp='unused argument \\(n\\.h = c\\(10, 20, 30\\)\\)')
})

#########
#Proportional allocation method
test_that("Proportional - allocate throws error/warning when invalid inputs are provided", {
  expect_error(allocate("proportional"),
               regexp='argument "N\\.h" is missing, with no default')
  expect_error(allocate("proportional",N.h=c(10,20,30)),
               regexp='The n\\.samp parameter must be specified for allocation=="proportional"')
  expect_warning(allocate("proportional",N.h=c(10,20,30),n.samp=90),
                 regexp='sum\\(N\\.h\\) is less than n.samp')
  expect_error(allocate("proportional",N.h=c(10,20,30),n.samp=5),
               regexp='lbound\\*length\\(N\\.h\\) must be less than or equal to n\\.samp')
  expect_error(allocate("proportional",N.h=numeric(),n.samp=6),
               regexp='The N\\.h parameter must be a vector of positive values \\(integers or non-integers\\)')
  expect_error(allocate("proportional",N.h=c(0,10,20),n.samp=6),
               regexp='The N\\.h parameter must be a vector of positive values \\(integers or non-integers\\)')
  expect_error(allocate("proportional",N.h=c(-10,10,20),n.samp=6),
               regexp='The N\\.h parameter must be a vector of positive values \\(integers or non-integers\\)')
  expect_error(allocate("proportional",N.h=c(10,20,30),n.samp=0),
               regexp='1: The n\\.samp parameter must be a positive integer of length 1\n2: lbound\\*length\\(N\\.h\\) must be less than or equal to n.samp')
  expect_error(allocate("proportional",N.h=c(10,20,30),n.samp=-1),
               regexp='1: The n\\.samp parameter must be a positive integer of length 1\n2: lbound\\*length\\(N\\.h\\) must be less than or equal to n.samp')
  expect_error(allocate("proportional",N.h=c(10,20,30),n.samp=c(1,10)),
               regexp='1: The n\\.samp parameter must be a positive integer of length 1\n2: lbound\\*length\\(N\\.h\\) must be less than or equal to n.samp')

})



test_that("Proportional - proportions in returned sample match the relative proportions in N.h (when min(n.samp*N.h/sum(N.h)) > lbound and all((n.samp*N.h/sum(N.h)) %% 1 == 0)", {
  #Formula: .n.samp*.N.h/sum(.N.h)
  expect_equal(allocate("proportional",N.h=c(2,4),n.samp=3,lbound=1),
               c(1,2))
  expect_equal(allocate("proportional",N.h=c(5,5),n.samp=4,lbound=1),
               c(2,2))
  expect_equal(allocate("proportional",N.h=c(20,30,40),n.samp=9,lbound=2),
               c(2,3,4))
  expect_equal(allocate("proportional",N.h=c(25,50,100),n.samp=21,lbound=3),
               c(3,6,12))
})


test_that("Proportional - does sum(rounded_allocations) = n.samp?", {
  expect_equal(sum(allocate("proportional",N.h=c(2,4),n.samp=3,lbound=1)),
               3)
  expect_equal(sum(allocate("proportional",N.h=c(5,5),n.samp=4,lbound=1)),
               4)
  expect_equal(sum(allocate("proportional",N.h=c(20,30,40),n.samp=9)),
               9)
  expect_equal(sum(allocate("proportional",N.h=c(25,50,100),n.samp=21,lbound=3)),
               21)
  expect_equal(sum(allocate("proportional",N.h=c(25,50,100),n.samp=9,lbound=3)),
               9)
})


test_that("Proportional - does min(rounded_allocations) >= lbound?", {
  expect_gte(min(allocate("proportional",N.h=c(2,4),n.samp=3,lbound=1)),
             1)
  expect_gte(min(allocate("proportional",N.h=c(5,5),n.samp=4,lbound=1)),
             1)
  expect_gte(min(allocate("proportional",N.h=c(20,30,40),n.samp=9)),
             2)
  expect_gte(min(allocate("proportional",N.h=c(25,50,100),n.samp=21,lbound=3)),
             3)
  expect_gte(min(allocate("proportional",N.h=c(25,50,100),n.samp=9,lbound=3)),
             3)
})



#########
#Power allocation method

test_that("Power - allocate throws error/warning when invalid inputs are provided", {
  expect_error(allocate("power"),
               regexp='argument "N\\.h" is missing, with no default')
  expect_error(allocate("power",N.h=c(10,20,30)),
               regexp='1: The n\\.samp parameter must be specified for allocation=="power"\n2: The power parameter must be specified for allocation=="power"')
  expect_error(allocate("power", N.h = c(10, 20, 30), n.samp = 90),
               regexp='The power parameter must be specified for allocation=="power"')
  expect_error(allocate("power", N.h = c(10, 20, 30), n.samp = 90, power = -0.5),
               regexp='The power parameter must be a positive value between 0 and 1, inclusive')
  expect_error(allocate("power", N.h = c(10, 20, 30), n.samp = 90, power = 2),
               regexp='The power parameter must be a positive value between 0 and 1, inclusive')
  expect_warning(allocate("power",N.h=c(10,20,30),n.samp=90, power = 0.5),
                 regexp='sum\\(N\\.h\\) is less than n.samp')
  expect_error(allocate("power",N.h=c(10,20,30),n.samp=5,power=0.5),
               regexp='lbound\\*length\\(N\\.h\\) must be less than or equal to n\\.samp')
})

test_that("Power - proportions in returned sample match the unrounded sample (when min(n.samp*N.h/sum(N.h)) > lbound and expected unrounded sample are all integers)", {
  #Formula:
  #N.h.powered <- N.h**power
  #N.powered <- sum(N.h.powered)
  #allocations <- n.samp * N.h.powered / N.powered
  expect_equal(allocate("power",power=0.5,N.h=c(4,4,16),n.samp=20),
               c(5,5,10))
  expect_equal(allocate("power",power=0.25,N.h=c(16,81),n.samp=10),
               c(4,6))
  expect_equal(allocate("power",power=1/3,N.h=c(8,27),n.samp=15),
               c(6,9))
  expect_equal(allocate("power",power=1/2,N.h=c(9,16,25),n.samp=24),
               c(6,8,10))
})


test_that("Power - does sum(rounded_allocations) = n.samp?", {
  expect_equal(sum(allocate("power",power=0.5,N.h=c(4,4,16),n.samp=20)),
               20)
  expect_equal(sum(allocate("power",power=0.25,N.h=c(16,81),n.samp=10)),
               10)
  expect_equal(sum(allocate("power",power=1/3,N.h=c(8,27),n.samp=15)),
               15)
  expect_equal(sum(allocate("power",power=1/2,N.h=c(9,16,25),n.samp=24)),
               24)
  expect_equal(sum(allocate("power",power=1/2,N.h=c(9,16,25),n.samp=24,lbound=8)),
               24)
})


test_that("Power - does min(rounded_allocations) >= lbound?", {
  expect_gte(min(allocate("power",power=0.5,N.h=c(4,4,16),n.samp=20)),
             2)
  expect_gte(min(allocate("power",power=0.25,N.h=c(16,81),n.samp=10)),
             2)
  expect_gte(min(allocate("power",power=1/3,N.h=c(8,27),n.samp=15)),
             2)
  expect_gte(min(allocate("power",power=1/2,N.h=c(9,16,25),n.samp=24)),
             2)
  expect_gte(min(allocate("power",power=1/2,N.h=c(9,16,25),n.samp=24,lbound=8)),
             8)
})


#########
#Neyman allocation method

test_that("Neyman - allocate throws error/warning when invalid inputs are provided", {
  expect_error(allocate("neyman"),
               regexp='argument "N\\.h" is missing, with no default')
  expect_error(allocate("neyman",N.h=c(10,20,30)),
               regexp='1: The n\\.samp parameter must be specified for allocation=="neyman"\n2: The S\\.h parameter must be specified for allocation=="neyman"')
  expect_error(allocate("neyman", N.h = c(10, 20, 30), n.samp = 90),
               regexp='The S\\.h parameter must be specified for allocation=="neyman"')
  expect_error(allocate("neyman", N.h = c(10, 20, 30), n.samp = 90, S.h = 0.5),
               regexp='The S\\.h parameter must be a vector of positive values \\(integers or non-integers\\) that are the same length as N\\.h')
  expect_error(allocate("neyman", N.h = c(10, 20), n.samp = 90, S.h = c(-0.5,1.5)),
               regexp='The S\\.h parameter must be a vector of positive values \\(integers or non-integers\\) that are the same length as N\\.h')
  expect_error(allocate("neyman", N.h = c(10, 20, 30), n.samp = 90, S.h = c(0,1,2)),
               regexp='The S\\.h parameter must be a vector of positive values \\(integers or non-integers\\) that are the same length as N\\.h')
  expect_warning(allocate("neyman",N.h=c(10,20,30),n.samp=90, S.h = c(0.5,1.5,2.5)),
                 regexp='sum\\(N\\.h\\) is less than n.samp')
  expect_error(allocate("neyman",N.h=c(20,30),n.samp=3,S.h=c(1,2)),
               regexp='lbound\\*length\\(N\\.h\\) must be less than or equal to n\\.samp')
})

test_that("Neyman - proportions in returned sample match the unrounded sample (when min(n.samp*N.h/sum(N.h)) > lbound and expected unrounded sample are all integers)", {
  #Formula:
  #propNum <- N.h * S.h # Numerator
  #propDen <- sum(propNum) # Denominator
  #allocations <- n.samp * propNum / propDen
  expect_equal(allocate("neyman", N.h=c(8,8,32), n.samp=20, S.h=c(1,1,2)),
               c(2,2,16))
  expect_equal(allocate("neyman",N.h=c(10,10,10),n.samp=20,S.h=c(1,1,2)),
               c(5, 5, 10))
  expect_equal(allocate("neyman",N.h=c(30,60),n.samp=12,S.h=c(1,1)),
               c(4,8))
  expect_equal(allocate("neyman",N.h=c(25,50,100),n.samp=99,S.h=c(5,15,2.5),lbound=1),
               c(11,66,22))
})


test_that("Neyman - does sum(rounded_allocations) = n.samp?", {
  expect_equal(sum(allocate("neyman", N.h=c(8,8,32), n.samp=20, S.h=c(1,1,2))),
               20)
  expect_equal(sum(allocate("neyman",N.h=c(10,10,10),n.samp=20,S.h=c(1,1,2))),
               20)
  expect_equal(sum(allocate("neyman",N.h=c(30,60),n.samp=12,S.h=c(1,1))),
               12)
  expect_equal(sum(allocate("neyman",N.h=c(25,50,100),n.samp=99,S.h=c(5,15,2.5),lbound=1)),
               99)
  expect_equal(sum(allocate("neyman",N.h=c(100,50,150),n.samp=100,S.h=c(7.5,10,2.5),lbound=25)),
               100)
})


test_that("Neyman - does min(rounded_allocations) >= lbound?", {
  expect_gte(min(allocate("neyman", N.h=c(8,8,32), n.samp=20, S.h=c(1,1,2))),
             2)
  expect_gte(min(allocate("neyman",N.h=c(10,10,10),n.samp=20,S.h=c(1,1,2))),
             2)
  expect_gte(min(allocate("neyman",N.h=c(30,60),n.samp=12,S.h=c(1,1))),
             2)
  expect_gte(min(allocate("neyman",N.h=c(25,50,100),n.samp=99,S.h=c(5,15,2.5),lbound=1)),
             1)
  expect_gte(min(allocate("neyman",N.h=c(100,50,150),n.samp=100,S.h=c(7.5,10,2.5),lbound=25)),
             25)
})


#########
#Optimal (cost) allocation method

test_that("Optimal (cost) - allocate throws error/warning when invalid inputs are provided", {
  expect_error(allocate("optimal"),
               regexp='argument "N\\.h" is missing, with no default')
  expect_error(allocate("optimal",N.h=c(10,20,30)),
               regexp='1: The S\\.h parameter must be specified for allocation=="optimal"\n2: The c\\.h parameter must be specified for allocation=="optimal"\n3: Exactly one of the cost and variance parameters should be supplied for allocation=="optimal"')
  expect_error(allocate("optimal", N.h = c(10, 20, 30), c.h = c(3,2,1)),
               regexp='1: The S\\.h parameter must be specified for allocation=="optimal"\n2: Exactly one of the cost and variance parameters should be supplied for allocation=="optimal"')
  expect_error(allocate("optimal", N.h = c(10, 20, 30), c.h = c(3,2,1), S.h = c(0.5,1,1.5)),
               regexp='Exactly one of the cost and variance parameters should be supplied for allocation=="optimal"')
  expect_error(allocate("optimal", N.h = c(10, 20, 30), c.h = c(3,2,1), S.h = c(0.5,1,1.5),cost=100,variance=5),
               regexp='Exactly one of the cost and variance parameters should be supplied for allocation=="optimal"')
  expect_error(allocate("optimal", N.h = c(10, 20, 30), c.h = c(3,2,1), S.h = c(0.5,1,1.5),cost=0),
               regexp='The cost parameter must be a positive value \\(integer or non-integer\\)')
  expect_error(allocate("optimal", N.h = c(10, 20, 30), c.h = c(3,2,1), S.h = c(0.5,1,1.5),cost=c(100,50)),
               regexp='The cost parameter must be a positive value \\(integer or non-integer\\)')
  expect_error(allocate("optimal", N.h = c(10, 20, 30), c.h = c(0,2,1), S.h = c(0.5,1,1.5),cost=100),
               regexp='The c\\.h parameter must be a vector of positive values \\(integers or non-integers\\) that are the same length as N.h')
  expect_error(allocate("optimal", N.h = c(10, 20, 30), c.h = c(3,2), S.h = c(0.5,1,1.5),cost=100),
               regexp='The c\\.h parameter must be a vector of positive values \\(integers or non-integers\\) that are the same length as N.h')
})

test_that("Optimal (cost) - proportions in returned sample match the unrounded sample (when min(n.samp*N.h/sum(N.h)) > lbound and expected unrounded sample are all integers)", {
  #Formula:
  #propNum <- N.h * S.h / sqrt(c.h)
  #propDen <- sum(N.h * S.h * sqrt(c.h))
  #allocations <- cost * propNum / propDen
  expect_equal(allocate("optimal",N.h=c(100,150),S.h=c(1.5,1),c.h=c(1,4),cost=24),
               c(8,4))
  expect_equal(allocate("optimal",N.h=c(100,75,150),S.h=c(1,1,1.5),c.h=c(1,1,4) ,cost=100),
               c(16, 12, 18))
})


test_that("Optimal (cost) - does the total cost of the allocated sample align with the cost parameter (when a solution exists (e.g., (sum(lbound*c.h) > cost)?", {
  expect_equal(sum(c(1,4)*allocate("optimal",N.h=c(100,150),S.h=c(1.5,1),c.h=c(1,4),cost=24)),
               24)
  expect_equal(sum(c(1,1,4)*allocate("optimal",N.h=c(100,75,150),S.h=c(1,1,1.5),c.h=c(1,1,4),cost=100)),
               100)
})


test_that("Optimal (cost) - does min(rounded_allocations) >= lbound?", {
  expect_gte(min(allocate("optimal",N.h=c(100,150),S.h=c(1.5,1),c.h=c(1,4),cost=24,lbound=4)),
             4)
  expect_gte(min(allocate("optimal",N.h=c(100,75,150),S.h=c(1,1,1.5),c.h=c(1,1,4),cost=100),lbound=12),
             12)
  expect_warning(expect_gte(min(allocate("optimal",N.h=c(100,75,150),S.h=c(1,1,1.5),c.h=c(1,4,4),cost=5)),
                            2))
})


#########
#Optimal (variance) allocation method

test_that("Optimal (variance) - allocate throws error/warning when invalid inputs are provided", {
  expect_error(allocate("optimal"),
               regexp='argument "N\\.h" is missing, with no default')
  expect_error(allocate("optimal",N.h=c(10,20,30)),
               regexp='1: The S\\.h parameter must be specified for allocation=="optimal"\n2: The c\\.h parameter must be specified for allocation=="optimal"\n3: Exactly one of the cost and variance parameters should be supplied for allocation=="optimal"')
  expect_error(allocate("optimal", N.h = c(10, 20, 30), c.h = c(3,2,1)),
               regexp='1: The S\\.h parameter must be specified for allocation=="optimal"\n2: Exactly one of the cost and variance parameters should be supplied for allocation=="optimal"')
  expect_error(allocate("optimal", N.h = c(10, 20, 30), c.h = c(3,2,1), S.h = c(0.5,1,1.5)),
               regexp='Exactly one of the cost and variance parameters should be supplied for allocation=="optimal"')
  expect_error(allocate("optimal", N.h = c(10, 20, 30), c.h = c(3,2,1), S.h = c(0.5,1,1.5),cost=100,variance=5),
               regexp='Exactly one of the cost and variance parameters should be supplied for allocation=="optimal"')
  expect_error(allocate("optimal", N.h = c(10, 20, 30), c.h = c(3,2,1), S.h = c(0.5,1,1.5),variance=0),
               regexp='The variance parameter must be a positive value \\(integer or non-integer\\)')
  expect_error(allocate("optimal", N.h = c(10, 20, 30), c.h = c(3,2,1), S.h = c(0.5,1,1.5),variance=c(100,50)),
               regexp='The variance parameter must be a positive value \\(integer or non-integer\\)')
  expect_error(allocate("optimal", N.h = c(10, 20, 30), c.h = c(0,2,1), S.h = c(0.5,1,1.5),variance=100),
               regexp='The c\\.h parameter must be a vector of positive values \\(integers or non-integers\\) that are the same length as N.h')
  expect_error(allocate("optimal", N.h = c(10, 20, 30), c.h = c(3,2), S.h = c(0.5,1,1.5),variance=100),
               regexp='The c\\.h parameter must be a vector of positive values \\(integers or non-integers\\) that are the same length as N.h')
})

test_that("Optimal (variance) - proportions in returned sample match the unrounded sample (when min(n.samp*N.h/sum(N.h)) > lbound and expected unrounded sample are all integers)", {
  #Formula:
  #propNum <- sum(N.h * S.h * sqrt(c.h))
  #propDen <- variance * sum(N.h)**2 + sum(N.h * S.h**2)
  #allocations <- N.h * S.h / sqrt(c.h) * propNum / propDen
  expect_equal(allocate("optimal",N.h=c(36,64),S.h=c(9,9),c.h=c(4,1),variance=1),
               c(11,39))
  expect_equal(allocate("optimal",N.h=c(100,64,144),S.h=c(49,36,64),c.h=c(16,4,9),variance=3.5),
               c(51,48,128))
})



test_that("Optimal (variance) - does min(rounded_allocations) >= lbound?", {
  expect_gte(min(allocate("optimal",N.h=c(36,64),S.h=c(9,9),c.h=c(4,1),variance=1)),
               2)
  expect_gte(min(allocate("optimal",N.h=c(100,64,144),S.h=c(49,36,64),c.h=c(16,4,9),variance=3.5)),
               2)
  expect_gte(min(allocate("optimal",N.h=c(100,75,150),S.h=c(1,1,1.5),c.h=c(1,4,4),variance=5)),
             2)
})
