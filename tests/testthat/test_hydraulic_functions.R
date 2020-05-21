context("Just testing the hydraulic functions")

test_that(desc = "Test dynamic viscosity, water density and kinematic_viscosity",
          {
            expect_equal( dynamic_viscosity(10),   1.3080, tolerance = 0.02)
            expect_equal( dynamic_viscosity(),     1.1334, tolerance = 0.02)
            expect_equal( dynamic_viscosity(20),   1.0020, tolerance = 0.01)
            expect_equal( dynamic_viscosity(30),   0.7978, tolerance = 0.01)
            expect_equal( dynamic_viscosity(40),   0.6531, tolerance = 0.01)
            expect_equal( dynamic_viscosity(50),   0.5471, tolerance = 0.01)
            expect_equal( dynamic_viscosity(60),   0.4658, tolerance = 0.01)
            expect_equal( dynamic_viscosity(70),   0.4044, tolerance = 0.01)
            expect_equal( dynamic_viscosity(80),   0.3550, tolerance = 0.01)

            expect_equal( water_density(10),  1008.3, tolerance = 0.1)
            expect_equal( water_density(),    1005.2, tolerance = 0.1)
            expect_equal( water_density(20),   998.2, tolerance = 0.1)
            expect_equal( water_density(40),   992.2, tolerance = 0.1)
            expect_equal( water_density(60),   983.2, tolerance = 0.1)
            expect_equal( water_density(80),   971.8, tolerance = 0.1)

            expect_equal( kinematic_viscosity(10), 1.3065 , tolerance = 0.02)
            expect_equal( kinematic_viscosity(),   1.1275 , tolerance = 0.02)
            expect_equal( kinematic_viscosity(20), 1.0035 , tolerance = 0.01)
            expect_equal( kinematic_viscosity(25), 0.8927 , tolerance = 0.01)
            expect_equal( kinematic_viscosity(30), 0.8007 , tolerance = 0.01)
            expect_equal( kinematic_viscosity(40), 0.6579 , tolerance = 0.01)
            expect_equal( kinematic_viscosity(50), 0.5531 , tolerance = 0.01)
            expect_equal( kinematic_viscosity(60), 0.4740 , tolerance = 0.01)
            expect_equal( kinematic_viscosity(70), 0.4127 , tolerance = 0.01)
            expect_equal( kinematic_viscosity(80), 0.3643 , tolerance = 0.01)

          }
)
