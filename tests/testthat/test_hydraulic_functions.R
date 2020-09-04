  context("Just testing the hydraulic functions")

  test_that(desc = "Test dynamic viscosity, water density and kinematic_viscosity",
            {
              expect_equal( dynamic_viscosity(10),   1.3080, tolerance = 0.02)
              expect_equal( dynamic_viscosity(),     1.1334, tolerance = 0.02)
              expect_equal( dynamic_viscosity(20),   1.0020, tolerance = 0.02)
              expect_equal( dynamic_viscosity(30),   0.7978, tolerance = 0.02)
              expect_equal( dynamic_viscosity(40),   0.6531, tolerance = 0.02)
              expect_equal( dynamic_viscosity(50),   0.5471, tolerance = 0.02)
              expect_equal( dynamic_viscosity(60),   0.4658, tolerance = 0.02)
              expect_equal( dynamic_viscosity(70),   0.4044, tolerance = 0.02)
              expect_equal( dynamic_viscosity(80),   0.3550, tolerance = 0.02)

              expect_equal( water_density(10),  1008.3, tolerance = 0.02)
              expect_equal( water_density(),    1005.2, tolerance = 0.02)
              expect_equal( water_density(20),   998.2, tolerance = 0.02)
              expect_equal( water_density(40),   992.2, tolerance = 0.02)
              expect_equal( water_density(60),   983.2, tolerance = 0.02)
              expect_equal( water_density(80),   971.8, tolerance = 0.02)

              expect_equal( kinematic_viscosity(10), 1.3065 , tolerance = 0.02)
              expect_equal( kinematic_viscosity(),   1.1275 , tolerance = 0.02)
              expect_equal( kinematic_viscosity(20), 1.0035 , tolerance = 0.02)
              expect_equal( kinematic_viscosity(25), 0.8927 , tolerance = 0.02)
              expect_equal( kinematic_viscosity(30), 0.8007 , tolerance = 0.02)
              expect_equal( kinematic_viscosity(40), 0.6579 , tolerance = 0.02)
              expect_equal( kinematic_viscosity(50), 0.5531 , tolerance = 0.02)
              expect_equal( kinematic_viscosity(60), 0.4740 , tolerance = 0.02)
              expect_equal( kinematic_viscosity(70), 0.4127 , tolerance = 0.02)
              expect_equal( kinematic_viscosity(80), 0.3643 , tolerance = 0.02)

            }
  )

  test_that(desc = "Test reynolds number and friction_colebrook",
           {
             expect_equal( reynolds_number(flow = 0.042, dn = 0.150, temp = 14.555), 312725, tolerance = 0.1)

             expect_equal( reynolds_number(flow = 0.00025, dn = 0.200, temp = 14.555),   1374.11023, tolerance = 0.1)
             expect_equal( reynolds_number(flow = 0.00030, dn = 0.200, temp = 14.555),   1648.93227, tolerance = 0.1)
             expect_equal( reynolds_number(flow = 0.00040, dn = 0.200, temp = 14.555),   2198.57637, tolerance = 0.1)
             expect_equal( reynolds_number(flow = 0.00050, dn = 0.200, temp = 14.555),   2748.22046, tolerance = 0.1)
             expect_equal( reynolds_number(flow = 0.00070, dn = 0.200, temp = 14.555),   3847.50864, tolerance = 0.1)
             expect_equal( reynolds_number(flow = 0.00090, dn = 0.200, temp = 14.555),   4946.79682, tolerance = 0.1)
             expect_equal( reynolds_number(flow = 0.00100, dn = 0.200, temp = 14.555),   5496.44091, tolerance = 0.1)
             expect_equal( reynolds_number(flow = 0.00200, dn = 0.200, temp = 14.555),   10992.8818, tolerance = 0.1)
             expect_equal( reynolds_number(flow = 0.00400, dn = 0.200, temp = 14.555),   21985.7637, tolerance = 0.1)
             expect_equal( reynolds_number(flow = 0.00700, dn = 0.200, temp = 14.555),   38475.0864, tolerance = 0.1)
             expect_equal( reynolds_number(flow = 0.01000, dn = 0.200, temp = 14.555),   54964.4091, tolerance = 0.1)
             expect_equal( reynolds_number(flow = 0.02000, dn = 0.200, temp = 14.555),  109928.8180, tolerance = 0.1)
             expect_equal( reynolds_number(flow = 0.04000, dn = 0.200, temp = 14.555),  219857.6370, tolerance = 0.1)
             expect_equal( reynolds_number(flow = 0.07000, dn = 0.200, temp = 14.555),  384750.8640, tolerance = 0.1)
             expect_equal( reynolds_number(flow = 0.10000, dn = 0.200, temp = 14.555),  549644.0910, tolerance = 0.1)
             expect_equal( reynolds_number(flow = 0.15000, dn = 0.200, temp = 14.555),  824466.1370, tolerance = 0.1)
             expect_equal( reynolds_number(flow = 0.20000, dn = 0.200, temp = 14.555), 1099288.1800, tolerance = 0.1)
             expect_equal( reynolds_number(flow = 0.30000, dn = 0.200, temp = 14.555), 1648932.2700, tolerance = 0.1)
             expect_equal( reynolds_number(flow = 0.40000, dn = 0.200, temp = 14.555), 2198576.3700, tolerance = 0.1)

             expect_equal( friction_colebrook(flow = 0.04200, dn = 0.150, roughness = 1.5e-6, temp = 14.555), 0.01446000, tolerance = 0.01)

             expect_equal( friction_colebrook(flow = 0.00025, dn = 0.200, roughness = 1.5e-4, temp = 14.555), 0.04657560, tolerance = 0.02)
             expect_equal( friction_colebrook(flow = 0.00030, dn = 0.200, roughness = 1.5e-4, temp = 14.555), 0.03881230, tolerance = 0.02)
             expect_equal( friction_colebrook(flow = 0.00040, dn = 0.200, roughness = 1.5e-4, temp = 14.555), 0.04835293, tolerance = 0.01)
             expect_equal( friction_colebrook(flow = 0.00050, dn = 0.200, roughness = 1.5e-4, temp = 14.555), 0.04534967, tolerance = 0.01)
             expect_equal( friction_colebrook(flow = 0.00070, dn = 0.200, roughness = 1.5e-4, temp = 14.555), 0.04110045, tolerance = 0.01)
             expect_equal( friction_colebrook(flow = 0.00090, dn = 0.200, roughness = 1.5e-4, temp = 14.555), 0.03832142, tolerance = 0.01)
             expect_equal( friction_colebrook(flow = 0.00100, dn = 0.200, roughness = 1.5e-4, temp = 14.555), 0.03724566, tolerance = 0.01)
             expect_equal( friction_colebrook(flow = 0.00200, dn = 0.200, roughness = 1.5e-4, temp = 14.555), 0.03128796, tolerance = 0.01)
             expect_equal( friction_colebrook(flow = 0.00400, dn = 0.200, roughness = 1.5e-4, temp = 14.555), 0.02691050, tolerance = 0.01)
             expect_equal( friction_colebrook(flow = 0.00700, dn = 0.200, roughness = 1.5e-4, temp = 14.555), 0.02427912, tolerance = 0.01)
             expect_equal( friction_colebrook(flow = 0.01000, dn = 0.200, roughness = 1.5e-4, temp = 14.555), 0.02295369, tolerance = 0.01)
             expect_equal( friction_colebrook(flow = 0.02000, dn = 0.200, roughness = 1.5e-4, temp = 14.555), 0.02103562, tolerance = 0.01)
             expect_equal( friction_colebrook(flow = 0.04000, dn = 0.200, roughness = 1.5e-4, temp = 14.555), 0.01981617, tolerance = 0.01)
             expect_equal( friction_colebrook(flow = 0.07000, dn = 0.200, roughness = 1.5e-4, temp = 14.555), 0.01920434, tolerance = 0.01)
             expect_equal( friction_colebrook(flow = 0.10000, dn = 0.200, roughness = 1.5e-4, temp = 14.555), 0.01893902, tolerance = 0.01)
             expect_equal( friction_colebrook(flow = 0.15000, dn = 0.200, roughness = 1.5e-4, temp = 14.555), 0.01872298, tolerance = 0.01)
             expect_equal( friction_colebrook(flow = 0.20000, dn = 0.200, roughness = 1.5e-4, temp = 14.555), 0.01861124, tolerance = 0.01)
             expect_equal( friction_colebrook(flow = 0.30000, dn = 0.200, roughness = 1.5e-4, temp = 14.555), 0.01849742, tolerance = 0.01)
             expect_equal( friction_colebrook(flow = 0.40000, dn = 0.200, roughness = 1.5e-4, temp = 14.555), 0.01843941, tolerance = 0.01)

           }
  )

  test_that(desc = "Test Velocity and Darcy-Weisbach function",
          {
            expect_equal( velocity(flow = 0.4, dn = 0.2), 12.7324, tolerance = 0.0001)
            expect_equal( darcy_weisbach( flow = 0.042, pipe_length = 970, dn = 0.150, roughness = 1.5e-6, temp = 14.5), 26.9326,  tolerance = 0.01)
          }
  )


  test_that(desc = "Test vapour_pressure function",
            {
              expect_equal(vapour_pressure(0)*100,     0.6113, tolerance = 0.001)
              expect_equal(vapour_pressure(20)*100,    2.3388, tolerance = 0.001)
              expect_equal(vapour_pressure(35)*100,    5.6267, tolerance = 0.001)
              expect_equal(vapour_pressure(50)*100,   12.3440, tolerance = 0.001)
              expect_equal(vapour_pressure(75)*100,   38.5630, tolerance = 0.001)
              expect_equal(vapour_pressure(100)*100, 101.3200, tolerance = 0.001)
            }
  )

  # https://www.sablesys.com/support/technical-library/barometric-pressure-vs-altitude-table/
  test_that(desc = "Atmospheric pressure",
            {
              expect_equal(atm_pressure(0),    1.0333, tolerance = 0.02)
              expect_equal(atm_pressure(153),  1.0150, tolerance = 0.02)
              expect_equal(atm_pressure(305),  0.9960, tolerance = 0.02)
              expect_equal(atm_pressure(458),  0.9780, tolerance = 0.02)
              expect_equal(atm_pressure(610),  0.9600, tolerance = 0.02)
              expect_equal(atm_pressure(763),  0.9430, tolerance = 0.02)
              expect_equal(atm_pressure(915),  0.9260, tolerance = 0.02)
              expect_equal(atm_pressure(1068), 0.9090, tolerance = 0.02)
              expect_equal(atm_pressure(1220), 0.8920, tolerance = 0.02)
              expect_equal(atm_pressure(1373), 0.8760, tolerance = 0.02)
              expect_equal(atm_pressure(1526), 0.8600, tolerance = 0.02)
              expect_equal(atm_pressure(1831), 0.8280, tolerance = 0.02)
              expect_equal(atm_pressure(2136), 0.7970, tolerance = 0.02)
              expect_equal(atm_pressure(2441), 0.7670, tolerance = 0.02)
              expect_equal(atm_pressure(2746), 0.7380, tolerance = 0.02)
              expect_equal(atm_pressure(3050), 0.7100, tolerance = 0.02)
              expect_equal(atm_pressure(4577), 0.5830, tolerance = 0.02)
            }
  )
