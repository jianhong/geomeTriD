test_that('threeJsGeometry works not correct', {
  expect_error(threeJsGeometry(type="line"))
  expect_s4_class(threeJsGeometry(type="line", properties=list(size=1)),
                  "threeJsGeometry")
  expect_error(threeJsGeometry(type="arrow", properties=list(size=1)))
  expect_s4_class(threeJsGeometry(type="arrow",
                                  properties=list(size=1,
                                                  headLength=1,
                                                  headWidth=1)),
                  "threeJsGeometry")
  expect_error(threeJsGeometry(type="segment"))
  expect_s4_class(threeJsGeometry(type="segment",
                                  properties=list(size=1)),
                  "threeJsGeometry")
  expect_error(threeJsGeometry(type="box"))
  expect_s4_class(threeJsGeometry(type="box",
                                  properties=list(width=1,
                                                  height=1,
                                                  depth=1)),
                  "threeJsGeometry")
  expect_error(threeJsGeometry(type="capsule"))
  expect_s4_class(threeJsGeometry(type="capsule",
                                  properties=list(radius=1,
                                                  height=1)),
                  "threeJsGeometry")
  expect_error(threeJsGeometry(type="cone"))
  expect_s4_class(threeJsGeometry(type="cone",
                                  properties=list(radius=1,
                                                  height=1)),
                  "threeJsGeometry")
  expect_error(threeJsGeometry(type="cylinder"))
  expect_s4_class(threeJsGeometry(type="cylinder",
                                  properties=list(radiusTop=1,
                                                  radiusBottom=1,
                                                  height=1)),
                  "threeJsGeometry")
  for(type in c("dodecahedron", "icosahedron", "octahedron", "sphere",
                "tetrahedron")){
    expect_s4_class(threeJsGeometry(type=type,
                                    properties=list(radius=1)),
                    "threeJsGeometry")
  }
  
  expect_error(threeJsGeometry(type="label"))
  expect_s4_class(threeJsGeometry(type="label",
                                  properties=list(label=1,
                                                  size=1)),
                  "threeJsGeometry")
  expect_error(threeJsGeometry(type="text",
                               properties=list(label=c(1, 2),
                                               font='some font',
                                               size=1,
                                               depth=1)))
  expect_s4_class(threeJsGeometry(type="text",
                                  properties=list(label=1,
                                                  font='some font',
                                                  size=1,
                                                  depth=1)),
                  "threeJsGeometry")
  expect_error(threeJsGeometry(type="torus"))
  expect_s4_class(threeJsGeometry(type="torus",
                                  properties=list(radius=1,
                                                  tube=1)),
                  "threeJsGeometry")
})