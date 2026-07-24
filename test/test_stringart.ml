let () =
  Alcotest.run "stringart"
    [
      Test_image.suite;
      Test_oklab.suite;
      Test_kmeans.suite;
      Test_metrics.suite;
      Test_geometry.suite;
      Test_raster.suite;
      Test_palette.suite;
      Test_solver.suite;
      Test_render.suite;
      Test_export.suite;
    ]
