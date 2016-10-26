## ✅ normalised.path.and.dir.exists -----------------------------------------------------------
expect_error(normalised.path.and.dir.exists('/dska/bla'))
expect_error(normalised.path.and.dir.exists('/dsk'))
expect_error(normalised.path.and.dir.exists('/dsk/'))
expect_error(normalised.path.and.dir.exists('/dsk/../../'))
