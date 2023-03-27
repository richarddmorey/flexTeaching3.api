
library(flexTeaching3.api)

flexTeaching3.api::ft3_options(master_secret = 'curious_capybara')
flexTeaching3.api::ft3_options(assignments_pkg = 'flexTeaching3.api')

tmpdir = '/tmp/ft3_scratch'

if(dir.exists(tmpdir)){
  unlink(tmpdir,recursive=TRUE, force=TRUE)
}
dir.create(tmpdir)

flexTeaching3.api::ft3_options(cache_location = tmpdir)
flexTeaching3.api::ft3_options(scratch_dir = tmpdir)
flexTeaching3.api::ft3_options(errors_to_client = TRUE)


flexTeaching3.api::ft3_serve_api(
  log_options = list(level = 'debug')
  )

