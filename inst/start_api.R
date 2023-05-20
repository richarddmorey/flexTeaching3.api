
library(flexTeaching3.api)
library(lgr)

tmpdir = '/tmp/ft3_scratch'
if(dir.exists(tmpdir)){
  unlink(tmpdir,recursive=TRUE, force=TRUE)
}
dir.create(tmpdir)

flexTeaching3.api::ft3_options(master_secret = 'curious_capybara')
flexTeaching3.api::ft3_options(assignments_pkg = 'flexTeaching3.api')
flexTeaching3.api::ft3_options(cache_location = tmpdir)
flexTeaching3.api::ft3_options(scratch_dir = tmpdir)
flexTeaching3.api::ft3_options(errors_to_client = TRUE)

# Use root logger lgr and write to file
lgr$set_threshold('debug')
format(Sys.time(), "ft3_api_log_%Y-%m-%d_%H:%M.json.log") |>
  file.path(tmpdir,x=_) |>
  lgr::AppenderJson$new() |>
  lgr$add_appender(name = 'json')

flexTeaching3.api::ft3_serve_api()

