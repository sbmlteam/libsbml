require 'test/unit'
if RUBY_VERSION >= '1.9'
 testfiles = File.join("test/**", "Test*.rb")
 files = Dir::glob(testfiles)
 Test::Unit.setup_argv {files
  if files.size == 1
    $0 = File.basename(files[0])
  else
    $0 = files.to_s
  end
  files
 }
else
 Test::Unit::AutoRunner.run(RUBY_VERSION >= '1.8.3' ,'./test/',['--pattern=/Test.*\.rb\Z/'])
end
