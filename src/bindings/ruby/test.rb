require 'test/unit'
Test::Unit::AutoRunner.run(RUBY_VERSION >= '1.8.3' ,'./test/',['--pattern=/Test.*\.rb\Z/'])
