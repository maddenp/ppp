def version_check
  version="1.9"
  if RUBY_VERSION < version
    puts "Ruby #{version}.x or greater required."
    exit 1
  end
end
