puts "RText service, listening on port 1234"
puts "number of args #{ARGV.length}"
ARGV.each do |arg|
  puts arg
end
STDOUT.flush
while true
  sleep 1
end
