puts "RText service, listening on port 12345"
puts "number of args #{ARGV.length}"
ARGV.each do |arg|
  puts arg
end
STDOUT.flush

require 'socket'
server = TCPServer.new('localhost', 12345)
client = server.accept

10.times do |i|
  s = client.gets
  sleep 2
  client.puts "returning #{s}"
end
client.close
server.close


