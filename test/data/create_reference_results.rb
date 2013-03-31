require '../../../rtext/lib/rtext/frontend/context.rb'

content = File.read('context.txt')
lines = content.split("\n")
for i in 1..lines.length
  current_context = lines[0...i]
  context = RText::Frontend::Context::extract(current_context)
  File.open("context_result_#{i}.txt", "w") do |file|
    file.write(context.join("\n"))
  end
end
