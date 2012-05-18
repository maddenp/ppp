ENV['GEM_PATH']=Dir.pwd+'/gems'

require 'rubygems'
require 'treetop'
require 'downcase.rb'
require 'downcase.tt'

def clean(tree)
  unless tree.elements.nil?
    tree.elements.delete_if { |e| e.class==Treetop::Runtime::SyntaxNode }
    tree.elements.each { |e| clean e }
  end
  tree
end

infile=ARGV[0]
if infile
  unless File.readable?(infile)
    puts "Cannot read input file: #{infile}"
    exit 1
  end
  s=File.open(infile,'rb').read
else
  s=STDIN.read
end

p=DowncaseParser.new
tree=clean(p.parse(s))

if tree.nil?
  puts "#{p.failure_reason}: #{p.failure_line}:#{p.failure_column}"
else
  puts "#{tree}"
end
