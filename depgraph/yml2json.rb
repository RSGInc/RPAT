require 'rubygems'
require 'yaml'
require 'json'
require 'ap'

Dir.glob('*.yml').each do |yml| 
    File.open("#{yml.gsub('.yml','')}.json", 'w') do |f| 
        torb = YAML.load_file(yml)
        f.write(JSON.pretty_generate(torb)) 
    end
end
