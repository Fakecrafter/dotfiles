#!/usr/bin/env ruby
require 'nokogiri'
require 'open-uri'
require 'uri'

## extract entries from dictionary
def extractEntries entries
  table = []
  entries.each do |entry|
    en = entry.css("side[lang='fr'] words")
    de = entry.css("side[lang='de'] words")

    # join word if multiple
    if en.children.size > 1
      en = en.children.map{ |c| c.text }.join(", ")
    else
      en = en.text
    end

    if de.children.size > 1
      de = de.children.map{ |c| c.text }.join(", ")
    else
      de = de.text
    end

    table.push([en, de])
  end

  return table
end

###############################################################################
## Main

# stop if no argument is given
exit 0 unless ARGV[0]

# query
query = URI.encode_www_form_component(ARGV[0])
url = "http://dict.leo.org/dictQuery/m-vocab/frde/query.xml?tolerMode=nof&lp=ende&lang=de&rmWords=off&rmSearch=on&search=#{query}&resultOrder=basic&multiwordShowSingle=on&sectLenMax=16&n=1"
doc = Nokogiri::XML(URI.open(url))


# extract translations
table = []

## get translations

# extract best results
sections = doc.css("sectionlist[sectionsort='bestPrio']").children

# go through section
sections.each do |section|

  # get entries
  entries = section.css("entry")

  # show section title if we have entries
  if entries.size > 0
    table.push ["--- #{section["sctTitle"]}", ""] rescue table.push ["---", ""]
  end

  # extract translations
  table.concat extractEntries( entries )
end

## get synonyms
syn_en = doc.css("ffsynlist side[lang='en'] word").children.map { |e| e.text }
syn_de = doc.css("ffsynlist side[lang='de'] word").children.map { |e| e.text }
# transform for proper view
syn_max = [syn_en.size, syn_de.size].max
syn_en.fill("", syn_en.size...syn_max)
syn_de.fill("", syn_en.size...syn_max)
# add when there are some
if syn_en.size > 0 || syn_de.size >0
  table.push ["--- Synonyme EN", "--- Synonyme DE"]
  table.concat [syn_en, syn_de].transpose
end

# get max lenght of word
len_max = table.map{ |t| t.map{ |w| w.size rescue 0 } }.flatten.max
len_max = [50, len_max.to_i].min # upper bound for tabs (max. possible width)

# print result
table.each do |t|
  printf "%-#{len_max}s %s\n", t[0], t[1]
end
