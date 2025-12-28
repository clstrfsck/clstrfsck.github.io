ranges_str, ingredients_str = File.read("y25d05.txt").split("\n\n")

ranges = ranges_str.lines.map { |l| a, b = l.split('-').map(&.to_i64); a..b }
ingredients = ingredients_str.lines.map(&.to_i64)

result1 = ingredients.count { |i| ranges.any?(&.includes?(i)) }
puts "Result1: #{result1}"

sorted = ranges.sort_by(&.begin)
merged = sorted.each_with_object([] of Range(Int64, Int64)) do |r, acc|
  (l = acc.last?) && l.end >= r.begin ?
    (acc[-1] = l.begin..Math.max(l.end, r.end)) :
    (acc << r)
end

result2 = merged.sum { |r| r.end - r.begin + 1 }
puts "Result2: #{result2}"