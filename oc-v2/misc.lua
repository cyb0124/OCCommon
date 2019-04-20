local startsWith = function(pattern, input)
  return string.sub(input, 1, #pattern) == pattern
end

local endsWith = function(pattern, input)
  return string.sub(input, #input - #pattern + 1) == pattern
end
