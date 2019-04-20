local computer = require "computer"
local f = io.open((...), "w")
local size = 0
while true do
  local name, _, x = computer.pullSignal()
  if name == "clipboard" then
    f:write(x)
    size = size + #x
  elseif name == "key_down" and string.lower(string.char(x)) == "x" then
    break
  end
end
f:close()
print(size)
