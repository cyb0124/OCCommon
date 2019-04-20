local waitUntil = function(t1)
  while true do
    local td = t1 - computer.uptime()
    if td <= 0 then break end
    computer.pullSignal(td)
  end
end

local sides = {
  bottom = 0, down  = 0, yn = 0,
  top    = 1, up    = 1, yp = 1,
  back   = 2, north = 2, zn = 2,
  front  = 3, south = 3, zp = 3,
  right  = 4, west  = 4, xn = 4,
  left   = 5, east  = 5, xp = 5
}

local rs = component.proxy(component.list("redstone")())
local inv = component.proxy(component.list("transposer")())

while true do
  local t0 = computer.uptime()
  if inv.getSlotStackSize(sides.east, 1) > 0 then
    rs.setOutput(sides.left, 15)
    waitUntil(t0 + 0.1)
    rs.setOutput(sides.back, 15)
    waitUntil(t0 + 1)
    rs.setOutput(sides.up, 15)
    waitUntil(t0 + 3)
    rs.setOutput(sides.left, 0)
    rs.setOutput(sides.back, 0)
    rs.setOutput(sides.up, 0)
  else
    waitUntil(t0 + 1)
  end
end
