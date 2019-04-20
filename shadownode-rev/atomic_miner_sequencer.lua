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
-- Placer: mechanical user, pulse, place block, right click, 1st slot only
-- Breaker: mechanical user, rs on, use item on block, left click, 1st slot only, 4 speed upgrades
--   if pick is fast enough: no speed upgrade
local sidePlacerA = sides.east
local sidePlacerR = sides.back
local sideAtomicR = sides.right
local sideBreakeR = sides.up

while true do
  local t0 = computer.uptime()
  if inv.getSlotStackSize(sidePlacerA, 1) > 0 then
    rs.setOutput(sidePlacerR, 15)
    waitUntil(t0 + 0.1)
    rs.setOutput(sideAtomicR, 15)
    waitUntil(t0 + 1)
    rs.setOutput(sideBreakeR, 15)
    waitUntil(t0 + 1.1) -- can be reduced if pick is fast
    rs.setOutput(sidePlacerR, 0)
    rs.setOutput(sideAtomicR, 0)
    rs.setOutput(sideBreakeR, 0)
  else
    waitUntil(t0 + 1)
  end
end
