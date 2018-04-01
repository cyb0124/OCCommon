local resolve = function(short)
  for addr in component.list() do
    if string.lower(string.sub(addr, 1, #short)) == string.lower(short) then
      return addr
    end
  end
end

local waitUntil = function(t1)
  while true do
    local td = t1 - computer.uptime()
    if td <= 0 then break end
    computer.pullSignal(td)
  end
end

local sleep = function(duration)
  waitUntil(computer.uptime() + duration)
end

local getEnergy = function()
  return computer.energy() / computer.maxEnergy()
end

local retrying = function(...)
  while true do
    local result = {({...})[1](select(2, ...))}
    if result[1] then
      return table.unpack(result)
    end
    computer.beep(880)
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
