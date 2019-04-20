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

local robot = component.proxy(component.list("robot")())

local walk = function(actions)
  for i = 1, #actions do
    local action = string.lower(string.sub(actions, i, i))
    if action == "f" then
      retrying(robot.move, sides.front)
    elseif action == "b" then
      retrying(robot.move, sides.back)
    elseif action == "u" then
      retrying(robot.move, sides.up)
    elseif action == "d" then
      retrying(robot.move, sides.down)
    elseif action == "l" then
      retrying(robot.turn, false)
    elseif action == "r" then
      retrying(robot.turn, true)
    else
      error("invalid walk action")
    end
  end
end

local inverseWalkActions = function(input)
  local output = ""
  input = string.reverse(input)
  for i = 1, #input do
    local now = string.lower(string.sub(input, i, i))
    if now == "f" then
      output = output .. "b"
    elseif now == "b" then
      output = output .. "f"
    elseif now == "u" then
      output = output .. "d"
    elseif now == "d" then
      output = output .. "u"
    elseif now == "l" then
      output = output .. "r"
    elseif now == "r" then
      output = output .. "l"
    else
      output = output .. now
    end
  end
  return output
end

local scopedWalk = function(actions, f)
  walk(actions)
  f()
  walk(inverseWalkActions(actions))
end

local inv = component.proxy(component.list("inventory_controller")())

local timeCheckedHarvester = -1/0

while true do
  if getEnergy() < 0.2 then
    scopedWalk("RFD", function()
      while getEnergy() < 0.8 do sleep(0.2) end
    end)
  end

  if computer.uptime() - timeCheckedHarvester > 60 then
    scopedWalk("RFL", function()
      for i = 3, 16 do
        robot.select(i)
        if not inv.getStackInInternalSlot(i) then
          robot.suck(sides.front)
        end
      end
    end)
    local slots = {}
    for i = 3, 16 do
      if inv.getStackInInternalSlot(i) then
        table.insert(slots, i)
      end
    end
    local slotsRemaining = #slots
    for _, i in pairs(slots) do
      robot.select(i)
      inv.dropIntoSlot(sides.down, 3)
      if not inv.getStackInInternalSlot(i) then
        slotsRemaining = slotsRemaining - 1
      end
    end
    if slotsRemaining == 0 then
      timeCheckedHarvester = computer.uptime()
    end
  end

  if robot.detect(sides.front) then
    robot.select(1)
    local fertilizer = inv.getStackInInternalSlot(1)
    if not fertilizer then
      inv.suckFromSlot(sides.bottom, 2)
    elseif fertilizer.name ~= "actuallyadditions:item_fertilizer" then
      inv.dropIntoSlot(sides.bottom, 3)
    else
      robot.place(sides.front)
    end
  else
    robot.select(2)
    local sapling = inv.getStackInInternalSlot(2)
    if not sapling then
      inv.suckFromSlot(sides.bottom, 1)
    else
      robot.place(sides.front)
    end
  end
end
