local robot = component.proxy(component.list("robot")())
local inv = component.proxy(component.list("inventory_controller")())

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

local retrying = function(f, ...)
  while true do
    local result = {f(...)}
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

while true do
  local t0 = computer.uptime()

  if getEnergy() < 0.2 then
    scopedWalk("FFFFFF", function()
      while getEnergy() < 0.8 do sleep(0.2) end
    end)
  end

  for i = 1, 16 do robot.suck(sides.down) end

  local slotsBook = {}
  local slotsEnchanted = {}
  local slotsDamaged = {}
  local slotsPam = {}
  local slotsOther = {}

  for i = 1, 16 do
    local item = inv.getStackInInternalSlot(i)
    if item then
      if item.name == "minecraft:enchanted_book" then
        table.insert(slotsBook, i)
      elseif item.enchantments then
        table.insert(slotsEnchanted, i)
      elseif item.maxDamage > 0 and item.damage > 0 then
        table.insert(slotsDamaged, i)
      elseif string.match(item.name, "^harvestcraft:")
          and item.name ~= "harvestcraft:octopusrawitem"
          and item.name ~= "harvestcraft:seaweeditem"
          and item.name ~= "harvestcraft:eelrawitem" then
        table.insert(slotsPam, i)
      else
        table.insert(slotsOther, i)
      end
    end
  end

  local points = {}
  local addPoint = function(location, slots)
    if #slots > 0 then
      table.insert(points, {location = location, action = function()
        for _, i in pairs(slots) do
          robot.select(i)
          robot.drop(sides.down)
        end
      end})
    end
  end

  addPoint(1, slotsBook)
  addPoint(2, slotsEnchanted)
  addPoint(3, slotsDamaged)
  addPoint(4, slotsPam)
  addPoint(5, slotsOther)

  local currentLocation = 0

  while #points > 0 do
    local point = points[1]
    if currentLocation < point.location then
      walk("F")
      currentLocation = currentLocation + 1
    else
      point.action()
      table.remove(points, 1)
    end
  end

  while currentLocation > 0 do
    walk("B")
    currentLocation = currentLocation - 1
  end
end
